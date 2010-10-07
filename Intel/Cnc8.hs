{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , TypeFamilies 
  , UndecidableInstances
  , OverlappingInstances
  , DeriveDataTypeable
  , MultiParamTypeClasses
  #-}
-- State monad transformer is needed for both step & graph:
#ifndef MODNAME
#define MODNAME Intel.Cnc8
#endif
#define CNC_SCHEDULER 8
#define STEPLIFT  S.lift$
#define GRAPHLIFT id$
#define SUPPRESS_put
#define SUPPRESS_newItemCol
#define SUPPRESS_initialize
#define SUPPRESS_itemsToList
#define SUPPRESS_graphInStep
#include "Cnc.Header.hs"

--------------------------------------------------------------------
-- Version 8: This also uses the GHC scheduler directly (like 3) but
-- it uses sparks rather than forkIO.
--------------------------------------------------------------------

-- Note, the spark pool is lossy and can't be counted on.  (It will
-- happily discard sparks when it overflows.  In the future it may not
-- even serve as GC roots.)

-- Therefore this version does a litle book-keeping it both sparks a
-- step, and adds the step to a list so that after each step is
-- completed it can "sync" on its children.  This this scheduler
-- behaves very much like a Cilk version of CnC.

-- Like Concurrent Collectins for C++, this version uses exceptions to
-- escape a step's execution upon a failed get.  An alternative is to
-- use the ContT monad transformer.

-- TODO: The Cilk-like functionality could be factored into its own
-- reusable module.

type TagCol a   = (IORef (Set.Set a), IORef [Step a])
--type ItemCol a b = MutableMap a b

-- Here the hidden state keeps track of 
--newtype StepCode a = StepCode (S.StateT HiddenState8 IO a)
type StepCode a = (S.StateT (HiddenState8) IO a)
type GraphCode = IO

-- The hidden stat stores two things:
--   (1) "Self": the current action, if needed for requeueing.
--   (2) A list of child tasks/thunks that were spawned in parallel.
newtype HiddenState8 = HiddenState8 (StepCode (), [()])

-- In this version we don't use MVars because gets don't block:
newtype ItemCol a b = ItemCol (IORef (Map.Map a ((Maybe b), WaitingSteps)))
type WaitingSteps = [StepCode ()]

data EscapeStep = EscapeStep  deriving (Show, Typeable)
instance Exception EscapeStep
--instance GHC.Exception.Exception EscapeStep

--------------------------------------------------------------------------------
-- Misc utility functions used by the version 8 API functions:
--------------------------------------------------------------------------------
liftHidden fn = (\ (HiddenState8 (self,ls)) -> HiddenState8 (self, fn ls))
atomicModifyIORef_ ref fn = atomicModifyIORef ref (\x -> (fn x, ()))

stepStats :: StepCode ()
stepStats = 
  do 
     tid <- S.lift myThreadId
     HiddenState8 (_, ls) <- S.get 
     S.lift$ putStrLn (">>>   Step state: list len: "++ show (length ls))

-- This is the high level interface for running several steps in
-- parallel and then blocking on the result.
launch_steps :: [StepCode ()] -> StepCode ()
launch_steps mls = 
--    foldM (\ () m -> spawn (do try_stepcode m m; return ()))
    foldM (\ () m -> forkStep m)
          () mls

-- New, more dynamic API:
forkStep s = 
  spawn (do try_stepcode s s; return ())

-- This consumes the state thats threaded through step code by capping
-- the end of the step with a sync.  It needs a retry action to tuck
-- into the state so that the step can store it if it needs to escape
-- with an exception.
try_stepcode :: StepCode () -> StepCode a -> IO (Maybe a)
try_stepcode retry m = wrapped
 where
    -- If data is not ready yet, fizzle:
    wrapped = do x <- try sync_action		 
		 case x of Left EscapeStep -> return Nothing 
			   Right v         -> return (Just v)
    -- This is a Cilk-like sync.  Run the action to accumulate the list of
    -- spawned children-actions.  Here we serially execute those children
    -- if they haven't been done in parallel.
    sync_action = 
      do -- First RUN the step code:
         (v, HiddenState8 (_, ls)) <- S.runStateT m (HiddenState8 (retry,[]))
         tid <- myThreadId
         -- Second, sync all child computations that the step created.
         -- We may be racing to fill these in with other threads.
#ifdef DEBUG_HASKELL_CNC
         putStrLn (">>> "++show tid ++":  Syncing children")
#endif
         --return (foldr pseq v ls)
         --return v
         foldr pseq (return v) ls

-- Release an IO action for parallel execution, and squirrel it away
-- so we can sync as well.
spawn :: IO () -> StepCode ()
spawn ioaction = 
    do -- Add the new action to the list of actions for this step.
       --let thunk = unsafeDupablePerformIO ioaction        
       let thunk = unsafePerformIO ioaction        

#ifdef DEBUG_HASKELL_CNC
       --let wrapped = unsafeDupablePerformIO$ 
       let wrapped = unsafePerformIO$ 
		     do { tid <- myThreadId; putStrLn ("\n>>> "++show tid++": STOLE WORK!\n"); pseq thunk (return ()) }
       let parthunk = wrapped
#else 
       let parthunk = thunk
#endif

       --S.modify $ liftHidden (parthunk:)
       S.modify $ liftHidden (thunk:)
       id <- S.lift$ myThreadId 

#ifdef DEBUG_HASKELL_CNC
       S.lift$ putStrLn $ ">>> "++ show id ++ ": Spawning..."
       stepStats
#endif

       --return (parthunk `par` ())
       parthunk `par` (do 
#ifdef DEBUG_HASKELL_CNC
		          mid <- S.lift$ myThreadId
		          S.lift$putStrLn (">>>  "++show mid++" (spawned parallel)") 
#endif
		          return ())



--------------------------------------------------------------------------------
-- The core of the version 8 implementation:
--------------------------------------------------------------------------------

newItemCol = do ref <- newIORef Map.empty
		return (ItemCol ref)

putt = proto_putt
	         (\ steps tag -> 
		  -- Spark each downstream step, attempting to do it in parallel before a 
		  -- subsequent sync (at the end of the containing step).		  
                  launch_steps (Prelude.map (\step -> step tag) steps))



get (ItemCol icol) tag = 
    do map <- S.lift$ readIORef icol       
       case Map.lookup tag map of 
	 Nothing                 -> addquit [] 
	 Just (Nothing, waiting) -> addquit waiting
	 Just (Just v,  [])      -> return v
	 Just (Just v, a:b)      -> error "CnC: internal invariant violated"
   where 
       addquit ls = 
	      do (HiddenState8 (retry ,_)) <- S.get
	         S.lift$ atomicModifyIORef_ icol (Map.insert tag (Nothing, retry:ls))
	         -- After adding ourself to the wait list, jump out of this step:
		 throw EscapeStep

initfin :: String -> StepCode a -> GraphCode a
initfin str m = do let err = error str
	           x <- try_stepcode err m
	           case x of Nothing -> err
		  	     Just v  -> return v

initialize = initfin "Get failed within initialize action!"
finalize   = initfin "Get failed within finalize action!"


-- Put must replay any steps that are waiting.
put (ItemCol icol) tag (!item) = 
    do waiting <- S.lift$ atomicModifyIORef icol mod 
       launch_steps waiting
       return ()
   where 
       mod map = 
	 let new = (Just item, [])
	     f key _ (Nothing, _) = new
#ifdef REPEAT_PUT_ALLOWED
	     f key _ old@(Just v, ls) = old
#else
	     f key _ (Just v, _)  = error ("Single assignment violated at tag: "++ show tag)
#endif
	     (old, map') = Map.insertLookupWithKey f tag new map
	 in case old of
	      Nothing                 -> (map', [])
	      Just (Nothing, waiting) -> (map', waiting)
#ifdef REPEAT_PUT_ALLOWED
	      Just (Just _, waiting)  -> (map , waiting)
#else
	      Just (Just _, _)        ->  error ("Single assignment violated at tag: "++ show tag)
#endif

itemsToList (ItemCol icol) = 
  do if not quiescence_support 
       then error "need to use a scheduler with quiescence support for itemsToList" 
       else return ()
     map <- S.lift$ readIORef icol 
     return   $ Prelude.map (\ (key, (Just v, _)) -> (key,v)) 
 	      $ Prelude.filter fil 
 	      $ (Map.toList map)
 where 
     fil (key, (Nothing, _)) = False
     fil _                   = True


-- To execute graph code inside a step we just need to lift it into the monad transformer:
graphInStep = S.lift

quiescence_support=True ;

--------------------------------------------------------------------------------
-- Version 9

-- TODO??? Get COULD explicitly capture the continuation to avoid replay from the beginning.

-- Combining continuation monad with IO:
-- import Control.Monad.Cont
-- import System.IO

-- main = do
--   hSetBuffering stdout NoBuffering
--   runContT (callCC askString) reportResult

-- askString :: (String -> ContT () IO String) -> ContT () IO String
-- askString next = do
--   liftIO $ putStrLn "Please enter a string"
--   s <- liftIO $ getLine
--   next s

-- reportResult :: String -> IO ()
-- reportResult s = do
--   putStrLn ("You entered: " ++ s)
