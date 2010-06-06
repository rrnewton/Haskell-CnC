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
#define MODNAME Intel.Cnc7
#endif
#define CNC_SCHEDULER 7
#define STEPLIFT  S.lift$
#define GRAPHLIFT S.lift$
#define SUPPRESS_runGraph
#define DEFINED_free_items
#include "Cnc.Header.hs"

------------------------------------------------------------
-- Version 7: Now with healing -- bring back worker threads that died
-- prematurely.  Specifically, in this version we use a global work
-- queue but we heal workers when we put into an empty queue.


-- DUPLICATING shared_5_6.hs here because I need to CHANGE THE TYPE:

type TagCol a   = (IORef (Set.Set a), IORef [Step a])
type ItemCol a b = MutableMap a b

-- Here the hidden state keeps track of a pointer to the work-sharing
-- stack used for this graph.
type StepCode a = (S.StateT (HiddenState5) IO a)

-- In this version we need to thread the state through the graph code as well:
type GraphCode a = StepCode a

-- Here the hidden state keeps four things:
--   (1) the stack used for this graph
--   (2) the number of workers for this graph
--   (3) the "make worker" function to spawn new threads
--   (4) the set of "mortal threads"
--   (5) the set of persistent, numbered workers that are currently dead
-- ***** CHANGE IN DUPLICATED CODE:
newtype HiddenState5 = HiddenState5 (HotVar [StepCode ()], 
                                     HotVar Int, 
				     Int -> IO (), 
				     HotVar (Set.Set ThreadId), 
				     HotVar (Set.Set Int))
  deriving Show

instance Show (Int -> IO ()) where
  show _ = "<int to IO unit function>"

atomicIncr :: Num n => HotVar n -> IO ()
atomicDecr :: Num n => HotVar n -> IO ()
atomicIncr x = modifyHotVar_ x (+ 1)
atomicDecr x = modifyHotVar_ x (\n -> n-1)


-- A simple stack interface:
----------------------------------------
push   :: HotVar [a] -> a -> IO ()
tryPop :: HotVar [a] -> IO (Maybe a)
push stack val = modifyHotVar_ stack (val:)
tryPop stack   = modifyHotVar stack tryfirst
  where 
    tryfirst []    = ([], Nothing)
    tryfirst (a:b) = (b,  Just a)

stepcode_push :: HotVar [a] -> a -> StepCode ()
stepcode_push stack val = 
  -- If push onto an empty stack, wake up deadset.  
  do old <- STEPLIFT modifyHotVar stack (\ls -> (val:ls, ls))
     if null old
      -- Wake up the dead:
      then do (HiddenState5 (_, numworkers, makeworker, _, deadset)) <- S.get
	      dead <- STEPLIFT readHotVar deadset
	      let len = Set.size dead
              STEPLIFT putStrLn$ "        **********    Waking the dead: " ++ show dead 
	      if len > 0 
	       then do STEPLIFT modifyHotVar_ numworkers (+ len)
		       STEPLIFT forM_ (Set.toList dead) (forkIO . makeworker)
	       else return ()
      else return () 

----------------------------------------

issueReplacement = 
  do (HiddenState5 (stack, numworkers, makeworker, _, _)) <- S.get
-- ***** CHANGE IN DUPLICATED CODE:
     STEPLIFT atomicIncr numworkers
     -- If this were CPS then we would just give our
     -- continuation to the forked thread.  Alas, no.
-- ***** CHANGE IN DUPLICATED CODE:
     STEPLIFT forkIO (makeworker (-1))

-- FIXME: [2010.05.05] I believe this has a problem.
-- tryTakeMVar can fail spuriously if there's a collision with another
-- thread reading the mvar.  This is a sense in which mvars CANNOT
-- mimick IVars (at least ivars with the ability to test for presence
-- -- a monotonic test!)

-- This should only be a performance bug (forks an extra task for no
-- good reason).  When the code below falls back to readMVar that
-- should succeed.

-- Grab an mvar, but bring in reinforcements if we need to go down:
grabWithBackup hook mvar =
    do hopeful <- STEPLIFT tryTakeMVar mvar
       case hopeful of 
         Just v  -> do STEPLIFT putMVar mvar v -- put it back where we found it
		       return v
	 -- Otherwise, no data.  If we block our own thread, we need to issue a replacement.
         Nothing -> do issueReplacement
		       
		       STEPLIFT hook -- Any IO action can go here...
#ifdef DEBUG_HASKELL_CNC
		       STEPLIFT putStrLn $ " >>> Blocked on "++ show tag ++"||| "
#endif
		       STEPLIFT readMVar mvar


ver5_6_core_get hook (col) tag = 
    do (HiddenState5 (stack, _, makeworker, _, _)) <- S.get
       mvar    <- STEPLIFT assureMvar col tag 
       grabWithBackup hook mvar


ver5_6_core_finalize :: Chan Int -> StepCode b -> (Int -> StepCode ()) -> Bool -> GraphCode b
ver5_6_core_finalize joiner finalAction worker shouldWait = 
    do (HiddenState5 (stack, numworkers, _, mortal, deadset)) <- S.get
       let mkwrkr id = do S.runStateT (worker id) state2; return ()
           state2 = HiddenState5 (stack, numworkers, mkwrkr, mortal, deadset)
       -- Write it back for the "finalAction" below:
       S.put state2

       GRAPHLIFT modifyHotVar_ numworkers (+ numCapabilities)
       -- Fork one worker per thread:
#ifdef DEBUG_HASKELL_CNC
       GRAPHLIFT putStrLn$ "Forking "++ show numCapabilities ++" threads"
#endif
       GRAPHLIFT forM_  [0..numCapabilities-1] (\n -> forkIO (mkwrkr n))
       --S.lift$ mapM (\n -> forkOnIO n mkwrkr) [0..numCapabilities-1]

       -- This waits for quiescense:
       let waitloop = do num <- readHotVar numworkers
	                 if num == 0
			  then return () 
			  else do 
#ifdef DEBUG_HASKELL_CNC
			          putStrLn ("=== Waiting on workers: "++ show num ++" left")
#endif
				  id <- readChan joiner
-- ***** CHANGE IN DUPLICATED CODE:
				  modifyHotVar_ deadset (Set.insert id)

				  atomicDecr numworkers
				  waitloop
       if shouldWait then S.lift$ waitloop else return ()
       finalAction


putt = proto_putt
	(\ steps tag -> 
	   do (HiddenState5 (stack, _, _, _, _)) <- S.get
              foldM (\ () step -> stepcode_push stack (step tag))
                       () steps)

runGraph x = unsafePerformIO (runState x)
runState x =
    do hv  <- newHotVar []
       hv2 <- newHotVar 0
       hv3 <- newHotVar Set.empty
       hv4 <- newHotVar Set.empty
       let msg = "Intel.Cnc"++ show CNC_SCHEDULER ++" internal error: makeworker thunk used before initalized"
       (a,_) <- S.runStateT x (HiddenState5 (hv,hv2, error msg, hv3, hv4))
       return a

------------------------------------------------------------
-- Experimental:  Free floating items:

type Item = MVar
newItem  = STEPLIFT newEmptyMVar
readItem = grabWithBackup (return ())
putItem mv x = 
  do b <- STEPLIFT tryPutMVar mv x
     if b then return ()
	  else error "Violation of single assignment rule; second put on Item!"


----------------------------------------------------------------------------------------------------
-- END CODE DUPLICATION
----------------------------------------------------------------------------------------------------


get col tag = ver5_6_core_get (return ()) col tag

-- At finalize time we set up the workers and run them.
finalize userFinalAction = 
    do (HiddenState5 (stack, _, _, _, _)) <- S.get
       joiner <- GRAPHLIFT newChan 
       let worker id = 
	       do x <- STEPLIFT tryPop stack
		  case x of 
		    Nothing -> STEPLIFT writeChan joiner id
		    Just action -> do action
				      worker id

       ver5_6_core_finalize joiner userFinalAction worker True

------------------------------------------------------------

quiescence_support = True
