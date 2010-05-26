{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , DeriveDataTypeable
  , MultiParamTypeClasses
  #-}
--  Note: PatternSignatures was deprecated after 6.8.

{-
 - Intel Concurrent Collections for Haskell
 - Copyright (c) 2010, Intel Corporation.
 -
 - This program is free software; you can redistribute it and/or modify it
 - under the terms and conditions of the GNU Lesser General Public License,
 - version 2.1, as published by the Free Software Foundation.
 -
 - This program is distributed in the hope it will be useful, but WITHOUT
 - ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 - FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 - more details.
 -
 - You should have received a copy of the GNU Lesser General Public License along with
 - this program; if not, write to the Free Software Foundation, Inc., 
 - 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
 -
 -}


-- #define DEBUG_HASKELL_CNC

{- 

   This is an implementation of CnC based on the IO monad.  The
   exposed interface is the same as the pure implementation, and CnC
   computations remain pure.

  This version formulates steps as side-effecting functions on tables
  of MVars (item collections).

  If we had concurrent hashtables, that would be one option.  Instead
  we need to use Data.Map.

 -}

-- This must be defined because the runtime may allow a low
-- probability of duplicating stolen work.
-- #define REPEAT_PUT_ALLOWED

{-|
  This module implements the Intel Concurrent Collections (CnC) programming model.

  Note that the module @Intel.CncPure@ is an alternative implementation that exposes the same interface as this module.
 -}
#ifndef INCLUDEMETHOD
module Intel.Cnc (

                  -- | The @GraphCode@ and @StepCode@ monads represent
                  -- | computations for constructing CnC graphs and
                  -- | which execute inside CnC graphs, respectively.
		  Step, TagCol, ItemCol,
		  StepCode(..), GraphCode,
		  newItemCol, newTagCol, prescribe, 
		  putt, put, get,
		  initialize, finalize,

                  runGraph, 
		  stepPutStr, cncPutStr, cncVariant,

                  Hashable,

                  tests, 
		 )
where
#else
#warning "Loading imperative, IO-based CnC implementation."
#endif

import Data.Set as Set
import Data.HashTable as HT
import Data.Map as Map
import Data.Int
import Data.IORef
import Data.Word
import Control.Monad
import Control.Monad.Trans
import qualified  Control.Monad.State.Strict as S 
--import qualified  Control.Monad.State.Lazy as S 
import Control.Concurrent.MVar
import Control.Concurrent.Chan
--import System.Environment
import System.IO.Unsafe
import GHC.IO

import Control.Concurrent
import GHC.Conc
import GHC.Prim
import GHC.Exts 

import Test.HUnit



import Intel.CncUtil as GM hiding (tests)

import Data.Typeable
--import Control.Exception
import Control.Exception.Extensible

------------------------------------------------------------
-- Configuration Toggles:

#ifdef MEMOIZE
#warning "Memoization enabled"
memoize = True
#else
memoize = False
#endif


------------------------------------------------------------
-- Abstract over the shared mutable data structure used for item
-- collections:

#ifdef HASHTABLE_TEST
type MutableMap a b = HashTable a (MVar b)
newMutableMap :: (Eq tag, Hashable tag) => IO (MutableMap tag b)
newMutableMap = HT.new (==) hash
assureMvar col tag = 
  do mayb <- HT.lookup col tag
     case mayb of 
         Nothing -> do mvar <- newEmptyMVar
		       HT.insert col tag mvar
		       return mvar
	 Just mvar -> return mvar
mmToList = HT.toList
#define ITEMPREREQS (Eq tag, Ord tag, Hashable tag, Show tag)
#warning "Enabling HashTable item collections.  These are not truly thread safe (yet)."
#else

#define USE_GMAP
#ifdef USE_GMAP

-- Trying to use GMaps:
type MutableMap a b = IORef (GMap a (MVar b))
newMutableMap :: (GMapKey tag) => IO (MutableMap tag b)
newMutableMap = newIORef GM.empty
assureMvar col tag = 
  do map <- readIORef col
     case GM.lookup tag map of 
         Nothing -> do mvar <- newEmptyMVar
		       atomicModifyIORef col 
			  (\mp -> 
			   let altered = GM.alter 
			                  (\mv -> 
					    case mv of
					     Nothing -> Just mvar
					     Just mv -> Just mv)
			                  tag mp 
			   -- Might be able to optimize this somehow...
			   in (altered, (GM.!) altered tag))
	 Just mvar -> return mvar
mmToList col = 
    do map <- readIORef col 
       return (GM.toList map)
-- #define ITEMPREREQS (Ord tag, Eq tag, GMapKey tag, Show tag)
#define ITEMPREREQS (GMapKey tag)

#else
-- A Data.Map based version:
-- Can probably get rid of this once we build a little confidence with GMap:
type MutableMap a b = IORef (Map a (MVar b))
newMutableMap :: (Ord tag) => IO (MutableMap tag b)
newMutableMap = newIORef Map.empty
assureMvar col tag = 
  do map <- readIORef col
     case Map.lookup tag map of 
         Nothing -> do mvar <- newEmptyMVar
		       atomicModifyIORef col 
			  (\mp -> 
			   let altered = Map.alter 
			                  (\mv -> 
					    case mv of
					     Nothing -> Just mvar
					     Just mv -> Just mv)
			                  tag mp 
			   -- Might be able to optimize this somehow...
			   in (altered, (Map.!) altered tag))
	 Just mvar -> return mvar
mmToList col = 
    do map <- readIORef col 
       return (Map.toList map)
#define ITEMPREREQS (Eq tag, Ord tag, Show tag)

#endif
#endif

------------------------------------------------------------
-- Type definitions:

-- A tag collection consists of a buffer of tags together with a list
-- of prescribed steps.
--
-- IORef's are not updated atomically.  Prescribing is not intended to
-- be done concurrently.  (Even though the graph monad is actually the
-- IO monad that is not indended to be exposed to the programmer.)
type TagCol0  a   = (IORef (Set a), IORef [Step0 a])
type ItemCol0 a b = MutableMap a b

type Step0 a = a -> StepCode0 ()
type StepCode0 = IO 
type GraphCode = IO

------------------------------------------------------------
-- (Optional) type signatures for operations:

-- Basically just a table for memoization:

-- |Attach a computation step to a feed of control tags.  This adds a new node in the computation graph.
prescribe   :: TagCol tag -> Step tag -> GraphCode ()

-- |Put-Tag.  Push a control tag out into the computation graph.
putt :: Ord tag         => TagCol  tag     -> tag         -> StepCode ()
-- |Put an item.  Subsequently, any steps waiting on the item may subsequently execute.
put  :: ITEMPREREQS     => ItemCol tag val -> tag -> val  -> StepCode ()
-- |Get an item.  Synchronous read-data operation.
get  :: ITEMPREREQS     => ItemCol tag val -> tag         -> StepCode val

initialize :: StepCode a -> GraphCode a
finalize   :: StepCode a -> GraphCode a

-- |Construct a new tag collection.
newTagCol  :: GraphCode (TagCol tag)
-- |Construct a new item collection.
newItemCol :: ITEMPREREQS => GraphCode (ItemCol tag val)


class Hashable a where
    hash :: a -> Int32

instance Hashable Bool where
    hash True  = 1
    hash False = 0

instance Hashable Int where
    hash = hashInt
instance Hashable Char where
    hash = hashInt . fromEnum 
instance Hashable Word16 where
    hash = hashInt . fromIntegral
--instance Hashable String where -- Needs -XTypeSynonymInstances 
instance Hashable [Char] where
    hash = hashString
instance (Hashable a, Hashable b) => Hashable (a,b) where 
    hash (a,b) = hash a + hash b

instance Hashable a => Hashable [a] where
    hash []    = 0 
    hash (h:t) = hash h + hash t

-- Needs -fallow-undecidable-instances:
-- instance Integral t => Hashable t where
--     hash n = hashInt (fromInteger (toInteger n))
-- instance Enum a => Hashable a where
--     hash = hashInt . fromEnum 


--------------------------------------------------------------------------------
--                             Implementation                                 --
--------------------------------------------------------------------------------

-- These 'new' functions need an argument if we don't want to run in
-- to the monomorphism restriction (-fno-monomorphism-restriction)
newItemCol0 :: ITEMPREREQS => GraphCode (ItemCol0 tag b)
newItemCol0 = newMutableMap

sharedNTC = do ref1 <- newIORef Set.empty
	       ref2 <- newIORef []
	       return (ref1, ref2)

newTagCol0  :: GraphCode (TagCol0 tag)
newTagCol0 = sharedNTC


-- Putting items: If it's not there we add the mvar ourselves.
-- 
-- [2010.02.15] Made this strict in the item.  Otherwise we could
-- unintentionally delay work until the after the (parallel) CnC
-- evaluation and do it in serial!
put0  :: ITEMPREREQS => ItemCol0 tag b -> tag -> b  -> StepCode0 ()
put0 col tag (!item) = 
    do mvar <- assureMvar col tag 
       bool <- tryPutMVar mvar item
#ifdef REPEAT_PUT_ALLOWED
       return ()
#else
       if not bool then error ("Already an item with tag " ++ show tag) else return ()
#endif


-- A tag collection stores the list of subscribed step collections.
-- To "prescribe", simply add it to the list:
prescribe (_set,_steps) step = 
    do steps <- readIORef _steps
       writeIORef _steps (step:steps)


-- This encapsulates the book-keeping necessary for a put-tag (putt).
-- It is common to all the scheduler variants below.
-- 
-- FIXME: Consider a trampoline.  Some schedulers may stack leak.
proto_putt :: Ord a =>  ([Step0 a] -> a -> StepCode0 b) -> TagCol0 a -> a -> StepCode0 b
proto_putt action tc@(_set,_steps) tag = 
    do set   <- readIORef _set
       steps <- readIORef _steps
       if memoize 
        then if Set.member tag set
	     then return ()
	     else writeIORef _set (Set.insert tag set)
        else return ()
       action steps tag

itemsToList0 :: ITEMPREREQS => ItemCol0 tag b -> StepCode0 [(tag,b)]
itemsToList0 ht = 
 do if not quiescence_support 
       then error "need to use a scheduler with quiescence support for itemsToList" 
       else return ()
    ls <- (mmToList ht)
    foldM (\ acc (key,mvar) -> 
	   do --putStrLn "Try take mvar..."
	      val <- readMVar mvar
	      --putStrLn "  Took!"
	      return $ (key,val) : acc)
	  [] ls

-- Embed StepCode in the graph construction program:
initialize0 x = x

-- Bring us from the graph monad back to the IO monad:
runGraph :: GraphCode a -> a
runGraph x = unsafePerformIO x

--------------------------------------------------------------------------------


-- The remaining API functions (putt & finalize) vary on a per-scheduler basis:

-- The core routines are duplicated for the different scheduler versions.
putt3 :: Ord a  => TagCol0 a -> a -> StepCode0 ()
putt5 :: Ord a  => TagCol0 a -> a -> StepCode0 ()
putt6 :: Ord a  => TagCol0 a -> a -> StepCode0 ()

finalize3 :: StepCode0 a -> GraphCode a
finalize5 :: StepCode0 a -> GraphCode a
finalize6 :: StepCode0 a -> GraphCode a

------------------------------------------------------------
-- TODO  TODO TODO TODO TODO TODO TODO TODO TODO TODO  -- 
------------------------------------------------------------

-- [2010.02.11] I need to look at unecessary control-flow
-- back-and-forth.  Currently, because of this "depth-first"
-- optimization, I will call down to a child and then probably return
-- (unless GHC manages to turn it into a tail call, maybe it does).  I
-- could help out GHC by just queueing a list of spawned downstream
-- tasks as I go through a step.  When the step is done, the list can
-- be spawned.  At that point if there is only one downstream it can
-- definitely be a tail call.


------------------------------------------------------------
--Version 1: Serial
-- (This version has been disabled/removed.)

-- Version 2: 
-- (This version has been disabled/removed.)

-- Here we do the tail call optimization for the common case of a single prescribed step.


------------------------------------------------------------
-- Version 3: Here we try for forked parallelism:

putt3 = proto_putt (\ steps tag -> 
		    case steps of 
	             --[] -> error "putt on tag collection with no prescribed steps"
	             steps -> 
		      foldM (\ () step -> do forkIO (step tag); return ())
   	  	       () steps
		   )

-- We needn't fork a new thread if it's "tail call"
tail_putt3 :: Ord a  => TagCol0 a -> a -> StepCode0 ()
tail_putt3 = proto_putt$ \ steps tag -> 
	       case steps of
	          []       -> error "putt on tag collection with no prescribed steps"
		  fst:rest -> 
		     do forM_ rest $ \step -> forkIO (step tag)
			fst tag


get3 col tag = do mvar <- assureMvar col tag 
		  readMVar mvar

-- The above 'putt's use a trivial finalizer:
-- WARNING -- this will not wait for workers to finish during finalization.
-- Therefore, this only works with programs that 'get' their output.
-- E.g. it does not support quiescent completion.
finalize3 x = x 
-- TODO: At least kill off the existing threads here?


------------------------------------------------------------
-- Version 4: forkIO with extra book-keeping to enable queiscence.
-- Namely,  we use an atomic counter to track the number of forks.

-- putt4 = proto_putt (\ steps tag -> 
-- 		    case steps of 
-- 	            -- Uncomment the next line to enable the depth-first
-- 	            -- optimization even with the parallel scheduler!!
-- 		     --[step] -> step tag
-- 	             steps -> 
-- 		      foldM (\ () step -> do forkIO (step tag); return ())
--    	  	       () steps)

-- get4 col tag = do mvar <- assureMvar col tag 
-- 		  readMVar mvar

-- -- The above 'putt's use a trivial finalizer:
-- -- WARNING -- this will not wait for workers to finish during finalization.
-- -- Therefore, this only works with programs that 'get' their output.
-- -- E.g. it does not support quiescent completion.
-- finalize3 x = x 
-- -- TODO: At least kill off the existing threads here?



------------------------------------------------------------
-- Version 5a: A global work queue.
-- (This version has been disabled/removed.)
-- This version uses a global work-queue.
-- Here laziness comes in handy, we queue the thunks.

------------------------------------------------------------
-- Version 5: Thread spammer -- fork a permanent worker every time we block.

-- TODO: we should do a better job here by using a monad transformer on top of IO:
-- But if we must keep the same CnC interface... This is expedient:


{- 
-- This will be one hot MVar:
global_stack :: MVar ([a])
global_stack = unsafePerformIO (newMVar [])

-- A simple stack interface:
----------------------------------------
push stack val = modifyMVar_ stack $ return . (val:)
tryPop stack = modifyMVar stack tryfirst
  where 
    tryfirst []    = return ([], Nothing)
    tryfirst (a:b) = return (b, Just a)
----------------------------------------
-}

-- This will be one hot IORef:
global_stack :: IORef [a]
global_stack = unsafePerformIO (newIORef [])

-- A simple stack interface:
----------------------------------------
push   :: IORef [a] -> a -> IO ()
tryPop :: IORef [a] -> IO (Maybe a)
push stack val = atomicModifyIORef stack (\ls -> (val:ls, ()))
tryPop stack   = atomicModifyIORef stack tryfirst
  where 
    tryfirst []    = ([], Nothing)
    tryfirst (a:b) = (b,  Just a)
----------------------------------------

putt5 = proto_putt (\ steps tag -> 
                    foldM (\ () step -> push global_stack (step tag))
                      () steps)

get5 col tag = ver5_6_core_get (return ()) col tag

-- At finalize time we set up the workers and run them.
finalize5 finalAction = 
    do joiner <- newChan 
       let worker = 
	       do x <- tryPop global_stack
		  case x of 
		    Nothing -> writeChan joiner ()
		    Just action -> do action; worker
       ver5_6_core_finalize joiner finalAction worker 


-- A couple pieces that are common to version 5 and 6
------------------------------------------------------------
ver5_6_core_finalize joiner finalAction worker = 
    do writeIORef global_makeworker worker
       atomicModifyIORef global_numworkers (\n -> (n + numCapabilities, ()))
       -- Fork one worker per thread:
#ifdef DEBUG_HASKELL_CNC
       putStrLn$ "Forking "++ show numCapabilities ++" threads"
#endif
       mapM (\n -> forkIO (worker)) [0..numCapabilities-1]

       -- This waits for quiescense:
       let waitloop = do num <- readIORef global_numworkers
	                 if num == 0
			  then return () 
			  else do 
#ifdef DEBUG_HASKELL_CNC
			          putStrLn ("=== Waiting on workers: "++ show num ++" left")
#endif
				  readChan joiner
				  atomicDecr global_numworkers
				  waitloop
       waitloop
       finalAction

-- FIXME: [2010.05.05] I believe this has a problem.
-- tryTakeMVar can fail spuriously if there's a collision with another
-- thread reading the mvar.  This is a sense in which mvars CANNOT
-- mimick IVars (at least ivars with the ability to test for presence
-- -- a monotonic test!)

-- This should only be a performance bug (forks an extra task for no
-- good reason).  When the code below falls back to readMVar that
-- should succeed.

ver5_6_core_get hook col tag = 
    do mvar    <- assureMvar col tag 
       hopeful <- tryTakeMVar mvar
       case hopeful of 
         Just v  -> do putMVar mvar v -- put it back where we found it
		       return v
	 -- Otherwise, no data.  If we block our own thread, we need to issue a replacement.
         Nothing -> do action <- readIORef global_makeworker
		       atomicIncr global_numworkers
		       -- If this were CPS then we would just give our
		       -- continuation to the forked thread.  Alas, no.
		       forkIO action
		       hook -- Any IO action can go here...
#ifdef DEBUG_HASKELL_CNC
		       putStrLn $ " >>> Blocked on "++ show tag ++"||| "
#endif
		       readMVar mvar
------------------------------------------------------------



------------------------------------------------------------
-- Version 6: Blocking with replacement.

-- When a thread goes down (blocks waiting on data) this version
-- regenerates a new thread to replace it.  The thread that went down
-- is mortal when it wakes up.  It will finish what it was doing and
-- then die.

-- This version is correct but sometimes inefficient.  It can have
-- threads terminate prematurely when the program enters a serial
-- bottleneck.

putt6 = putt5

-- Then at finalize time we set up the workers and run them.
finalize6 finalAction = 
    do joiner <- newChan 
       let worker = 
	       do x <- tryPop global_stack
		  case x of 
		    Nothing -> writeChan joiner ()
		    Just action -> 
			do action 
			   myId <- myThreadId
			   set  <- readIORef global_mortalthreads
			   if Set.notMember myId set
			      then worker
			      else writeChan joiner ()
       ver5_6_core_finalize joiner finalAction worker

get6 col tag = ver5_6_core_get io col tag
 where io = do myId  <- myThreadId
	       atomicModifyIORef global_mortalthreads (\s -> (Set.insert myId s, ()))

-- This is a bit silly, this emulates "thread local storage" to let
-- each worker thread know whether it is recursive (True) or "oneshot".
global_mortalthreads :: IORef (Set ThreadId)
global_mortalthreads = unsafePerformIO (newIORef Set.empty)

global_numworkers :: IORef Int
global_numworkers = unsafePerformIO (newIORef 0)

-- A computation that forks a new worker thread:
global_makeworker :: IORef (IO ())
global_makeworker = unsafePerformIO$ newIORef (return ())

------------------------------------------------------------
-- Version 7: Now with healing

-- TODO: Improve on 6 by correcting premature deaths.



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

type Step8 a = a -> StepCode8 ()
type TagCol8 a   = (IORef (Set a), IORef [Step8 a])
--type ItemCol a b = MutableMap a b

-- Here the hidden state keeps track of 
--newtype StepCode8 a = StepCode8 (S.StateT HiddenState8 IO a)
type StepCode8 a = (S.StateT (HiddenState8) IO a)

-- The hidden stat stores two things:
--   (1) "Self": the current action, if needed for requeueing.
--   (2) A list of child tasks/thunks that were spawned in parallel.
newtype HiddenState8 = HiddenState8 (StepCode8 (), [()])

-- In this version we don't use MVars because gets don't block:
--type ItemCol8 a b = IORef (Map a (IORef (Maybe b), IORef WaitingSteps))
newtype ItemCol8 a b = ItemCol8 (IORef (Map a ((Maybe b), WaitingSteps)))
type WaitingSteps = [StepCode8 ()]

data EscapeStep = EscapeStep  deriving (Show, Typeable)
instance Exception EscapeStep
--instance GHC.Exception.Exception EscapeStep

--------------------------------------------------------------------------------
-- Misc utility functions used by the version 8 API functions:
--------------------------------------------------------------------------------
liftHidden fn = (\ (HiddenState8 (self,ls)) -> HiddenState8 (self, fn ls))
atomicModifyIORef_ ref fn = atomicModifyIORef ref (\x -> (fn x, ()))

stepStats :: StepCode8 ()
stepStats = 
  do 
     tid <- S.lift myThreadId
     HiddenState8 (_, ls) <- S.get 
     S.lift$ putStrLn (">>>   Step state: list len: "++ show (length ls))


-- Could remove this bit of duplication if I would lift the other
-- cases into an identity monad transformer.  (Or unit state.) 
proto_putt8 :: Ord a =>  ([Step8 a] -> a -> StepCode8 b) -> TagCol8 a -> a -> StepCode8 b
proto_putt8 action tc@(_set,_steps) tag = 
    do set   <- S.lift$ readIORef _set
       steps <- S.lift$ readIORef _steps
       if memoize 
        then if Set.member tag set
	     then return ()
	     else S.lift $ writeIORef _set (Set.insert tag set)
        else return ()
       action steps tag

-- This is the high level interface for running several steps in
-- parallel and then blocking on the result.
launch_steps :: [StepCode8 ()] -> StepCode8 ()
launch_steps mls = 
    foldM (\ () m -> spawn (do try_stepcode m m; return ()))
          () mls


-- This consumes the state thats threaded through step code by capping
-- the end of the step with a sync.  It needs a retry action to tuck
-- into the state so that the step can store it if it needs to escape
-- with an exception.
-- 
-- DESIGN DECISION: 
try_stepcode :: StepCode8 () -> StepCode8 a -> IO (Maybe a)
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
spawn :: IO () -> StepCode8 ()
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

newItemCol8 :: ITEMPREREQS => GraphCode (ItemCol8 tag b)
newItemCol8 = do ref <- newIORef Map.empty
		 return (ItemCol8 ref)

newTagCol8  :: GraphCode (TagCol8 tag)
newTagCol8 = sharedNTC

putt8 :: Ord a  => TagCol8 a -> a -> StepCode8 ()
putt8 = proto_putt8
	         (\ steps tag -> 
		  -- Spark each downstream step, attempting to do it in parallel before a 
		  -- subsequent sync (at the end of the containing step).		  
                  launch_steps (Prelude.map (\step -> step tag) steps))

get8  :: ITEMPREREQS => ItemCol8 tag b -> tag -> StepCode8 b
get8 (ItemCol8 icol) tag = 
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


initfin :: String -> StepCode8 a -> GraphCode a       
initfin str m = do let err = error str
	           x <- try_stepcode err m
	           case x of Nothing -> err
		  	     Just v  -> return v

initialize8 = initfin "Get failed within initialize action!"
finalize8   = initfin "Get failed within finalize action!"


-- Put must replay any steps that are waiting.
put8  :: (Show a, Ord a) => ItemCol8 a b -> a -> b  -> StepCode8 ()
put8 (ItemCol8 icol) tag (!item) = 
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

itemsToList8 :: ITEMPREREQS => ItemCol8 tag b -> StepCode8 [(tag,b)]
itemsToList8 (ItemCol8 icol) = 
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


----------------------------------------------------------------------------------------------------

-- Pick an implementation:
-- (This has grown more complex with differences in the types used between schedulers.)

#ifndef CNC_SCHEDULER
#warning  "Cnc.hs -- CNC_SCHEDULER unset, defaulting to scheduler 6 "
#define CNC_SCHEDULER 6
#endif

cncVariant :: String
cncVariant="io/" ++ show (CNC_SCHEDULER :: Int)

#if CNC_SCHEDULER == 3
get=get3; putt=putt3; finalize=finalize3; quiescence_support=False; 
#elif CNC_SCHEDULER == 4
#elif CNC_SCHEDULER == 5
get=get5; putt=putt5; finalize=finalize5; quiescence_support=True ;
#elif CNC_SCHEDULER == 6
get=get6; putt=putt6; finalize=finalize6; quiescence_support=True ;
#elif CNC_SCHEDULER == 8
get=get8; putt=putt8; finalize=finalize8; quiescence_support=True ;
#else
#error "Cnc.hs -- CNC_SCHEDULER is not set to a support scheduler: {3,4,5,6,8}"
#endif

itemsToList :: ITEMPREREQS => ItemCol tag b -> StepCode [(tag,b)]

-- |A monad representing the computations performed by nodes in the CnC
-- |graph.  This includes putting out tags and items that will be
-- |consumed by other steps.
#if CNC_SCHEDULER == 8
type StepCode a   = StepCode8 a
type TagCol   a   = TagCol8   a 
type ItemCol  a b = ItemCol8  a b
newItemCol = newItemCol8
newTagCol  = newTagCol8
put = put8 
initialize = initialize8
itemsToList x = itemsToList8 x
stepUnsafeIO io = S.lift$ io
#else
type StepCode a   = StepCode0 a
type TagCol   a   = TagCol0   a 
type ItemCol  a b = ItemCol0  a b
newItemCol = newItemCol0
newTagCol  = newTagCol0
put = put0
initialize = initialize0
itemsToList x = itemsToList0 x
stepUnsafeIO io = io
#endif

-- |Steps are functions that take a single 'tag' as input and perform
-- |a computation in the "StepCode" monad, which may perform "put"s and "get"s.
type Step     a   = a -> StepCode ()

cncUnsafeIO io = io

stepPutStr :: String -> StepCode ()
stepPutStr str = stepUnsafeIO (putStr str)
cncPutStr :: String -> GraphCode ()
cncPutStr  str = cncUnsafeIO  (putStr str)


--------------------------------------------------------------------------------

atomicIncr x = atomicModifyIORef x (\n -> (n+1, ()))
atomicDecr x = atomicModifyIORef x (\n -> (n-1, ()))

-- --------------------------------------------------------------------------------
-- -- Test program:
{-
incrStep d1 (t2,d2) tag = 
 do val <- get d1 tag 
    putStr ("  ("++ show tag ++") Incrementing " ++ show val ++"\n")
    put d2 tag (val + 1)
    putt t2 tag

test = -- Allocate collections:
    do t1 <- newTagCol()
       t2 <- newTagCol()
       t3 <- newTagCol()
       d1 <- newItemCol()
       d2 <- newItemCol()
       d3 <- newItemCol()
        -- Initialize:
       put d1 'a' 33
       put d1 'b' 100
       -- Build and execute the graph:
       prescribe t1 (incrStep d1 (t2,d2))
       prescribe t2 (incrStep d2 (t3,d3))
       -- Start things up:	 
       putt t1 'a'
       putt t1 'b'
       -- Read the results (waits until they're ready):
       result1 <- get d3 'a'
       result2 <- get d3 'b'
       return (result1, result2) 

-}


tests :: Test
tests = TestList [ ]

