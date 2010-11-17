
--------------------------------------------------------------------------------
-- Cilk interface in Haskell.
-- This version uses the GHC scheduler directly -- sparks rather than forkIO.
--------------------------------------------------------------------------------

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

module Intel.HCilk.HCilk_Sparks 
    ( 
      HCilk, Future 
    , runCilk
    , spawn, spawnDupable
    , sync
    )
where


import qualified  Control.Monad.State.Strict as S 
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import System.IO.Unsafe
import Data.List

import Control.Exception
--import Control.Exception.Extensible
import GHC.IO
import GHC.Conc

-- TEMP:
import Data.Int
import System.Environment

type HCilk a = S.StateT HiddenState IO a
newtype Future a = Future a

-- The hidden state stores a list of child tasks/thunks that were
-- spawned in parallel.
newtype HiddenState = HiddenState [()]

-- | Run a Cilk computation in parallel and then return control to the calling thread.
runCilk :: HCilk a -> IO a
runCilk hc = 
      do (v, HiddenState ls) <- S.runStateT hc (HiddenState [])
         -- Sync all child computations that were created.
         -- We may be racing to fill these in with other threads.
	 (foldl' pseq () ls) `pseq` return v

{-# INLINE spawn_core #-}
spawn_core unsafe hc = 
    do -- Add the new action to the list of actions for this step.
       let thunk = unsafe $ runCilk hc
       -- I wish this could be avoided, I hate to create a wrapper:
       let unitthunk = thunk `pseq` ()
       S.modify $ (\ (HiddenState ls) -> HiddenState$ unitthunk:ls)
       thunk `par` (return (Future thunk))

{-# INLINE spawn #-}
spawn :: HCilk a -> HCilk (Future a)
spawn = spawn_core unsafePerformIO

{-# INLINE spawnDupable #-}
spawnDupable :: HCilk a -> HCilk (Future a)
spawnDupable = spawn_core unsafeDupablePerformIO

{-# INLINE sync #-}
-- | Synchronize only a single outstanding spawned computation and return its result.
sync :: Future a -> HCilk a 
sync (Future thunk) = thunk `pseq` return thunk

