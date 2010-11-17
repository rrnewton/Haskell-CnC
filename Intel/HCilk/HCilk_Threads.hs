
--------------------------------------------------------------------------------
-- Cilk interface in Haskell.
-- This simple version uses forkIO on every spawn.
--------------------------------------------------------------------------------

module Intel.HCilk.HCilk_Threads
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


-- TEMP:
import Data.Int
import System.Environment

--type SyncObj = MVar ()
--data Future a = Future a 
type SyncObj = IO ()
newtype Future a = Future (MVar a)
type HCilk a = S.StateT [SyncObj] IO a


------------------------------------------------------------
-- TODO 
-- Should we add HCilk specific versions of IVar/MVar?
-- Is there anything the scheduler could do on a blocking call?

------------------------------------------------------------

lift :: IO a -> HCilk a
lift = S.lift 

runCilk :: HCilk a -> IO a
runCilk hc = 
    do (res,ls) <- S.runStateT hc []
       -- Clear out waiting futures, all IO's must happen.
       sequence_ ls
       return res

-- | Spawn an IO computation for asynchronous, possibly parallel execution.
spawn :: HCilk a -> HCilk (Future a)
spawn hc = 
  do mvar <- S.lift$ newEmptyMVar
     -- The mvars store multiple types, so we represent the stored syncs as IO actions:
     S.modify ((do readMVar mvar; return ()) :)
     S.lift$ forkIO (do x <- runCilk hc; putMVar mvar x)
     return (Future mvar)

-- | This version of spawn permits (optional) low-probability
-- | duplication of spawned tasks in exchange for possibly greater
-- | efficiency.
spawnDupable = spawn

-- | Synchronize all spawned computations (like the Cilk sync keyword).
syncAll :: HCilk ()
syncAll = 
  do ls <- S.get 
     S.lift$ sequence_ ls

-- | Synchronize only a single outstanding spawned computation and return its result.
sync :: Future a -> HCilk a 
sync (Future mv) = S.lift$ readMVar mv



