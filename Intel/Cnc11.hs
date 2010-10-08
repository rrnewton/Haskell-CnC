{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , RankNTypes
  #-}


-- This version will implement work stealing deques as
-- Haskell data structures.


#ifndef MODNAME
#define MODNAME Intel.Cnc10
#endif
#define CNC_SCHEDULER 10
#define STEPLIFT  C.liftIO$ R.liftIO$
#define GRAPHLIFT C.liftIO$ R.liftIO$

#define SUPPRESS_put
#define SUPPRESS_newItemCol
#define SUPPRESS_newTagCol
#define SUPPRESS_itemsToList
#define SUPPRESS_runGraph
#define SUPPRESS_initialize

#warning "Loading Scheduler 11..."
#include "Cnc.Header.hs"

data Sched = Sched 
    { workpool :: Array.Array Int (HotVar (Seq.Seq (StepCode ()))),
      randoms :: Array.Array Int (IORef Random.StdGen),
      myid :: Int,
      killflag :: IORef Bool
     }
--  deriving Show

defaultState = do
  dvars <- forM [0..numCapabilities] $ \_ -> newHotVar Seq.empty
  let randoms = Array.listArray (0,numCapabilities) [ Random.mkStdGen id | id <- [0..numCapabilities ]]
  rands <- sequence [ newIORef (Random.mkStdGen id) | id <- [0..numCapabilities ]]
  let randoms = Array.listArray (0,numCapabilities-1) rands
  let deques  = Array.listArray (0,numCapabilities-1) dvars
  kill <- newIORef False
  return$ Sched { workpool= deques, 
		  randoms = randoms,
		  killflag = kill,
		  myid = 0 -- The scheduler thread.
		}

#if 1
initialize x = x 
#else
-- One option here is that at the end of the initialize action, stripe
-- the work across dequeus.  Another option to accomplish the same
-- work-sharing effect is to round-robin distribute the work when a
-- push is done from the scheduler thread running "initialize".
initialize initAction = 
  do initAction
     Sched { workpool }  <- R.ask
     -- Read the deque of the scheduler thread.  No parallelism yet so don't worry about atomicity:
     initdeque <- C.liftIO$ readHotVar (workpool Array.! 0) 
     -- Just for hygiene clear it:
     C.liftIO$ writeHotVar (workpool Array.! 0) Seq.empty

     -- We take one of the pieces of work ourselves (worker 0)
     let segs = splitSeq numCapabilities initdeque
     C.liftIO$ forM_ (zip [0..] $ segs) $ \ (id,seq) -> 
       modifyHotVar_ (workpool Array.! id) $ \ olddeq -> 
         if not$ Seq.null olddeq
	 then error$ "Worker's deque ("++ show id ++") was not empty at initialization, size: "++ show (aSeq.length olddeq)
	 else seq
     stepPutStr$ " +++ FINISHED DISSEMINATING " ++ show (Seq.length initdeque) ++ " initial units of work!: "
		 ++ show (map Seq.length segs) ++ "\n"
#endif

splitSeq :: Int -> Seq.Seq a -> [Seq.Seq a]
splitSeq pieces seq = all
  where 
   (all, _)     = loop portion (pieces-remain) seq' bigs
   (bigs, seq') = loop (portion+1) remain seq []

   len = Seq.length seq
   (portion, remain) = len `quotRem` pieces
   loop block 0 seq acc = ( acc, seq )
   loop block n seq acc = 
      let (hd,tl) = Seq.splitAt block seq 
      in loop block (n-1) tl (hd:acc)



finalize finalAction = 
  proto_finalize $ \ joiner -> do
       Sched { killflag } <- R.ask
       result <- finalAction			   
       stepPutStr$ " +++ Finished final action, now killing...\n"
       C.liftIO$ writeIORef killflag True
       -- Optional, wait till all workers complete also:
       --GRAPHLIFT forM_ [1.. numCapabilities] $ \_ -> readChan joiner
       return result

itemsToList = error "itemstolist not implemented yet for this scheduler" -- XXX

quiescence_support=False

numWorkers = numCapabilities - 1

--------------------------------------------------------------------------------

-- Load the work sharing task-pool:
#include "work_stealing_deques.hs"

-- Load the core of the new ContT implementation (Simon's):
#include "CncSM.hs"
