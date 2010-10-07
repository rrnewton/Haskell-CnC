{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , TypeFamilies 
  , UndecidableInstances
  , OverlappingInstances
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , NamedFieldPuns
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


#define SUPPRESS_HiddenState5
#include "shared_5_6.hs"

-- We extend the type with the "deadset" field:
data HiddenState5 = 
    HiddenState5 { stack :: HotVar [StepCode ()], 
		   numworkers :: HotVar Int, 
		   makeworker :: Int -> IO (), 
		   mortal :: HotVar (Set.Set ThreadId),
		   myid :: Int,
		   deadset :: HotVar [Int]
		 }
  deriving Show

defaultState = 
  do hv  <- newHotVar []
     hv2 <- newHotVar 0
     hv3 <- newHotVar Set.empty
     hv4 <- newHotVar []
     let msg = "Intel.Cnc"++ show CNC_SCHEDULER ++" internal error: makeworker thunk used before initalized"
     return$ HiddenState5 { stack = hv, numworkers = hv2, makeworker= error msg, 
			    mortal = hv3, myid = -1, 
			    deadset = hv4 }

-- A push that also wakes the dead.
stepcode_push :: HotVar [a] -> a -> StepCode ()
stepcode_push stack val = 
  -- If push onto an empty stack, wake up deadset.  
  do old <- STEPLIFT modifyHotVar stack (\ls -> (val:ls, ls))
     if null old
      -- Wake up the dead:
      then do (HiddenState5 { numworkers, makeworker, deadset }) <- S.get
	      dead <- STEPLIFT modifyHotVar deadset (\old -> ([], old))
	      let len = length dead
              --STEPLIFT putStrLn$ "        **********    Waking the dead: " ++ show dead 
	      if len > 0 
	       then do STEPLIFT modifyHotVar_ numworkers (+ len)
		       STEPLIFT forM_ dead (forkIO . makeworker)
	       else return ()
      else return () 

putt = proto_putt
	(\ steps tag -> 
	   do (HiddenState5 { stack }) <- S.get
              foldM (\ () step -> stepcode_push stack (step tag))
                       () steps)

-- New, more dynamic API:
forkStep s = 
  do (HiddenState5 { stack }) <- S.get
     stepcode_push stack s

get col tag = ver5_6_core_get (return ()) col tag

-- At finalize time we set up the workers and run them.
finalize userFinalAction = 
    do (HiddenState5 { stack, deadset }) <- S.get
       joiner <- GRAPHLIFT newChan 
       let worker id = 
	       do x <- STEPLIFT tryPop stack
		  case x of 
		    Nothing -> STEPLIFT writeChan joiner id
		    Just action -> do action
				      worker id

       let joinerHook id = modifyHotVar_ deadset (id:)

       ver5_6_core_finalize joiner userFinalAction worker True numCapabilities joinerHook

------------------------------------------------------------

quiescence_support = True

