{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , CPP
  #-}
-- State monad transformer is needed for both step & graph:
#ifndef MODNAME
#define MODNAME Intel.Cnc5
#endif
#define CNC_SCHEDULER 5
#define STEPLIFT  S.lift$
#define GRAPHLIFT S.lift$
#define SUPPRESS_runGraph
#include "Cnc.Header.hs"

------------------------------------------------------------
-- Version 5a: A global work queue.
-- (This version has been disabled/removed.)
-- This version uses a global work-queue.
-- Here laziness comes in handy, we queue the thunks.

#include "shared_5_6.hs"

------------------------------------------------------------
-- Version 5: Thread spammer -- fork a permanent worker every time we block.

-- TODO: we should do a better job here by using a monad transformer on top of IO:
-- But if we must keep the same CnC interface... This is expedient:
 
get col tag = ver5_6_core_get (return ()) col tag

-- At finalize time we set up the workers and run them.
finalize finalAction = 
    do (HiddenState5 (stack, _, _, _)) <- S.get
       joiner <- GRAPHLIFT newChan 
       let worker = 
	       do x <- STEPLIFT tryPop stack
		  case x of 
		    Nothing -> STEPLIFT writeChan joiner ()
		    Just action -> do action
				      worker      
       ver5_6_core_finalize joiner finalAction worker 

------------------------------------------------------------

quiescence_support = True

type Item = MVar
newItem  = STEPLIFT newEmptyMVar
readItem = grabWithBackup (return ())
putItem mv x = 
  do b <- STEPLIFT tryPutMVar mv x
     if b then return ()
	  else error "Violation of single assignment rule; second put on Item!"
