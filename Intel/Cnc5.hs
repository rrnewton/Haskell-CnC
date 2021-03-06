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
#define MODNAME Intel.Cnc5
#endif
#define CNC_SCHEDULER 5
#define STEPLIFT  S.lift$
#define GRAPHLIFT S.lift$
#define SUPPRESS_runGraph
#define DEFINED_free_items
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
    do (HiddenState5 { stack }) <- S.get
       joiner <- GRAPHLIFT newChan 
       let worker id = 
	       do x <- STEPLIFT tryPop stack
		  case x of 
		    Nothing -> STEPLIFT writeChan joiner id
		    Just action -> do action
				      worker id     
       ver5_6_core_finalize joiner finalAction worker True numCapabilities (\_ -> return ())

------------------------------------------------------------

quiescence_support = True

