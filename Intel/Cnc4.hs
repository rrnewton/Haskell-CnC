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
#define MODNAME Intel.Cnc4
#endif
#define CNC_SCHEDULER 4
#define STEPLIFT  S.lift$
#define GRAPHLIFT S.lift$
#define SUPPRESS_runGraph
#define DEFINED_free_items
#include "Cnc.Header.hs"

------------------------------------------------------------
-- Version 4: Global work queue and worker threads that spin until
-- execution is finished.

#include "shared_5_6.hs"

----------------------------------------------------------------------------------------------------
 
get col tag = ver5_6_core_get (return ()) col tag

-- At finalize time we set up the workers and run them.
finalize userFinalAction = 
    do (HiddenState5 (stack, numworkers, _, _, _)) <- S.get
       joiner <- GRAPHLIFT newChan 
       let worker id = 
	       do x <- STEPLIFT tryPop stack
		  case x of 
		    Nothing -> do b <- STEPLIFT readHotVar numworkers
				  if b == 0 
				   then return ()
				   else do -- Should we be cooperative by sleeping a little?
					   --System.Posix.usleep 1000
					   STEPLIFT yield
					   worker id
		    Just action -> do action
				      worker id

       let finalAction = 
	    do val <- userFinalAction
	       -- UGLY Convention: reusing numworkers variable that's already in the type:
	       -- This variable becomes a "command" rather than diagnosing the current state.  Zero means stop working.
	       STEPLIFT writeHotVar numworkers 0 -- Shut workers down (eventually).
	       return val

       ver5_6_core_finalize joiner finalAction worker False

------------------------------------------------------------

quiescence_support = False
