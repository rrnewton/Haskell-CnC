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


-- FIXME: TODO: This version needs the Mortal threads mechanism in version 6...
-- Otherwise it creates extra threads that SPIN, which is terrible.

----------------------------------------------------------------------------------------------------
 
get col tag = ver5_6_core_get (return ()) col tag

-- At finalize time we set up the workers and run them.
finalize userFinalAction = 
    do (state @ HiddenState5 { stack, numworkers } ) <- S.get
       joiner <- GRAPHLIFT newChan 
       let worker id = 
	       do x <- STEPLIFT tryPop stack
		  case x of 
		    Nothing -> do n <- STEPLIFT readHotVar numworkers
				  --STEPLIFT putStrLn$ "NUM WORKERS IS " ++ show n++"\n"
				  if n == 0 
				   then do --STEPLIFT putStrLn$ "================ SHUTTING DOWN "++ show id ++"=============="
					   return ()
				   else do -- Should we be cooperative by sleeping a little?
					   --System.Posix.usleep 1000
					   STEPLIFT yield
					   worker id
		    Just action -> do action
				      worker id

       let finalAction = 
	    do S.put$ state { myid = numCapabilities-1 }
               val <- userFinalAction
	       -- UGLY Convention: reusing numworkers variable that's already in the type:
	       -- This variable becomes a "command" rather than diagnosing the current state.  Zero means stop working.
	       STEPLIFT writeHotVar numworkers 0 -- Shut workers down (eventually).
	       --STEPLIFT putStrLn$ " ..............>>>>>>>>>>>>>>>>>>> Initiated shutdown...........\n"
	       return val

       -- The final action will itself be one of the threads and it
       -- will replace itself when it blocks on a get.  Therefore we
       -- request one fewer worker:
       --ver5_6_core_finalize joiner finalAction worker False (numCapabilities-1)
       -- 
       -- FIXME: TODO: Currently having inexplicable problems on embarassingly_par with the N-1 approach.
       -- For now oversubscribing intentionally as the lesser of evils:
       ver5_6_core_finalize joiner finalAction worker False numCapabilities (\_ -> return ())

------------------------------------------------------------

quiescence_support = False
