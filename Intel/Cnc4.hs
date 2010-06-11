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
-- execution is finished.  Note, this ALSO includes the mortal threads
-- business from version 6.

#include "shared_5_6.hs"


-- FIXME: TODO: This version needs the Mortal threads mechanism in version 6...
-- Otherwise it creates extra threads that SPIN, which is terrible.

----------------------------------------------------------------------------------------------------
 
get col tag = ver5_6_core_get (return ()) col tag

-- get col tag = 
--  do (HiddenState5 { stack, mortal }) <- S.get
--     let io = do myId  <- myThreadId	      
--  	        modifyHotVar_ mortal (Set.insert myId)
--     ver5_6_core_get io col tag


-- At finalize time we set up the workers and run them.
finalize userFinalAction = 
    do (state @ HiddenState5 { stack, numworkers, mortal } ) <- S.get
       
       let worker id = 
	       do x <- STEPLIFT tryPop stack
                  let lifecheck = 
		        do myId <- STEPLIFT myThreadId
			   set  <- STEPLIFT readHotVar mortal
			   return (Set.notMember myId set)
		  case x of 
		    Nothing -> do n <- STEPLIFT readHotVar numworkers
				  --STEPLIFT putStrLn$ "NUM WORKERS IS " ++ show n++"\n"
				  -- numWorkers == 0 implies a message to shutdown.
				  if n == 0 
				   then do --STEPLIFT putStrLn$ "================ SHUTTING DOWN "++ show id ++"=============="
					   return ()					
				   else do -- A mortal thread should never spin:
					   b <- lifecheck
					   if b 
					    -- Should we be cooperative by sleeping a little?
					    --System.Posix.usleep 1000
					    then do STEPLIFT yield
						    worker id
					    else return ()
		    Just action -> do action
				      b <- lifecheck
				      if b then worker id else return ()

       let finalAction = 
	    do S.modify$ \stt -> stt { myid = numCapabilities-1 }

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
       ver5_6_core_finalize (error "joiner unused") finalAction worker False numCapabilities (\_ -> return ())

------------------------------------------------------------

quiescence_support = False

