{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , DeriveDataTypeable
  , MultiParamTypeClasses
  #-}
-- State monad transformer is needed for both step & graph:
#ifndef MODNAME
#define MODNAME Intel.Cnc6
#endif
#define CNC_SCHEDULER 6
#define STEPLIFT  S.lift$
#define GRAPHLIFT S.lift$
#define SUPPRESS_runGraph
#include "Cnc.Header.hs"

------------------------------------------------------------
-- Version 6: Blocking with replacement.

#include "shared_5_6.hs"

-- When a thread goes down (blocks waiting on data) this version
-- regenerates a new thread to replace it.  The thread that went down
-- is mortal when it wakes up.  It will finish what it was doing and
-- then die.

-- This version is correct but sometimes inefficient.  It can have
-- threads terminate prematurely when the program enters a serial
-- bottleneck.


-- Then at finalize time we set up the workers and run them.
finalize finalAction = 
    do joiner <- GRAPHLIFT newChan 
       (HiddenState5 (stack, _, _, mortalthreads)) <- S.get
       let worker :: StepCode () = 
	       do x <- STEPLIFT tryPop stack
		  case x of 
		    Nothing -> STEPLIFT writeChan joiner ()
		    Just action -> 
			do action 
			   myId <- STEPLIFT myThreadId
			   set  <- STEPLIFT readIORef mortalthreads
			   if Set.notMember myId set
			      then worker -- keep going
			      else STEPLIFT writeChan joiner ()
       ver5_6_core_finalize joiner finalAction worker

get col tag = 
 do (HiddenState5 (stack, _, _, mortalthreads)) <- S.get
    let io = do myId  <- myThreadId	      
 	        atomicModifyIORef mortalthreads (\s -> (Set.insert myId s, ()))
    ver5_6_core_get io col tag

quiescence_support = True


------------------------------------------------------------
-- Version 7: Now with healing -- bring back worker threads that died
-- prematurely.

-- TODO: Improve on 6 by correcting premature deaths.

