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
-- We don't need to lift through a monad transformer for the step or
-- graph monads in this implementation:
#ifndef MODNAME
#define MODNAME Intel.Cnc3
#endif
#define CNC_SCHEDULER 3
#define STEPLIFT  id$
#define GRAPHLIFT id$
-- #define SUPPRESS_cncFor
#include "Cnc.Header.hs"

type TagCol  a   = (IORef (Set.Set a), IORef [Step a])
type ItemCol a b = MutableMap a b

type StepCode  = IO 
type GraphCode = IO

------------------------------------------------------------
-- Version 3: Here we try for forked parallelism:
------------------------------------------------------------

putt = proto_putt (\ steps tag -> 
		    case steps of 
	             --[] -> error "putt on tag collection with no prescribed steps"
	             steps -> 
		      foldM (\ () step -> do forkIO (step tag); return ())
   	  	       () steps
		   )

-- We needn't fork a new thread if it's "tail call"
tail_putt :: Ord a  => TagCol a -> a -> StepCode ()
tail_putt = proto_putt$ \ steps tag -> 
	       case steps of
	          []       -> error "putt on tag collection with no prescribed steps"
		  fst:rest -> 
		     do forM_ rest $ \step -> forkIO (step tag)
			fst tag

get col tag = do mvar <- assureMvar col tag 
		 readMVar mvar

-- The above 'putt's use a trivial finalizer:
-- WARNING -- this will not wait for workers to finish during finalization.
-- Therefore, this only works with programs that 'get' their output.
-- E.g. it does not support quiescent completion.
finalize x = x 
-- TODO: At least kill off the existing threads here?

quiescence_support=False; 


--------------------------------------------------------------------------------
-- EXPERIMENTAL:
--------------------------------------------------------------------------------
-- This is a proposed addition for manipulating items outside of item collections.

type Item = MVar
newItem  = newEmptyMVar
readItem = readMVar
putItem mv x = 
  do b <- tryPutMVar mv x
     if b then return ()
	  else error "Violation of single assignment rule; second put on Item!"

#ifdef SUPPRESS_cncFor
-- Because this scheduler doesn't have the *nested* structure that,
-- say, scheduler 8 does, the default definition of cncFor will not
-- provide much benefit.  Instead, we try one that uses explicit
-- placement of threads.
cncFor start end body = 
 -- With this version we don't create any additional graph nodes.
 -- Instead, we create additional IO threads.
 do forM_ [0..numthreads-1] fork_thread      
 where 
    splitfactor = 1 -- TBB uses 4, but IO threads have more overhead...
    numthreads = numCapabilities * splitfactor
    ranges = splitInclusiveRange numthreads (start,end)
    fork_thread i = 
     -- Assign the IO thread to a particular CPU:
     forkOnIO (i `quot` splitfactor) $ 
       let (x,y) = ranges !! i in
       for_ x y body
#endif
