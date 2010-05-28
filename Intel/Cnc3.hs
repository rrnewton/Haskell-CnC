{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
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
#include "Cnc.Header.hs"

type TagCol  a   = (IORef (Set a), IORef [Step a])
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
