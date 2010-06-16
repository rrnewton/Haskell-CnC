{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , RankNTypes
  #-}

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


#warning "Loading new split version..."

#include "Cnc.Header.hs"

-----------------------------------------------------------------------------

data Sched = Sched 
    { workpool :: HotVar [StepCode ()],
      myid :: Int
    }
  deriving Show

defaultState = do
  pool <- newHotVar []
  return$ Sched { workpool=pool, myid = -999 }


finalize finalAction = 
  proto_finalize $ \ joiner -> do
       -- Wait till all workers complete.
       GRAPHLIFT forM_ [1.. numCapabilities] $ \_ -> readChan joiner
       --cncPutStr$ " *** Workers returned, now finalize action:\n"
       finalAction			   

quiescence_support=True

numWorkers = numCapabilities

-----------------------------------------------------------------------------

-- Load the work sharing task-pool:
#include "simple_stack.hs"

-- Load the core of the new ContT implementation (Simon's):
#include "CncSM.hs"
