{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , RankNTypes
  #-}


-- UNFINISHED: this version will implement work stealing deques as
-- Haskell data structures.


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
#define SUPPRESS_initialize


#warning "Loading Scheduler 11..."

#include "Cnc.Header.hs"

-- Load the work sharing task-pool:
#include "work_stealing_deques.hs"

-- Load the core of the new ContT implementation (Simon's):
#include "CncSM.hs"
