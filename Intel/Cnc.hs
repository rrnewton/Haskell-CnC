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
  , RankNTypes
  #-}
{-# OPTIONS_HADDOCK prune #-}


#define MODNAME Intel.Cnc

-- This file is simple here to dispatch to the appropriate scheduler implementation.

#ifndef CNC_SCHEDULER
#warning  "Cnc.hs -- CNC_SCHEDULER unset, defaulting to scheduler 7 "
#define CNC_SCHEDULER 7
#endif

#if CNC_SCHEDULER == 3
#include "Cnc3.hs"
#elif CNC_SCHEDULER == 4
#include "Cnc4.hs"
#elif CNC_SCHEDULER == 5
#include "Cnc5.hs"
#elif CNC_SCHEDULER == 6
#include "Cnc6.hs"
#elif CNC_SCHEDULER == 7
#include "Cnc7.hs"
#elif CNC_SCHEDULER == 8
#include "Cnc8.hs"
#elif CNC_SCHEDULER == 9
#include "Cnc9.hs"

#elif CNC_SCHEDULER == 10
#include "Cnc10.hs"

#elif CNC_SCHEDULER == 11
#include "Cnc11.hs"

#elif CNC_SCHEDULER == 99
#include "Cnc10ver1.hs"

#else
#error "Cnc.hs -- CNC_SCHEDULER is not set to a support scheduler: {3,4,5,6,7,8,10,11}"
#endif
