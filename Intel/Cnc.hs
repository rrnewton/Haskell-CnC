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
{-# OPTIONS_HADDOCK prune #-}
{-
 - Intel Concurrent Collections for Haskell
 - Copyright (c) 2010, Intel Corporation.
 -
 - This program is free software; you can redistribute it and/or modify it
 - under the terms and conditions of the GNU Lesser General Public License,
 - version 2.1, as published by the Free Software Foundation.
 -
 - This program is distributed in the hope it will be useful, but WITHOUT
 - ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 - FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 - more details.
 -
 - You should have received a copy of the GNU Lesser General Public License along with
 - this program; if not, write to the Free Software Foundation, Inc., 
 - 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
 -
 -}

#define MODNAME Intel.Cnc

-- This file is simple here to dispatch to the appropriate scheduler implementation.

#ifndef CNC_SCHEDULER
#warning  "Cnc.hs -- CNC_SCHEDULER unset, defaulting to scheduler 6 "
#define CNC_SCHEDULER 6
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
#else
#error "Cnc.hs -- CNC_SCHEDULER is not set to a support scheduler: {3,4,5,6,7,8,9}"
#endif
