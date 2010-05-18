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


#define INCLUDEMETHOD

#if   CNC_VARIANT == 0
#warning "Loading CNC as a separately compiled module" 
-- This is here to test the efficiency of the normal module include method:
import Intel.Cnc

#undef INCLUDEMETHOD

#elif CNC_VARIANT == 1
#include "Intel/CncPure.hs"
#elif CNC_VARIANT == 2
#include "Intel/Cnc.hs"
#elif CNC_VARIANT == 3
#error "Cnc_serialST not fully working yet"
#include "Cnc_serialST.hs"
#else
#error "CNC_VARIANT not set to a known value."
#endif
