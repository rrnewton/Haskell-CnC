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


import System.Environment
import Data.Int
import Control.Monad

#include "haskell_cnc.h"

-- This is a SERIAL benchmark, but it demonstrates how item
-- collections can be used for something like dynamic programming.

run n = runGraph $  
       do tags  :: TagCol  Int       <- newTagCol
	  items :: ItemCol Int Int64 <- newItemCol
	  prescribe tags $ \i -> 
	     do x <- get items (i-1)
		y <- get items (i-2)
		put items i (x+y)
	  initialize $ 
	     do put items 0 0 		
		put items 1 1 
		forM_ [2..n] (putt tags)
		--forM_ (reverse [2..n]) (putt tags)
 	  finalize $ 
	     do get items n


main = do args <- getArgs 
	  putStrLn $ show $ 
	   case args of 
	    []  -> run 10
	    [s] -> run (read s)

