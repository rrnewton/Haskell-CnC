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
-- Author: Ryan Newton 

-- #include <haskell_cnc.h>

-- This demonstrates the normal (NOT #include) method of loading CnC:
import Intel.Cnc


-- Here's an odd little hello world where we communicate the two words
-- to a computational step which puts them together.

myStep items tag =
  do word1 <- get items "left"
     word2 <- get items "right"
     put items "result" (word1 ++ word2 ++ show tag)

cncGraph = 
  do tags  <- newTagCol
     items <- newItemCol
     prescribe tags (myStep items)
     initialize$ 
        do put items "left"  "Hello "
	   put items "right" "World "
	   putt tags 99
     finalize$ 
        do get items "result"

main = putStrLn (runGraph cncGraph)
