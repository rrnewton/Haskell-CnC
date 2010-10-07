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

import Data.Complex
import Data.Int
import System.Environment
import Control.Monad

-- #define USE_GMAP
-- #define MEMOIZE
#include <haskell_cnc.h>

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0 
  where   
   fn = magnitude
   loop i z 
    | i == max_depth = i
    | fn(z) >= 2.0   = i
    | otherwise      = loop (i+1) (z*z + c) 

type Pair = (Int16, Int16)

dynAPI = True -- TEMPTOGGLE

mandelProg :: Int -> Int -> Int -> GraphCode Int
mandelProg max_row max_col max_depth = 
    do --dat      :: ItemCol Pair (Complex Double) <- newItemCol
       pixel    :: ItemCol Pair Int              <- newItemCol
       
       let mandelStep tag@(i,j) = 
	    let z = (r_scale * (fromIntegral j) + r_origin) :+ 
  		    (c_scale * (fromIntegral i) + c_origin) in
	    do tid <- stepUnsafeIO myThreadId
	       --stepPutStr$ "["++ show tid ++"] Mandel Step executing: "++ show tag ++ "\n"
	       --cplx <- get dat tag
	       put pixel tag (mandel max_depth z)

       position :: TagCol  Pair <- prescribeNT [mandelStep] 

       initialize $ 
        forM_ [0..max_row-1] $ \i -> 
         forM_ [0..max_col-1] $ \j ->
          let (_i,_j) = (fromIntegral i, fromIntegral j)
	      z = (r_scale * (fromIntegral j) + r_origin) :+ 
  		  (c_scale * (fromIntegral i) + c_origin) in
	  do -- put dat (_i,_j) z
	     if dynAPI
	       then forkStep$ mandelStep (_i,_j)
	       else putt position (_i,_j)

       -- Final result, count coordinates of the  pixels with a certain value:
       finalize $ do 
        --stepPutStr$ "Finalize action begun...\n"
	foldM (\acc i -> 
          foldM (\acc j -> 
	           do --stepPutStr$ " ... try get pixel "++ show (i,j) ++"\n "
		      p <- get pixel (fromIntegral i, fromIntegral j)
		      --stepPutStr$ " GET PIXEL SUCCESSFUL "++ show (i,j) ++"\n "
		      if p == max_depth
   		       then return (acc + (i*max_col + j))
   		       else return acc)
	        acc [0..max_col-1]
              ) 0 [0..max_row-1] 
       
   where 
    r_origin = -2                            :: Double
    r_scale  = 4.0 / (fromIntegral max_row)  :: Double
    c_origin = -2.0                          :: Double
    c_scale = 4.0 / (fromIntegral max_col)   :: Double


runMandel a b c = 
	   let check = runGraph $ mandelProg a b c in
	   putStrLn ("Mandel check " ++ show check)

main = do args <- getArgs  
	  case args of
	   --[]      -> runMandel 1 1 3   -- Should output 57.
	   []      -> runMandel 4 4 3   -- Should output 57.
	   [a,b,c] -> runMandel (read a) (read b) (read c)
