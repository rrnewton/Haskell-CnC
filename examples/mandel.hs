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
import Data.Word
import System.Environment

-- #define USE_GMAP
-- #define MEMOIZE
#include <haskell_cnc.h>

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0 0
  where   
   fn = magnitude
   loop i z count
    | i == max_depth = count
    | fn(z) >= 2.0   = count 
    | otherwise      = loop (i+1) (z*z + c) (count+1)

-- A pair will fit in a word:
--type Pair = (Word16, Word16)
type Pair = (Int, Int)

mandelProg :: Int -> Int -> Int -> GraphCode Int
mandelProg max_row max_col max_depth = 
    do position :: TagCol  Pair                  <- newTagCol
       dat      :: ItemCol Pair (Complex Double) <- newItemCol
       pixel    :: ItemCol Pair Int              <- newItemCol
       
       let mandelStep tag = 
	    do cplx <- get dat tag
	       put pixel tag (mandel max_depth cplx)

       prescribe position mandelStep 

-- #define trust_lists

       initialize $ 
#ifdef trust_lists
#warning "TRUSTING LIST FUSION OPTS"
        forM_ [0..max_row] $ \i -> 
         forM_ [0..max_col] $ \j ->
          let (_i,_j) = (fromIntegral i, fromIntegral j)
	      z = (r_scale * (fromIntegral j) + r_origin) :+ 
  		  (c_scale * (fromIntegral i) + c_origin) in
	  do put dat (_i,_j) z
	     putt position (_i,_j)
#else
        for_ 0 max_row $ \i -> 
         for_ 0 max_col $ \j ->
          let (_i,_j) = (fromIntegral i, fromIntegral j)
	      z = (r_scale * (fromIntegral j) + r_origin) :+ 
  		  (c_scale * (fromIntegral i) + c_origin) in
	  do put dat (_i,_j) z
	     putt position (_i,_j)
#endif

       -- Final result, count coordinates of the  pixels with a certain value:
       finalize $ 
#ifdef trust_lists
	foldM (\acc i -> 
          foldM (\acc j -> 
	           do p <- get pixel (fromIntegral i, fromIntegral j)
		      if p == max_depth
   		       then return (acc + (i*max_col + j))
   		       else return acc)
	        acc [0..max_col]
              ) 0 [0..max_row] 
#else
	foldRange 0 max_row (return 0) $ \acc i -> 
	 foldRange 0 max_col acc $ \acc j -> 
	   do cnt <- acc
	      p <- get pixel (fromIntegral i, fromIntegral j)
	      if p == max_depth
   	       then return (cnt + (i*max_col + j))
   	       else return cnt
#endif

       
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
	   []      -> runMandel 3 3 3   -- Should output 24.
	   [a,b,c] -> runMandel (read a) (read b) (read c)
