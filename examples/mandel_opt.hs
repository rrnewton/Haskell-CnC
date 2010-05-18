-- Author: Ryan Newton 

-- In this version we flatten our (x,y) positions into a single integer.
-- To that end, we FIX the grid size statically to 300x300.

import Data.Complex
import Data.Word
import System.Environment

-- #define MEMOIZE
#include <haskell_cnc.h>

max_row = 300
max_col = 300

compress (r,c) = r * max_row + c
decompress n = (r,c)
  where (r,c) = n `quotRem` max_row


mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0 0
  where   
   fn = magnitude
   loop i z count
    | i == max_depth = count
    | fn(z) >= 2.0   = count 
    | otherwise      = loop (i+1) (z*z + c) (count+1)

mandelProg :: Int -> GraphCode Int
mandelProg max_depth = 
    do position :: TagCol  Int                  <- newTagCol
       dat      :: ItemCol Int (Complex Double) <- newItemCol
       pixel    :: ItemCol Int Int              <- newItemCol
       
       let mandelStep tag = 
	    do cplx <- get dat tag
	       put pixel tag (mandel max_depth cplx)

       prescribe position mandelStep 

       initialize $ 
        for_ 0 max_row $ \i -> 
         for_ 0 max_col $ \j ->
          let (_i,_j) = (fromIntegral i, fromIntegral j)
	      z = (r_scale * (fromIntegral j) + r_origin) :+ 
  		  (c_scale * (fromIntegral i) + c_origin) 
	      cmprsd = compress (_i,_j)
	  in
	  do put dat cmprsd z
	     putt position cmprsd

       -- Final result, count coordinates of the  pixels with a certain value:
       finalize $ 
	foldRange 0 max_row (return 0) $ \acc i -> 
	 foldRange 0 max_col acc $ \acc j -> 
	   do cnt <- acc
	      p <- get pixel (compress (fromIntegral i, fromIntegral j))
	      if p == max_depth
   	       then return (cnt + (i*max_col + j))
   	       else return cnt
       
   where 
    r_origin = -2                            :: Double
    r_scale  = 4.0 / (fromIntegral max_row)  :: Double
    c_origin = -2.0                          :: Double
    c_scale = 4.0 / (fromIntegral max_col)   :: Double


runMandel  c = 
	   let check = runGraph $ mandelProg c in
	   putStrLn ("Mandel check " ++ show check)

main = do args <- getArgs  
	  case args of
	   []  -> runMandel 3  
	   [a] -> runMandel (read a)
