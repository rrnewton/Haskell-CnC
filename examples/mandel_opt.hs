-- Author: Ryan Newton 

-- In this version we flatten our (x,y) positions into a single integer.
-- To that end, we FIX the grid size statically to 300x300.


-- In this improved version, we look at three different optimizations.
--   First: enable GMaps and pack 
--   Second: use cncFor to structure the spewing of the inital workload.
--   Third: do the work *inside* a cncFor and thereby drastically reduce the step count.

-- Set the environment variable MANDELOPT to {1,2,3} to add each of these optimizations.

import Data.Complex
import Data.Word
import System.Environment
import Control.Monad
import Data.Bits

-- #define MEMOIZE
#define USE_GMAP
#include <haskell_cnc.h>

-- Constants for this benchmark:
max_row = 300
max_col = 300

-- Here we manually pack our pairs into scalars.
-- In the future the ItemCol data type may do this for us auto-magically.
type Pair = (Word16, Word16)
pack   :: Pair -> Int
unpack :: Int -> Pair
pack (a,b) = shiftL (fromIntegral a) 16 + (fromIntegral b)
unpack n   = (fromIntegral$ shiftR n 16, fromIntegral$ n .&. 0xFFFF)

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0 0
  where   
   fn = magnitude
   loop i z count
    | i == max_depth = count
    | fn(z) >= 2.0   = count 
    | otherwise      = loop (i+1) (z*z + c) (count+1)

mandelProg :: Int -> Int -> GraphCode Int
mandelProg optlvl max_depth = 
    do position :: TagCol  Int                  <- newTagCol
       dat      :: ItemCol Int (Complex Double) <- newItemCol
       pixel    :: ItemCol Int Int              <- newItemCol
       
       let mandelStep tag = 
	    do cplx <- get dat tag
	       put pixel tag (mandel max_depth cplx)

       prescribe position mandelStep 

       let packit i j = (pack (_i,_j), z)
             where (_i,_j) = (fromIntegral i, fromIntegral j)
       	           z = (r_scale * (fromIntegral j) + r_origin) :+
       		       (c_scale * (fromIntegral i) + c_origin)

       let kernel i j = do let (packed,z) = packit i j 
 		       	   put  dat packed z
       	                   putt position packed

       let init1 = forM_ [0..max_row] $ \i -> 
                     forM_ [0..max_col] $ \j ->
  	               kernel i j 

       -- This version uses cncFor to structure the work spawning.
       let init2 = 
                 do stepPutStr "mandel_opt: Using cncFor implementation...\n"
                    cncFor2D (0,0) (max_row, max_col)  $ \ i j ->
		       kernel i j			  

       -- This version uses cncFor to do the actual work.
       let init3 = do stepPutStr "mandel_opt: Using even better cncFor method...\n"
       		      cncFor2D (0,0) (max_row, max_col)  $ \ i j ->
			 do let (packed,z) = packit i j
       	                    put pixel packed (mandel max_depth z)

       initialize $ 
        case optlvl of 
	 1 -> init1
	 2 -> init2 
	 3 -> init3

       -- Final result, count coordinates of the  pixels with a certain value:
       finalize $ 
	foldRange 0 max_row (return 0) $ \acc i -> 
	 foldRange 0 max_col acc $ \acc j -> 
	   do cnt <- acc
	      p <- get pixel (pack (fromIntegral i, fromIntegral j))
	      if p == max_depth
   	       then return (cnt + (i*max_col + j))
   	       else return cnt
       
   where 
    r_origin = -2                            :: Double
    r_scale  = 4.0 / (fromIntegral max_row)  :: Double
    c_origin = -2.0                          :: Double
    c_scale = 4.0 / (fromIntegral max_col)   :: Double


runMandel optlvl c = 
	do --env <- getEnvironment
	   --let optlvl = case Prelude.lookup "MANDELOPT" env of 
	   --		 Nothing -> 1
	   --		 Just n  -> read n
	   putStrLn$ "Running mandel with opt level: "++ show optlvl
	   let check = runGraph $ mandelProg optlvl c 
	   putStrLn ("Mandel check " ++ show check)

main = do args <- getArgs  
	  case args of
	   []     -> runMandel 1 3  
	   [a]    -> runMandel 1 (read a)
	   [o, a] -> runMandel (read o) (read a)
