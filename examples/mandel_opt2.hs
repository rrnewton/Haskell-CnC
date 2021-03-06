-- Author: Ryan Newton 

-- In this version we flatten our (x,y) positions into a single integer.

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

-- Here we manually pack our pairs into scalars.
-- In the future the ItemCol data type may do this for us auto-magically.
type Pair = (Word16, Word16)
pack   :: Pair -> Int
unpack :: Int -> Pair
pack (a,b) = shiftL (fromIntegral a) 16 + (fromIntegral b)
unpack n   = (fromIntegral$ shiftR n 16, fromIntegral$ n .&. 0xFFFF)

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0 
  where   
   fn = magnitude
   loop i z 
    | i == max_depth = i
    | fn(z) >= 2.0   = i
    | otherwise      = loop (i+1) (z*z + c) 

dynAPI = True

mandelProg :: Int -> Int -> Int -> Int -> GraphCode Int
mandelProg optlvl max_row max_col max_depth = 
    do 
       pixel    :: ItemCol Int Int              <- newItemCol
       
       let mandelStep tag = 
            let (i,j) = unpack tag
       	        z = (r_scale * (fromIntegral j) + r_origin) :+
       	   	    (c_scale * (fromIntegral i) + c_origin)
            in put pixel tag (mandel max_depth z)

       position <- prescribeNT [mandelStep]

       let push i j = 
	       if dynAPI 
	       then forkStep$ mandelStep (pack (fromIntegral i, fromIntegral j))
	       else putt position $ pack (fromIntegral i, fromIntegral j)

	   init1 = forM_ [0..max_row] $ \i -> 
                     forM_ [0..max_col] $ \j ->
  	               push i j

       -- This version uses cncFor to structure the work spawning.
       let init2 = 
                 do stepPutStr "mandel_opt: Using cncFor implementation...\n"
                    cncFor2D (0,0) (max_row, max_col)  $ \ i j ->
		       push i j

       -- This version uses cncFor to do the actual work.
       let init3 = do stepPutStr "mandel_opt: Using even better cncFor method...\n"
       		      cncFor2D (0,0) (max_row, max_col)  $ \ i j ->
			 mandelStep $ pack (fromIntegral i, fromIntegral j)

       initialize $ 
        case optlvl of 
	 1 -> init1
	 2 -> init2 
	 3 -> init3

       -- Final result, count coordinates of the  pixels with a certain value:
       finalize $ 
	foldM (\acc i -> 
          foldM (\acc j -> 
	           do p <- get pixel (pack (fromIntegral i, fromIntegral j))
		      if p == max_depth
   		       then return (acc + (i*max_col + j))
   		       else return acc)
	        acc [0..max_col]
              ) 0 [0..max_row] 
       
   where 
    r_origin = -2                            :: Double
    r_scale  = 4.0 / (fromIntegral max_row)  :: Double
    c_origin = -2.0                          :: Double
    c_scale = 4.0 / (fromIntegral max_col)   :: Double


runMandel optlvl a b c = 
	do putStrLn$ "Running mandel with opt level: "++ show optlvl
	   let check = runGraph $ mandelProg optlvl a b c 
	   putStrLn ("Mandel check " ++ show check)

main = do args <- getArgs  
	  case args of
	   []         -> runMandel 1 3 3 3   -- Should output 24.
	   [o, a,b,c] -> runMandel (read o) (read a) (read b) (read c)
