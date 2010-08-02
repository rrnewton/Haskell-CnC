{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

----------------------------------------------------------------------------------------------------
-- This is the code generator for Haskell CnC itself.
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

module Intel.Cnc.Spec.Codegen.Haskell where

import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.CncGraph
import Intel.Cnc.Spec.GatherGraph
import Intel.Cnc.Spec.Util 
import Control.Monad.State
import StringTable.Atom
import Data.Maybe
import Text.PrettyPrint.HughesPJClass

import qualified StringTable.AtomMap as AM
import qualified StringTable.AtomSet as AS

emitHaskell :: StringBuilder m => CncSpec -> m ()
emitHaskell (spec @ CncSpec{..}) = do
  putS$ "\n-- This code was GENERATED from a CnC specification, do not modify.\n\n"
  

{-

import Data.Complex
import Data.Int
import System.Environment
import Control.Monad

-- #define USE_GMAP
-- #define MEMOIZE
#include <haskell_cnc.h>

type Pair = (Int16, Int16)

mandelProg :: Int -> Int -> Int -> GraphCode Int
mandelProg max_row max_col max_depth = 
    do dat      :: ItemCol Pair (Complex Double) <- newItemCol
       pixel    :: ItemCol Pair Int              <- newItemCol
       
       let mandelStep tag = 
	    do tid <- stepUnsafeIO myThreadId
	       --stepPutStr$ "["++ show tid ++"] Mandel Step executing: "++ show tag ++ "\n"
	       cplx <- get dat tag
	       put pixel tag (mandel max_depth cplx)

       position :: TagCol  Pair <- prescribeNT [mandelStep] 

       initialize $ 
        forM_ [0..max_row-1] $ \i -> 
         forM_ [0..max_col-1] $ \j ->
          let (_i,_j) = (fromIntegral i, fromIntegral j)
	      z = (r_scale * (fromIntegral j) + r_origin) :+ 
  		  (c_scale * (fromIntegral i) + c_origin) in
	  do put dat (_i,_j) z
	     putt position (_i,_j)

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
	   []      -> runMandel 1 1 3   -- Should output 57.
	   []      -> runMandel 4 4 3   -- Should output 57.
	   [a,b,c] -> runMandel (read a) (read b) (read c)
-}



{-
-- Here's how it might look refactored:

module Mandel 

import MandelBase

mandelStep MandelContext{..} tag = 
 do tid <- stepUnsafeIO myThreadId
    cplx <- get dat tag
    put pixel tag (mandel max_depth cplx)

mandel :: Int -> Complex Double -> Int
mandel max_depth c = loop 0 0 0
  where   
   fn = magnitude
   loop i z count
    | i == max_depth = count
    | fn(z) >= 2.0   = count 
    | otherwise      = loop (i+1) (z*z + c) (count+1)


initStep MandelContext{..} =
        forM_ [0..max_row-1] $ \i -> 
         forM_ [0..max_col-1] $ \j ->
          let (_i,_j) = (fromIntegral i, fromIntegral j)
	      z = (r_scale * (fromIntegral j) + r_origin) :+ 
  		  (c_scale * (fromIntegral i) + c_origin) in
	  do put dat (_i,_j) z
	     putt position (_i,_j)
  where
    r_origin = -2                            :: Double
    r_scale  = 4.0 / (fromIntegral max_row)  :: Double
    c_origin = -2.0                          :: Double
    c_scale = 4.0 / (fromIntegral max_col)   :: Double


finalizeStep MandelContext{..} =
     do foldM (\acc i -> 
          foldM (\acc j -> 
	           do --stepPutStr$ " ... try get pixel "++ show (i,j) ++"\n "
		      p <- get pixel (fromIntegral i, fromIntegral j)
		      --stepPutStr$ " GET PIXEL SUCCESSFUL "++ show (i,j) ++"\n "
		      if p == max_depth
   		       then return (acc + (i*max_col + j))
   		       else return acc)
	        acc [0..max_col-1]
              ) 0 [0..max_row-1] 

runMandel a b c = 
	   let check = runGraph $ mandelGraph a b c in
	   putStrLn ("Mandel check " ++ show check)

main = do args <- getArgs  
	  case args of
	   []      -> runMandel 1 1 3   -- Should output 57.
	   []      -> runMandel 4 4 3   -- Should output 57.
	   [a,b,c] -> runMandel (read a) (read b) (read c)



----------------------------------------------------------------------------------------------------

type Pair = (Int16, Int16)

-- 
mandelStep


mandelGraph max_row max_col max_depth = 
    do dat      :: ItemCol Pair (Complex Double) <- newItemCol
       pixel    :: ItemCol Pair Int              <- newItemCol
       
       position :: TagCol  Pair <- prescribeNT [mandelStep] 


       -- Final result, count coordinates of the  pixels with a certain value:
       
   where 



-}