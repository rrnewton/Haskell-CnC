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
{-# LANGUAGE ExistentialQuantification
   , ScopedTypeVariables
   , BangPatterns
   , NamedFieldPuns 
   , RecordWildCards
   , FlexibleInstances
   , DeriveDataTypeable
  #-}

-- Author: Chih-Ping Chen

-- This program uses CnC to do cholesky transformation.  

-- Description
-- -----------
-- Given a symmetric positive definite matrix A, the Cholesky decomposition is
-- a lower triangular matrix L such that A=L.L^(T). 

-- Cholesky has six steps (k_compute), (kj_compute), (kji_compute), (S1_compute),
-- (S2_compute) and (S3_compute).  (k_compute), (kj_compute) and (kji_compute)
-- take in 'p' as input which is n/b, where 'n' is the matrix size, 
-- 'b' is block/tile size. Both 'n' and 'b' are command line arguments.
-- They produce loop indices <tags> for the execution of the other steps. 

-- (S1_compute) performs unblocked cholesky factorization of the input tile
-- [Lkji:k,k,k], which is an input from the environment. It produces output
-- item [Lkji:k,k,k+1] which is one of the inputs to (S2_compute).
-- This step performs triangular system solve, also takes in as input
-- [Lkji:j,k,k] and ouputs [Lkji:j,k,k+1].  (S3_compute) performs a symmetric
-- rank-k update and takes in as input [Lkji:j,i,k] along with [Lkji:j,k,k+1] 
-- if its a diagonal tile. If its not a diagonal tile, it also takes in
-- [Lkji:i,k,k+1] as input. It produces [Lkji:j,i,k+1] as output. 

-- The constraints on parallelism for this kernel are as follows: a [Lkji]
-- instance may not be processed by a compute step until the [Lkji] instance
-- and the associated controlling tags <control_S1>, <control_S2> 
-- or <control_S3> have been produced. For example, one of the inputs to
-- the step (S2_compute) is computed by step (S1_compute) and the loop indices
-- are tags <control_S2> which are generated by step (kj_compute). So unless these
-- steps execute and produce the ouput, step (S2_compute) cannot execute.

-- The constraints on the algorithm are as follows: the input matrix should be
-- a symmetric positive definite (SPD) matrix. Otherwise Cholesky factorization
-- cannot be performed. 


-- Usage
-- -----

-- The command line is:

-- cholesky [-v] n b filename [number_of_threads]
-- 	-v :	verbose option
-- 	 n :	input SPD matrix size
-- 	 b : 	block/tile size
-- 	 filename: input matrix file name
-- 	 number_of_threads : optional argument for specifying the number of
-- 	                     threads to run the application.

-- Several sample input files are provided. m6.in is a 6x6 matrix (n=6).
-- Input_matrix.zip contains the files m50.in, m100.in, m500.in, and m1000.in with
-- corresponding 'n' of 50, 100, 500, and 1000.

-- e.g.
-- cholesky 1000 50 m1000.in 4
-- cholesky -v 6 2 m6.in

-- The input SPD matrix is read from the file specified. The output will be a
-- lower triangular matrix written to a text file cholesky_out if -v is specified. 

  
import System.Environment
import Data.Int
import qualified Data.List as List
import qualified Data.Array as Array
-- import Intel.CncUtil
import Data.Array.IO
import Data.Array.MArray
import Debug.Trace

#include "haskell_cnc.h"

type MutableArray = IOUArray  (Int, Int) Float
type StaticArray = Array.Array (Int, Int) Float

--kCompute :: ItemCol Int Int -> TagCol Int -> Int -> StepCode ()
kCompute :: Int -> TagCol Int -> Int -> StepCode ()
kCompute p controlS1 tag =
    for_ 0 p (putt controlS1) 

--s1Compute :: ItemCol (Int, Int, Int) MutableArray -> ItemCol Int Int -> Int -> StepCode ()
s1Compute :: ItemCol (Int, Int, Int) MutableArray -> Int -> Int -> StepCode ()
s1Compute lkji b k = 
    do 
       aBlock <- get lkji (k, k, k)
       put lkji (k, k, k+1) (s1Core aBlock b)
    where s1Core aBlock b = unsafePerformIO $ 
                            do lBlock <- newArray ((0,0), (b-1,b-1)) 0.0
                               forM_ [0..b-1] (outer aBlock lBlock b)
                               return lBlock
          outer aBlock lBlock b kb = do base <- readArray aBlock (kb,kb)
                                        writeArray lBlock (kb,kb) (sqrt base)
                                        forM_ [kb+1 .. b-1] (inner1 aBlock lBlock kb)
                                        forM_ [kb+1 .. b-1] (inner2 aBlock lBlock kb b)
          inner1 aBlock lBlock kb jb = do base1 <- readArray aBlock (jb,kb)
                                          base2 <- readArray lBlock (kb,kb)
                                          writeArray lBlock (jb,kb) (base1 /base2)
          inner2 aBlock lBlock kb b jbb = do forM_ [kb+1 .. b-1] (inner3 aBlock lBlock jbb kb)
          inner3 aBlock lBlock jbb kb ib = do base1 <- readArray aBlock (ib, jbb)
                                              base2 <- readArray lBlock (ib, kb)
                                              base3 <- readArray lBlock (jbb, kb)
                                              writeArray aBlock (ib,jbb) (base1 - base2 * base3)
       
--kjCompute :: ItemCol Int Int -> TagCol (Int, Int) -> Int -> StepCode ()    
kjCompute :: Int -> TagCol (Int, Int) -> Int -> StepCode ()    
kjCompute p controlS2 k =
       for_ (k + 1) p (\j -> putt controlS2 (k, j))
    
s2Compute :: ItemCol (Int, Int, Int) MutableArray -> Int -> (Int, Int) -> StepCode ()    
s2Compute lkji b (k, j) =
    do 
       aBlock <- get lkji (j,k,k)
       liBlock <- get lkji (k,k,k+1)
       put lkji (j,k,k+1) (s2Core aBlock liBlock b)
    where s2Core aBlock liBlock b = unsafePerformIO $
                                    do loBlock <- newArray ((0,0),(b-1,b-1)) 0.0
                                       forM_ [0..b-1] (outer aBlock liBlock loBlock b)
                                       return loBlock
          outer aBlock liBlock loBlock b kb = do forM_ [0..b-1] (inner1 aBlock liBlock loBlock kb)
                                                 forM_ [kb+1..b-1] (inner2 aBlock liBlock loBlock b kb)
          inner1 aBlock liBlock loBlock kb ib = do base1 <- readArray aBlock (ib,kb)
                                                   base2 <- readArray liBlock (kb,kb)
                                                   writeArray loBlock (ib,kb) (base1 / base2)
          inner2 aBlock liBlock loBlock b kb jb = do forM_ [0..b-1] (inner3 aBlock liBlock loBlock kb jb)
          inner3 aBlock liBlock loBlock kb jb ib = do base1 <- readArray aBlock (ib,jb)
                                                      base2 <- readArray liBlock (jb,kb)
                                                      base3 <- readArray loBlock (ib,kb)
                                                      writeArray aBlock (ib,jb) (base1 - (base2 * base3))
                                                      
       
--kjiCompute :: ItemCol Int Int -> TagCol (Int, Int, Int) -> (Int, Int) -> StepCode ()
kjiCompute :: TagCol (Int, Int, Int) -> (Int, Int) -> StepCode ()
kjiCompute  controlS3 (k, j) =
    do for_ (k + 1) (j + 1) (\i -> putt controlS3 (k, j, i))
    
s3Compute :: ItemCol (Int, Int, Int) MutableArray -> Int -> (Int, Int, Int) -> StepCode ()    
s3Compute lkji b (k,j,i) | i == j =
    do 
       aBlock <- get lkji (j,i,k)
       l2Block <- get lkji (j,k,k+1)
       put lkji (j,i,k+1) (s3Core aBlock l2Block b)
    where s3Core aBlock l2Block b = unsafePerformIO $
                                    do forM_ [0..b-1] (outer aBlock l2Block b)
                                       return aBlock
          outer aBlock l2Block b jb = do forM_ [0..b-1] (inner1 aBlock l2Block b jb)
          inner1 aBlock l2Block b jb kb = do base <- readArray l2Block (jb,kb) 
                                             forM_ [jb..b-1] (inner2 aBlock l2Block jb kb (-base))
          inner2 aBlock l2Block jb kb temp ib = do base1 <- readArray aBlock (ib,jb)
                                                   base2 <- readArray l2Block (ib,kb)
                                                   writeArray aBlock (ib,jb) (base1 + temp * base2)
       
s3Compute lkji b (k,j,i) | otherwise =
    do 
       aBlock <- get lkji (j,i,k)
       l1Block <- get lkji (i,k,k+1)
       l2Block <- get lkji (j,k,k+1)
       put lkji (j,i,k+1) (s3Core aBlock l1Block l2Block b)
    where s3Core aBlock l1Block l2Block b = unsafePerformIO $
                                            do forM_ [0..b-1] (outer aBlock l1Block l2Block b)
                                               return aBlock
          outer aBlock l1Block l2Block b jb = do forM_ [0..b-1] (inner1 aBlock l1Block l2Block b jb)
          inner1 aBlock l1Block l2Block b jb kb = do base <- readArray l2Block (jb,kb) 
                                                     forM_ [0..b-1] (inner2 aBlock l1Block jb kb (-base))
          inner2 aBlock l1Block jb kb temp ib = do base1 <- readArray aBlock (ib,jb)
                                                   base2 <- readArray l1Block (ib,kb)
                                                   writeArray aBlock (ib,jb) (base1 + temp * base2)

initLkji :: StaticArray -> ItemCol (Int, Int, Int) MutableArray -> Int -> Int -> Int -> StepCode ()    
initLkji arrA lkji n p b = 
    let buildTiles = [ put lkji (i, j, 0) (tile i j) | i <- [0..p-1], j <- [0..p-1] ]
        tile i j = unsafePerformIO $ newListArray ((0,0),(b-1,b-1)) (tileList i j)
        tileList i j = [ arrA Array.! (i * b + ii, j * b + jj) | ii <- [0..b-1], jj <-[0..b-1]]
    in sequence_ buildTiles   
    
    
composeResult :: ItemCol (Int, Int, Int) MutableArray -> Int -> Int -> Int -> StepCode StaticArray    
composeResult lkji n p b =
    do assocs <- sequence [ grab i ib j | i <- [0..p-1], ib <- [0..b-1], j <- [0..i]]
       return $ Array.array ((0,0),(n-1,n-1)) (concat assocs)
    where grab i ib j = if (i == j) then
                           do matOut <- get lkji (i,j,j+1)   
                              compose1 matOut 
                        else
                           do matOut <- get lkji (i,j,j+1)
                              compose2 matOut
                        where compose1 matOut = do forM [0..ib] (compose11 matOut)
                              compose11 matOut jb = let elem = unsafePerformIO $
                                                               do readArray matOut (ib,jb)
                                                    in return ((i*b+ib,j*b+jb),elem)
                              compose2 matOut = do forM [0..b-1] (compose11 matOut)   

run :: Int -> Int -> StaticArray -> StaticArray
run n b arrA = 
        let p = n `div` b
        in
           runGraph $  
           do 
              lkji     <- newItemCol
              singleton <- newTagCol
              controlS1 <- newTagCol
              controlS2 <- newTagCol
              controlS3 <- newTagCol

              prescribe singleton (kCompute p controlS1)
              prescribe controlS1 (s1Compute lkji b)
              prescribe controlS1 (kjCompute p controlS2)
              prescribe controlS2 (s2Compute lkji b)
              prescribe controlS2 (kjiCompute controlS3)
              prescribe controlS3 (s3Compute lkji b)

              initialize $
                 do putt singleton 1
                    initLkji arrA lkji n p b

              finalize $ 
                 do stepPutStr "Begin finalize action.\n"
                    composeResult lkji n p b


main = 
    do ls <- getArgs 
       let [n, b, fname] = 
            case ls of 
-- cholesky 1000 50 m1000.in 4
-- cholesky -v 6 2 m6.in
              []      -> ["6", "2", "cholesky_m6.in"]
              []      -> ["1000", "50", "m1000.in"]
	      [_,_,_] -> ls
       arrA <- initMatrix (read n) fname
       putStrLn $ show $ arrA
       arrB <- return $ run (read n) (read b) arrA
       putStrLn $ show $ [((i,j),arrB Array.! (i,j)) | i <-[0..(read n)-1], j<-[0..i]]

initMatrix :: Int -> [Char] -> IO StaticArray
initMatrix n fname = 
    do fs <- readFile fname
       return $ Array.listArray ((0,0), (n-1, n-1)) (List.map read (words fs))
	   
