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
  
import System.Environment
import Data.Int
import qualified Data.List as List
import qualified Data.Array as Array
import Intel.CncUtil
import Data.Array.IO
import Data.Array.MArray
import Debug.Trace

#include "haskell_cnc.h"

type MutableArray = IOUArray  (Int, Int) Float
type StaticArray = Array (Int, Int) Float

kCompute :: ItemCol Int Int -> TagCol Int -> Int -> StepCode ()
kCompute pItem controlS1 tag =
    do p <- get pItem 0
       for_ 0 p (putt controlS1) 

s1Compute :: ItemCol (Int, Int, Int) MutableArray -> ItemCol Int Int -> Int -> StepCode ()
s1Compute lkji bItem k = 
    do b <- get bItem 0
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
       
kjCompute :: ItemCol Int Int -> TagCol (Int, Int) -> Int -> StepCode ()    
kjCompute pItem controlS2 k =
    do p <- get pItem 0
       for_ (k + 1) p (\j -> putt controlS2 (k, j))
    
s2Compute :: ItemCol (Int, Int, Int) MutableArray -> ItemCol Int Int -> (Int, Int) -> StepCode ()    
s2Compute lkji bItem (k, j) =
    do b <- get bItem 0
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
                                                      
       
kjiCompute :: ItemCol Int Int -> TagCol (Int, Int, Int) -> (Int, Int) -> StepCode ()
kjiCompute pItem controlS3 (k, j) =
    do for_ (k + 1) (j + 1) (\i -> putt controlS3 (k, j, i))
    
s3Compute :: ItemCol (Int, Int, Int) MutableArray -> ItemCol Int Int -> (Int, Int, Int) -> StepCode ()    
s3Compute lkji bItem (k,j,i) | i == j =
    do b <- get bItem 0
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
       
s3Compute lkji bItem (k,j,i) | otherwise =
    do b <- get bItem 0
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
       return $ array ((0,0),(n-1,n-1)) (concat assocs)
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
           do pItem    <- newItemCol
              bItem    <- newItemCol
              lkji     <- newItemCol
              singleton <- newTagCol
              controlS1 <- newTagCol
              controlS2 <- newTagCol
              controlS3 <- newTagCol

              prescribe singleton (kCompute pItem controlS1)
              prescribe controlS1 (s1Compute lkji bItem)
              prescribe controlS1 (kjCompute pItem controlS2)
              prescribe controlS2 (s2Compute lkji bItem)
              prescribe controlS2 (kjiCompute pItem controlS3)
              prescribe controlS3 (s3Compute lkji bItem)

              initialize $
                 do putt singleton 1
                    put pItem 0 p
                    put bItem 0 b
                    initLkji arrA lkji n p b

              finalize $ 
                 do stepPutStr "Begin finalize action.\n"
                    composeResult lkji n p b


main = 
    do [n, b, fname] <- getArgs 
       arrA <- initMatrix (read n) fname
       putStrLn $ show $ arrA
       arrB <- return $ run (read n) (read b) arrA
       putStrLn $ show $ [((i,j),arrB Array.! (i,j)) | i <-[0..(read n)-1], j<-[0..i]]

initMatrix :: Int -> [Char] -> IO StaticArray
initMatrix n fname = 
    do fs <- readFile fname
       return $ listArray ((0,0), (n-1, n-1)) (List.map read (words fs))
	   
