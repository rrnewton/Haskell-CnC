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

-- This program uses CnC to calculate the accelerations of the bodies in a 3D system.  
  
import System.Environment
import Data.Int
import qualified Data.List as List
import qualified Data.Array as Array

#include "haskell_cnc.h"

-- This step generates the bodies in the system.
genVector tag = (tag' * 1.0, tag' * 0.2, tag' * 30.0)
   where tag' = fromIntegral tag

-- Only doing the O(N^2) part in parallel:
-- This step computes the accelerations of the bodies.       
compute vecList accels tag =
    do let myvector = vecList !! (tag-1)
       put accels tag (accel myvector vecList)
       where --vecList = elems vecArr
	     accel vector vecList = multTriple g $ sumTriples $ List.map (pairWiseAccel vector) vecList
             pairWiseAccel (x,y,z) (x',y',z') = let dx = x'-x
                                                    dy = y'-y
                                                    dz = z'-z
                                                    eps = 0.005
                                                    distanceSq = dx^2 + dy^2 + dz^2 + eps
                                                    factor = 1/sqrt(distanceSq ^ 3)
                                                in multTriple factor (dx,dy,dz)
             sumTriples = foldr (\(x,y,z) (x',y',z') -> (x+x',y+y',z+z')) (0,0,0)
             multTriple c (x,y,z) = (c*x,c*y,c*z)
             g = 9.8


type Float3D = (Float, Float, Float)

-- This describes the graph-- The same tag collection prescribes the two step collections.             
--run :: Int -> (b, c)
--run :: Int -> ([(Int, (Float, Float, Float))], [(Int, (Float, Float, Float))])
--run :: Int -> ([Float3D], [Float3D])
run :: Int -> [Float3D]
run n = runGraph $  
        do tags    <- newTagCol
           accels  <- newItemCol

           --let initArr = array (0,n-1) [ (i, genVector i) | i <- [0..n-1] ]
           let initVecs = List.map genVector [1..n]

           prescribe tags (compute initVecs accels)

           initialize $
               do sequence_ (List.map (putt tags) [1..n])

           finalize $ 
               do stepPutStr "Begin finalize action.\n"
		  accList <- sequence (List.map (get accels) [1..n])
                  return accList 

main = 
    do args <- getArgs 
       let accList = case args of 
                      []  -> run (3::Int)
		      [s] -> run (read s)
       --putStrLn $ show accList;
       -- Do a meaningless sum to generate a small output:
       --putStrLn $ show (foldl (\sum (_,(x,y,z)) -> sum + x+y+z) 0 accList)
       putStrLn $ show (foldl (\sum (x,y,z) -> if x>0 then sum+1 else sum) 0 accList)

