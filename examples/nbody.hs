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

import GHC.Exts

import qualified Data.List as List
import qualified Data.Array as Array

#include "haskell_cnc.h"

type Float3D = (Float, Float, Float)

type UFloat3D = (# Float#, Float#, Float# #)


-- This step generates the bodies in the system.
genVector tag = (tag' * 1.0, tag' * 0.2, tag' * 30.0)
   where tag' = fromIntegral tag

-- Only doing the O(N^2) part in parallel:
-- This step computes the accelerations of the bodies.       
compute vecList accels tag =
    do --let myvector = vecList !! (tag-1)
       let myvector = vecList Array.! (tag-1)
       put accels tag (accel myvector vecList)
       --put accels tag myvector
       return ()
       where --vecList = elems vecArr
             g = 9.8

--             multTriple :: Float# -> UFloat3D -> UFloat3D
--             multTriple c (# x,y,z #) = (# c*x,c*y,c*z #)

             multTriple :: Float -> Float3D -> Float3D
             multTriple c ( x,y,z ) = ( c*x,c*y,c*z )

	     pairWiseAccel :: Float3D -> Float3D -> Float3D
             pairWiseAccel (x,y,z) (x',y',z') = let dx = x'-x
                                                    dy = y'-y
                                                    dz = z'-z
                                                    eps = 0.005
                                                    distanceSq = dx^2 + dy^2 + dz^2 + eps
                                                    factor = 1/sqrt(distanceSq ^ 3)
--                                                in multTriple factor (dx,dy,dz)
                                                in multTriple factor (dx,dy,dz)

#if 0
             sumTriples = foldr (\(x,y,z) (x',y',z') -> (x+x',y+y',z+z')) (0,0,0)
	     accel vector vecList = multTriple g $ sumTriples $ List.map (pairWiseAccel vector) vecList
#else
-- Making this much leCss haskell like to avoid allocation:
             (strt,end) = Array.bounds vecList
             addTriples :: Float3D -> Float3D -> Float3D
             addTriples (x,y,z) (x',y',z') = (x+x',y+y',z+z')

	     accel vector vecList = 

             -- Manually inlining to see if the tuples unbox:
	        let (sx,sy,sz) =
                     foldRange strt (end+1) (0,0,0) $ \ (ax,ay,az) i -> 
--		       let (px,py,pz) = pairWiseAccel vector (vecList Array.! i)

                       let (x,y,z)    = vector
			   (x',y',z') = (vecList Array.! i)
			   (px,py,pz) = let dx = x'-x
                                            dy = y'-y
                                            dz = z'-z
                                            eps = 0.005
                                            distanceSq = dx^2 + dy^2 + dz^2 + eps
					    factor = 1/sqrt(distanceSq ^ 3)
					in (factor * dx, factor * dy, factor *dz)

		       in (ax+px, ay+py, az+pz)
		in (g*sx, g*sy, g*sz)




                --------------------------------------------------------------------------------
		-- multTriple g $ 			   
	        -- List.foldl' (\ acc i ->                           
		-- 	      addTriples acc (pairWiseAccel vector (vecList Array.! i))
		-- 	     )
	  	--         (0,0,0) [strt .. end]

-- multTriple g $ sumTriples $ List.map (pairWiseAccel vector) vecList

#endif


-- This describes the graph-- The same tag collection prescribes the two step collections.             
--run :: Int -> (b, c)
--run :: Int -> ([(Int, (Float, Float, Float))], [(Int, (Float, Float, Float))])
--run :: Int -> ([Float3D], [Float3D])
run :: Int -> [Float3D]
run n = runGraph $  
        do tags    <- newTagCol
           accels  <- newItemCol

           let initVecs = Array.array (0,n-1) [ (i, genVector i) | i <- [0..n-1] ]
           --let initVecs = List.map genVector [1..n]

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

