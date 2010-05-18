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
import Intel.CncUtil
import Data.List

#include "haskell_cnc.h"

-- This step generates the bodies in the system.
genVectors vectors tag = 
    do put vectors tag (tag' * 1.0, tag' * 0.2, tag' * 30.0)
       where tag' = fromIntegral tag

-- This step computes the accelerations of the bodies.       
compute vectors accels n tag =
    do vecList <- sequence (List.map (get vectors) [1..n])
       vector <- get vectors tag
       put accels tag (accel vector vecList)
       where accel vector vecList = multTriple g $ sumTriples $ List.map (pairWiseAccel vector) vecList
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

-- This describes the graph-- The same tag collection prescribes the two step collections.             
run n = runGraph $  
        do tags    <- newTagCol
           vectors <- newItemCol
           accels  <- newItemCol
           prescribe tags (genVectors vectors)
           prescribe tags (compute vectors accels n)
           initialize $
               do sequence_ (List.map (putt tags) [1..n])
           finalize $ 
               do stepPutStr "Begin finalize action.\n"
		  vecList <- itemsToList vectors
                  accList <- itemsToList accels
                  return (vecList, accList)

main = 
    do args <- getArgs 
       let (vecList, accList) = case args of 
                                  []  -> run 3
				  [s] -> run (read s)
       --putStrLn $ show vecList; putStrLn $ show accList;
       -- Do a meaningless sum to generate a small output:
       --putStrLn $ show (foldl (\sum (_,(x,y,z)) -> sum + x+y+z) 0 vecList)
       --putStrLn $ show (foldl (\sum (_,(x,y,z)) -> sum + x+y+z) 0 accList)
       putStrLn $ show (foldl (\sum (_,(x,y,z)) -> if x>0.1 then sum+1 else sum) 0 vecList)
       putStrLn $ show (foldl (\sum (_,(x,y,z)) -> if x>0 then sum+1 else sum) 0 accList)

