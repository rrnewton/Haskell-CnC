#!/usr/bin/env runhaskell

import System.Environment
import HSH
import Control.Monad
import Data.List

data LogEntry = Config String String Int -- bench sched threads
	      | Productivity Double
  deriving Show

parseLine (_: "Config" :_ : bench: _: _: _: sched : "threads": thr : _) = Config bench sched (read thr)
parseLine ("Productivity": percent: _) = 
    Productivity (read$ filter (not . (=='%')) percent)
parseLine line = error$ "Unmatched line: "++ unwords line


configp (Config _ _ _)  = True
configp _ = False

deProd (Productivity n) = n

print_group ls = 
  case head ls of 
    Config bench sched threads -> do
      putStrLn$ bench ++"  "++ sched ++"  "++ show threads ++"   "++
	        (concat$ intersperse " " $ map show $ sort $ map (deProd) $ tail ls)

schedEq (Config _ a _) (Config _ b _) = a == b
benchEq (Config a _ _) (Config b _ _) = a == b

(-<) f g = \x y -> f (g x) (g y)

main = do
 files <- getArgs
 if null files then print "No files given!" else return()
 forM_ files $ \file -> do
   lines <- run$ ("grep -E \"Config|Productivity\" "++file)
   let parsed = map parseLine $ map words lines
       grouped = groupBy (\ a b -> not$ configp b) parsed
--       deep = groupBy (schedEq) $ groupBy benchEq $ grouped
--       deep = groupBy (schedEq -< (head.head)) $ groupBy (benchEq -< head) $ grouped
       deep = groupBy (benchEq -< (head.head)) $ groupBy (schedEq -< head) $ grouped
   putStrLn "# Benchmark Scheduler Threads MutatorProductitivies..."
   putStrLn "# "
   --mapM_ print_group grouped
   forM_ deep $ \ bybench -> do
    forM_ bybench $ \ bysched -> do
      mapM_ print_group bysched
      putStrLn ""
    putStrLn ""
