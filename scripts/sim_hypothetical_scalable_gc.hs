#!/usr/bin/env runhaskell

-- A quick and dirty script to zip together a .dat and .gc and simulate perfectly scalable GC


import System.Environment
import System.FilePath
import System.IO
import HSH
import Control.Monad
import Data.List
import Data.Function
import Data.Char
import qualified Data.Map as M

import Debug.Trace

-- nbody.exe io 7 31 0 8.61 8.92 8.94
-- nbody  7  16   75.7 76.0 76.7

data LogEntry = Config String String Int -- bench sched threads
	      | Productivity Double
  deriving Show



fst3 (a,b,c) = a
fstNsnd (a,b,c) = (a,b)
fourth (a,b,c,d) = d

trim      :: String -> String
trim      = f . f
   where f = reverse . dropWhile isSpace

isInteger s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False
 
isDouble s = case reads s :: [(Double, String)] of
  [(_, "")] -> True
  _         -> False
 
isNumeric :: String -> Bool
isNumeric s = isInteger s || isDouble s


parseDat x@[bench, variant, sched, threads, _, min, med, max] =  
    ((dropExtension bench, sched, threads), x)
parseDat x = error$ "parseDat unmatched : "++ show x

-- INPUT TOLERANT:
-- If med/max is missing, take what's there:
parseGc x@[bench, sched, threads, prod] = 
    [((dropExtension bench, sched, threads), 
     (bench, sched, threads, read prod :: Double))]

parseGc x@(bench: sched: threads: min: med: _) = 
    [((dropExtension bench, sched, threads), 
      (bench, sched, threads, read med :: Double))]

parseGc x = []
--parseGc x = error$ "parseGc unmatched : "++ show x

----------------------------------------------------------------------------------------------------

-- Munging around in text

-- comb bestprod left
--      (_ben: _sch: _thr: _min: productivity : _)
--    = comb bestprod left [_ben, _sch, _thr, productivity]
-- tolerate missing med/max on some lines:

comb bestmap  [bench, variant, sched, threads, hsh, min, med, max] 
--     [_ben, _sch, _thr, productivity]
     (_ben, _sch, _thr, productivity)
   = [bench, variant, sched, threads, hsh, scale min, scale med, scale max
      -- Including extra fields for debugging:
     , show productivity, show best
     ]
 where 
  best = case M.lookup (dropExtension bench,sched) bestmap of
  	  Just x -> x
          Nothing -> error$ "Could not find entry in map: " ++ show (bench,sched)
  invbestprod = 100.0 / best

--  scale time = time
  scale "ERR" = "ERR"
  scale time | not (isNumeric time) = error$ "scale: Not numeric time: " ++ time
  scale time = show $ (read time) * (productivity / 100.0) * invbestprod

comb bestprod  x y = error$ "comb unmatched \n" ++ show x ++"\n" ++ show y


-- Extract out the bits that will order it nicely:

ord_relevant [bench, variant, sched, threads, hsh, min, med, max] = 
--    trace ("parsing " ++ threads ) $
    (bench, variant, sched, read threads :: Int)
--    (bench, variant, sched, threads)

----------------------------------------------------------------------------------------------------

isComment = (== '#') . head . trim

strip = map words .
	filter (not . isComment) .
	filter (not . null . trim) .
	lines

main = do 
 args <- getArgs
 let [_dat,_gc] = 
      case args of 
	 [_dat,_gc] -> args
         [one] -> [dropExtension one `addExtension` "dat",
		   dropExtension one `addExtension` "log.gc"]
 dat <- readFile _dat
 gc  <- readFile  _gc
 let lsd = map parseDat $ strip dat
     lsg = concat$ map parseGc  $ strip gc
     dats = M.fromList lsd
     gcs  = M.fromList lsg

     -- Group by (bench,sched), but not threads:
     grouped = groupBy ((==) `on` (fstNsnd . fst)) lsg

     -- Indexed by (bench,sched)
     bests = map (\ ls -> 
--		  foldl1 min $ map (fourth.snd) ls
		  (fstNsnd$ fst $ head ls,
		   foldl1 max $ map (fourth.snd) ls)
		 ) grouped

     both = M.toList $
	    M.intersectionWith (comb$ M.fromList bests) dats gcs

 hPutStrLn stderr$ ".dat file size " ++ show (M.size dats)
 hPutStrLn stderr$ ".gc  file size " ++ show (M.size gcs)
 
 hPutStrLn stderr$ "\nDifference dat-gc: \n================================================================================" 
 mapM_ (hPrint stderr) (M.toList $ M.difference dats gcs)
 hPutStrLn stderr$ "\nDifference gc-dat: \n================================================================================"
 mapM_ (hPrint stderr) (M.toList $ M.difference gcs dats)

 hPutStrLn stderr$ "\n================================================================================"

-- mapM_ (putStrLn . unwords . snd) $ both

 mapM_ (\ (_,[bench, variant, sched, threads, hsh, min, med, max, productivity, best]) -> do
          putStrLn$ "   # Normalizing to best "++best++" given productivity: "++ productivity
          putStrLn$ unwords [bench, variant, sched, threads, hsh, min, med, max]
	) both

-- mapM_ print grouped
