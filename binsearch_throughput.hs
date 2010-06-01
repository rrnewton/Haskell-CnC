#!/usr/bin/env runhaskell


-- ---------------------------------------------------------------------------
--  Intel Concurrent Collections for Haskell
--  Copyright (c) 2010, Intel Corporation.
-- 
--  This program is free software; you can redistribute it and/or modify it
--  under the terms and conditions of the GNU Lesser General Public License,
--  version 2.1, as published by the Free Software Foundation.
-- 
--  This program is distributed in the hope it will be useful, but WITHOUT
--  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
--  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
--  more details.
-- 
--  You should have received a copy of the GNU Lesser General Public License along with
--  this program; if not, write to the Free Software Foundation, Inc., 
--  51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
-- ---------------------------------------------------------------------------


-- This is a script used for timing the throughput of benchmarks that
-- take one argument and have linear complexity.

-- It does a binary search over 

import System
import System.Cmd
import System.Exit
import Data.Time.Clock -- Not in 6.10
import Data.List

-- In seconds:
desired_exec_length = 3
trials = 1

ghc_default_flags = ["-O2"]
ghc_default_rts   = ["-qa"]

--putStrLn "Hello world"

main = 
 do args <- getArgs 
    if null args then error "Usage: binsearch_throughput <benchmarkfile.exe>" else return ()
    putStrLn$ "Searching input sizes for: " ++ show args
    loop args trials 1



loop args 0 n = return ()
loop args trial n | n > 2 ^ 100 = error "This executable doesn't seem to scale in proportion to its first argument."
loop args@(file:extra) trial n = do 
		 putStr$ show n ++ " "
		 --time <- timeit$ rawSystem file [show n] 
		 time <- timeit$ system$ file ++" "++ show n ++(concat $ intersperse " " extra)++" > /dev/null"
		 putStrLn$ "Time consumed: "++ show time
		 let rate = fromIntegral n / time
		 let guess = round$ desired_exec_length * rate
		 if time >= desired_exec_length
		  then do putStrLn$ "  TRIAL: "++show trial++" rate: "++ show (round rate) ++ " seconds: "++show time++"\n"
		          loop (file:extra) (trial-1) guess
		  else if time < 0.100 
		  then loop args trial (2*n)
		  else do putStrLn$ "\n Estimated rate to be "++show (round$ rate)++
				    " per second.  Trying to scale up:\n"
			  loop args trial guess
				  


timeit io = 
    do strt <- getCurrentTime
       code <- io       
       end  <- getCurrentTime
       case code of 
         ExitSuccess -> return ()
	 ExitFailure x -> error$ "Child process failed with exit code "++ show x
       return (diffUTCTime end strt)

	      
--    exitWith ExitSuccess

