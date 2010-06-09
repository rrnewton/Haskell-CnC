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
import System.IO
import System.Cmd
import System.Exit
import Data.Time.Clock -- Not in 6.10
import Data.List

-- In seconds:
--desired_exec_length = 3
desired_exec_length = 5

--ghc_default_flags = ["-O2"]
--ghc_default_rts   = ["-qa"]


main = 
 do args <- getArgs 
    let usage = error "Usage: binsearch_throughput <benchmarkfile.exe> <all-args-but-last ...>"
    case args of
       []  -> usage
       [t] -> usage
       (trials:rest) -> do putStrLn$ "Searching input sizes for ("++ show trials ++" trials): " ++ show rest 
		           loop rest (read trials) 1

showTime t = show ((fromRational $ toRational t) :: Double)

loop :: [String] -> Int -> Integer -> IO ()
loop args 0 n = return ()
loop args trial n | n > 2 ^ 100 = error "This executable doesn't seem to scale in proportion to its last argument."
loop args@(file:extra) trial n = do 
		 putStr$ show n ++ " "
		 hFlush stdout
		 --time <- timeit$ rawSystem file [show n] 
                 -- This added it as a first argument:
		 --time <- timeit$ system$ file ++" "++ show n ++(concat $ intersperse " " extra)++" > /dev/null"
		 -- Instead lets add it as a last argument:
		 time <- timeit$ system$ file ++" "++ (concat $ intersperse " " extra)++  " "++show n++" > /dev/null"

		 putStrLn$ "Time consumed: "++ show time
		 hFlush stdout
		 let rate = fromIntegral n / time


		 -- [2010.06.09] Introducing a small fudge factor to help our guess get over the line: 
		 let initial_fudge_factor = 1.10
		     fudge_factor = 1.01 -- Even in the steady state we fudge a little
		     guess = desired_exec_length * rate 
		 if time >= desired_exec_length
		  then do putStrLn$ "  TRIAL: "++show trial++
				    " secPerUnit: "++ showTime (time / fromIntegral n) ++ 
				    " ratePerSec: "++ show (rate) ++ 
                                    " seconds: "++showTime time++"\n"
			  hFlush stdout
		          loop (file:extra) (trial-1) (round$ guess * fudge_factor)			  
		 
                  -- Here we're still in the doubling phase:
		  else if time < 0.100 
		  then loop args trial (2*n)

                  else do putStrLn$ "\n Estimated rate to be "++show (round$ rate)++
				    " per second.  Trying to scale up:"

		          -- Here we've exited the doubling phase, but we're making our first guess as to how big a real execution should be:
			  if time > 0.100 && time < 0.33 * desired_exec_length
  	  		     then do putStrLn$  "  (Fudging first guess a little bit extra)\n"
				     loop args trial (round $ guess * initial_fudge_factor)
			     else do putStrLn$ ""
				     loop args trial (round$ guess * fudge_factor)
				  
timeit io = 
    do strt <- getCurrentTime
       code <- io       
       end  <- getCurrentTime
       case code of 
         ExitSuccess -> return ()
	 ExitFailure x -> error$ "Child process failed with exit code "++ show x
       return (diffUTCTime end strt)

