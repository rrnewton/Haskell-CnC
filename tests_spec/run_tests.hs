#!/usr/bin/env runhaskell

import Control.Monad
import HSH
import HSH.ShellEquivs
import Test.HUnit

----------------------------------------------------------------------------------------------------
--   First some global constants:
----------------------------------------------------------------------------------------------------

-- The root of the haskell_cnc working copy:
root = ".."  
-- The location of the cnc executable built by cabal:
-- cnc = root ++ "/dist/build/cnc/cnc"
-- Or in this case built by the Makefile (yuck, but no partial-target builds in cabal atm):
cnc = root ++ "/build/cnc"

all_tests = 
 [
   "tagfun_checking_valid" 
 , "tagfun_depends" 
 , "simple_cycle" 
 , "bigger_cycle"
 , "two_cycles"
 , "reduction_test"
 , "reduction_test2"
 ]

----------------------------------------------------------------------------------------------------
--   next the main script that performs the test
----------------------------------------------------------------------------------------------------

main = do
  putStrLn$ "running tests of cnc translator..."
  forM_ all_tests $ \ str -> do 
     putStrLn ""
     putStrLn$ "Running test, "++ str
     putStrLn "================================================================================"
     
     runIO$ setenv [("CNC_NUM_THREADS","1")] $
	    ("./"++ str ++".exe") -|- tee [str++".out"]


	  
----------------------------------------------------------------------------------------------------
-- To implement the above, here are some helpers for checking output.
----------------------------------------------------------------------------------------------------


-- TODO: exact comparison

-- TODO: line set comparison (sorted/unsorted)

-- TODO: fuzzy number matching and/or regular expressions.



----------------------------------------------------------------------------------------------------
-- Currently UNUSED utilities
----------------------------------------------------------------------------------------------------

trans_all = do
  putStrLn$ "Translating several test .cnc files:"
  trans "tagfuns1.cnc"
  trans "tagfuns2.cnc"
  trans "tagfuns3.cnc"
  trans "cholesky.cnc"
  trans "mandel.cnc"
  trans "eigensolver.cnc"


dubquote str = "\"" ++ str ++ "\""
trans file = 
  do 
     putStrLn$ "\nTranslating .cnc file: "++ file
     putStrLn$ "================================================================================"
     let cmd = cnc ++ " trans " ++ dubquote file
     putStrLn$ "Running command: " ++ cmd
     runIO$ cmd
