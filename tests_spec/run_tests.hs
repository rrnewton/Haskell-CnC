#!/usr/bin/env runhaskell 

import Control.Monad
import HSH
import HSH.ShellEquivs
import Test.HUnit
import Debug.Trace
import qualified Data.Set as S

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

  let myPut msg True  () = do putStrLn ""; putStrLn$ msg
      myPut msg False () = return ()
  runTestText (PutText myPut ()) $
    TestList [ test_cases "1"
	     , test_cases "4"
	     ]
  return ()

test_cases numthreads = 
    testSet ("translator, num threads = "++numthreads) $ 
    map (each_test numthreads) all_tests

each_test numthreads name = 
  testCase "" ("Running, " ++ numthreads ++" thread(s): "++ name ) $ test $
     do out <- run$ setenv [("CNC_NUM_THREADS", numthreads)] $
 	            ("./"++ name ++".exe") -|- tee [name++".out"]
	expected <- readFile$ name ++ ".cmpr"
	putStrLn$ "     Out was "++ show (length (out::String)) ++" expected was " ++ show (length expected)
	putStrLn$ "   Equal? "++ show (out == expected)

        let set_eq = lineSetEqual out expected
	    s1 = S.fromList$ lines out
	    s2 = S.fromList$ lines expected
	putStrLn$ "   Set Equal? "++ show set_eq

        when (not set_eq) $ do
          let d1 = S.difference s1 s2
	      d2 = S.difference s2 s1

          putStrLn$ "     Diff1: " 
          putStrLn$ "------------------------------------------------------------" 
	  mapM_ putStrLn (S.toList d1)
          putStrLn$ "------------------------------------------------------------" 
          putStrLn$ "     Diff2: " 
          putStrLn$ "------------------------------------------------------------" 
	  mapM_ putStrLn (S.toList d2)
          putStrLn$ "------------------------------------------------------------" 

	assert (lineSetEqual out expected)
	putStrLn$ ""
	return ()
 
	  
----------------------------------------------------------------------------------------------------
-- To implement the above, here are some helpers for checking output.
----------------------------------------------------------------------------------------------------


-- TODO: exact comparison

-- TODO: line set comparison (sorted/unsorted)

lineSetEqual a b = 
  S.fromList (lines a) == S.fromList (lines b)

-- TODO: fuzzy number matching and/or regular expressions.


-- Tag a little bit more verbose output to the tests:
testCase prefix str tst = TestLabel lab (trace (tag++ lab) tst)
 where lab = if prefix == ""
             then str
	     else prefix ++ ": " ++ str
--       tag = " *** "
       tag = " [test] "

-- Likewise, identify the per-module sub-groups of tests
testSet name ls = 
    trace ("\n"++header ++"\n"++ spacer) (TestList ls)
 where header = "Running tests for " ++ show name 
       spacer = (take (length header) $ repeat '=')


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
