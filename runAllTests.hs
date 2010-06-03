
import System.Directory
import qualified Intel.Cnc
import qualified Intel.CncPure
import qualified Intel.CncUtil
import System.Cmd(system) 
import System.Exit

import HSH
import Test.HUnit

import Data.String.Utils -- from MissingH package

main = do 
	  cd <- getCurrentDirectory

	  putStrLn$ "Running Unit tests in directory: " ++ show cd
	  
	  putStrLn$ "\n================================================================================"
	  putStrLn$ "Running "++ show (testCaseCount Intel.CncUtil.tests) ++" tests from Intel.CncUtil"
	  putStrLn$ "================================================================================\n"
	  code1 <- runTestTT Intel.CncUtil.tests

	  putStrLn$ "\n================================================================================"
	  putStrLn$ "Running "++ show (testCaseCount Intel.Cnc.tests) ++" tests from Intel.Cnc"
	  putStrLn$ "================================================================================\n"
	  code2 <- runTestTT Intel.Cnc.tests

	  putStrLn$ "\n================================================================================"
	  putStrLn$ "Running "++ show (testCaseCount Intel.CncPure.tests) ++" tests from Intel.CncPure"
	  putStrLn$ "================================================================================\n"
	  code3 <- runTestTT Intel.CncPure.tests

          let problems = errors code1 + failures code1 +
			 errors code2 + failures code2 + 
			 errors code3 + failures code3

	  putStrLn$ "\n================================================================================"
	  putStrLn$ "Finally running system tests in all configurations (example programs):"
	  putStrLn$ "================================================================================\n"

          b1 <- doesFileExist "run_all_examples.sh"
          if b1
           then putStrLn "!!! run_all_examples.sh found in current directory, using that!\n\n"
           else do [ver] <- run "ghc-pkg latest haskell-cnc"
                   dir <- run$ ("ghc-pkg field "++ver++" include-dirs ") -|- replace "include-dirs:" ""
                   putStrLn$ "Switching to directory: " ++ dir
                   setCurrentDirectory dir
	   
          b2 <- doesFileExist "run_all_examples.sh"
          if not b2
            then error$ "Uh oh, the script run_all_examples.sh doesn't exist in this directory.\n"
    -- 		             "  If cabal installed the package you may find it in your ~/.cabal/lib directory."
	    else return ()   

          -- I have problems with cabal sdist not preserving executable flags.
          system "chmod +x ./runcnc" 
          system "chmod +x ./run_all_examples.sh" 
          system "chmod +x ./ntimes" 
          system "chmod +x ./ntimes_minmedmax" 

          code <- system "TRIALS=1 ./run_all_examples.sh" 
	  let code = ExitSuccess 
	  case (problems,code) of
	    (0, ExitSuccess)   -> exitWith ExitSuccess
	    (n, ExitSuccess)   -> do putStrLn$ "ERROR: "++ show n ++" failures in unit tests!";    exitWith (ExitFailure n)
	    (_, ExitFailure n) -> do putStrLn "ERROR: Example programs failed!\n"; exitWith (ExitFailure n)
