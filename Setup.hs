#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.PackageDescription	
import Distribution.Simple.LocalBuildInfo	
import System.Cmd(system) 
import System.Exit

--main = defaultMainWithHooks simpleUserHooks
-- --defaultUserHooks

--main = defaultMainWithHooks hooks
--  where hooks = simpleUserHooks { runTests = runTests' }

--import Intel.Cnc

main :: IO () 
main = do putStrLn$ "Running Setup.hs ..."
	  defaultMainWithHooks (simpleUserHooks {runTests = myTests}) 

myTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO () 
myTests _ _ _ _ = do code <- system "./dist/build/runAllTests/runAllTests" 
		     exitWith code

