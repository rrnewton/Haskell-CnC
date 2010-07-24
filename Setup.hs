#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.PreProcess
import Distribution.PackageDescription	
import Distribution.Simple.LocalBuildInfo	
import System.Cmd(system) 
import System.Exit

--main = defaultMainWithHooks simpleUserHooks
-- --defaultUserHooks


--import Intel.Cnc

main :: IO () 
main = do putStrLn$ "Running Setup.hs ..."
	  let hooks = simpleUserHooks 
	  defaultMainWithHooks 
	   (hooks {runTests = myTests
	          --, hookedPreProcessors= (mypp : hookedPreProcessors hooks) 
		  }) 


-- mypp :: PPSuffixHandler
-- mypp = (".y.pp", \ _ _ -> ppTestHandler)

-- ppTestHandler :: PreProcessor
-- ppTestHandler =
--    PreProcessor {
--      platformIndependent = True,
--      runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
--        do putStrLn$ (inFile++" has been preprocessed to "++outFile)
--           stuff <- readFile inFile
--           writeFile outFile ("-- preprocessed as a test\n\n" ++ stuff)
--           return ()
--    }


myTests :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO () 
myTests _ _ _ _ = do code <- system "./dist/build/haskell-cnc-runTests/haskell-cnc-runTests"
		     exitWith code

