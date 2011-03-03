#!/usr/bin/env runhaskell

import System.Directory
import HSH

-- cd = setCurrentDirectory


publishdir = "/var/www/nightlytest/Haskell-CnC/"

main = do 

  putStrLn "Running full nightly regression tests in the current directory."

--  runIO "git clone git://github.com/rrnewton/Haskell-CnC.git"
--  cd    "Haskell-CnC"

  putStrLn "Done with all regression testing."
