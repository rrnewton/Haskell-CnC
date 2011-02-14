
-- This is an alternative wrapper around the main command which
-- switches to a different directory before executing.

-- It can be used with "runhaskell" to run from source.

import qualified Intel.Cnc.Spec.MainExecutable as M
import System.Environment
import System.Directory

main = 
 do
    putStrLn$ "[CnC] Script wrapper, running from source."
    args <- getArgs 
    let dir = head args
    putStrLn$ "[CnC] Working directory: " ++ dir
    setCurrentDirectory dir
    withArgs (tail args) M.mainExecutableCommand
