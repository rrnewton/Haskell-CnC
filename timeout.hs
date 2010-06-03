#!/usr/bin/env runhaskell

import Data.List
import Data.IORef
import System
import System.Exit
import System.Process
import System.Environment
import System.Posix.Unistd

import Control.Concurrent

polltime = 2 -- in seconds

main = 
    do args <- getArgs 
       -- A flag for completion:
       ref <- newIORef False
       case args of 
        time:rest -> 
	    let timeout = read time 
		cmd = concat $ intersperse " " rest
            in
	    do putStrLn$ "++ Running command with timeout = "++ show timeout ++" seconds:\n  " ++ cmd
	       pid <- runCommand  cmd

               sync <- newEmptyMVar

	       let loop acc = 
		    if acc > (timeout :: Int)
		    then do putStrLn$ "\nERROR: TIMED OUT!"
	 		    exitWith (ExitFailure 88)
	 	    else do sleep polltime
			    x <- getProcessExitCode pid
			    case x of 
	                      Nothing -> do putStrLn$ "++ Polling, approximately "++ 
						      show (acc+polltime) ++" seconds have elapsed..."
				            loop (acc+polltime)
			      Just code -> putMVar sync code
	       let poll_thread = loop 0
	       let wait_thread = 
                        do waitForProcess pid
			   writeIORef ref True
			   putStrLn "++ Command completed without timing out."
			   putMVar sync ExitSuccess
	       forkIO wait_thread
	       forkIO poll_thread
	       final <- readMVar sync
	       exitWith final
	       