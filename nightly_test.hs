#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}
-- MUST be run from the directory containing this script.

import System.Directory
import System.IO
import System.Environment
import System.Exit
import System.IO.Error
import System.Process
import System.Locale
import System.FilePath
-- import System.Time
import Data.Time
import Control.Monad
import Control.Concurrent
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
import GHC.IO.Handle
import HSH

----------------------------------------------------------------------------------------------------
-- Global settings 

tag = " [Haskell-CnC] "
publishdir = "/var/www/nightlytest/Haskell-CnC/"

-- This script produces output BOTH to stdout and to a logfile.
logfile = "nightly_test.log"


----------------------------------------------------------------------------------------------------
-- Main Script

main = do 
  -- Initialization:
  b <- doesFileExist logfile
  when b $ do 
    putStrLn$ tag++"Logfile already exists, deleting."
    removeFile logfile
    putStrLn$ tag++"Deleted"

  mprint "Running full nightly regression tests in the current directory."
  emails <- getArgs 
  mprint$ "Reporting results to email addresses: "++ show emails

  -- Grab the commit hash:
  line :: String <- runSL "git log" 
  let [_,hash]  = words line
      shorthash = take 10 hash
  mprint$ "Git reports HEAD commit hash: "++ hash
  mprint "============================================================"
  mprint "  Machine information:"
  mprint "============================================================"
  mrun "hostname"
  mrun "cat /etc/issue"
  mrun "cat /proc/cpuinfo | head -n 50"
  mrun "top | head -n 50"

  --------------------------------------------------------------------------------
  mprint "============================================================"
  mprint "  Beginning main tests"
  mprint "============================================================"
  mrun "cabal configure"
  mrun "cabal build"
  cd "tests_spec"
  mrun "make test"
--  mrun "make test"
  --------------------------------------------------------------------------------

  mprint ""

  publish shorthash emails




----------------------------------------------------------------------------------------------------
-- Helper definitions

passed = unsafePerformIO$ newIORef True

-- ifM p t f  = p >>= (\p' -> if p' then t else f)

publish shorthash emails = do 
  p <- readIORef passed
  let victory = if p then "PASSED" else "FAILED"
  mprint$ "Done with all regression testing ("++ victory ++").  Next publishing results."

  utc      <- getCurrentTime
  timezone <- getCurrentTimeZone
  let local = utcToLocalTime timezone utc
      tstr = formatTime defaultTimeLocale "%Y_%m_%d_%H:%M" local
      finaldest = publishdir </> tstr ++"_"++shorthash++"_"++ victory ++ ".log"

  putStrLn$ tag++" Copying "++ logfile ++"  "++ finaldest
  copyFile logfile finaldest
  runCommand ("chmod ugo+rX -R "++publishdir++"/*") >>= waitForProcess
  putStrLn$ tag++"Done copying.  Next sending emails."
  
  forM_ emails $ \ email -> do
     let mailcmd = "mail "++email++" -s '[ArBB-VM] Nightly Test "++victory++ "' < " ++ logfile
     putStrLn$ tag++"Running: "++mailcmd
     proc <- runCommand mailcmd
     waitForProcess proc
     Just code <- getProcessExitCode proc
     case code of 
       ExitSuccess   -> putStrLn$ "Mail command exited successfully."
       ExitFailure n -> putStrLn$ "Mail command failed with code: "++ show n
     


mprint str = 
  do let tagged = tag++ str 
     putStrLn  tagged
--     h <- readIORef loghand 
-- Here's the expensive approach, append each time:
     withFile logfile AppendMode $ \ h -> do 
       hPutStrLn h tagged


echoLoop indent inh outh= 

  do mbln <- System.IO.Error.try$ hGetLine inh
     case mbln of 
       Left e -> if isEOFError e then return () else ioError e
       Right ln -> do 
	 let ln' = indent ++ ln
	 putStrLn       ln'
	 hPutStrLn outh ln'
	 echoLoop indent inh outh


forkJoin ls = 
  do sync <- newChan 
     forM_ ls $ \m -> forkIO$ do m; writeChan sync ()
     forM_ ls $ \_ -> readChan sync
  
mrun cmd = 
  do 
     putStrLn ""
     withFile logfile AppendMode $ \h -> hPutStrLn h ""

     mprint$ "Running command: "++ show cmd
     withFile logfile AppendMode $ \ logh -> do 
--       (hin,Just hout,herr, phand) <- createProcess (shell cmd)
       (hin,hout,herr, phand) <- runInteractiveCommand cmd
--       runCommand cmd
-- I couldn't get this to work:
-- --       hDuplicateTo hin  stdin
-- --       hDuplicateTo hout stdout
-- --       hDuplicateTo herr stderr
--        hDuplicateTo herr hout
--        putStrLn$ "Redirected, getting output:"

       forkJoin [ do echoLoop "   " hout logh
                , do echoLoop "   " herr logh ]

       waitForProcess phand
       Just code <- getProcessExitCode phand
       case code of 
         ExitSuccess   -> return ()
	 ExitFailure n -> do writeIORef passed False
			     hClose logh
			     mprint$ "ERROR: subprocess exited with code: "++show n
			     exitWith (ExitFailure n)

