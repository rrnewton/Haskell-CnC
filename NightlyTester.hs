{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module NightlyTester 
 (
   NightlyConfig(..),
   mkDefaultConfig, runNightlyTest,
   section, newline,
   mrun, mprint, publish, 

   reportMachineInfo, gitReportHead
 )
where 

import System.Directory
import System.IO
import System.Environment
import System.Exit
import System.IO.Error
import System.Process
import System.Locale
import System.FilePath
import System.Info
import Data.Time
import Data.Maybe
import Control.Monad
import Control.Concurrent
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
import GHC.IO.Handle
import HSH


data NightlyConfig = NC {
   chatterTag :: String 
 , logFile    :: String
 , publishDir :: String
 , reponame   :: String
 , emails     :: [String]
 , revID      :: Maybe String  -- An identification of the revision being tested.
 }

mkDefaultConfig reponame emails = 
  NC { logFile    = "nightly_test.log"
     , chatterTag = " ["++ reponame ++"] "
       -- There's no real system-independent way to make an absolute path here:
       --     , publishDir = joinPath ["var","www","nightlytest",reponame]
     , publishDir = "/var/www/nightlytest" </> reponame
     , reponame   = reponame
     , revID      = Nothing
     , emails     = emails
     }



-- This should be a state transformer monad.  But because we will
-- never have more than one nightly test running at once we just make
-- these global variables inside the IO monad:
passed     = unsafePerformIO$ newIORef True
currentCfg = unsafePerformIO$ newIORef (error "currentCfg uninitialized!!")

publish = do 
  NC{..} <- readIORef currentCfg
  p      <- readIORef passed
  let victory = if p then "PASSED" else "FAILED"
      rev     = maybe "" id revID
                -- case revID of 
		--   Nothing -> ""
		--   Just x  -> x
  mprint$ "Done with all regression testing ("++ victory ++").  Next publishing results."

  utc      <- getCurrentTime
  timezone <- getCurrentTimeZone
  let local = utcToLocalTime timezone utc
      tstr = formatTime defaultTimeLocale "%Y_%m_%d_%H:%M" local
      finaldest = publishDir </> tstr ++"_"++rev++"_"++ victory ++ ".log"

  putStrLn$ chatterTag++" Copying "++ logFile ++"  "++ finaldest
  copyFile logFile finaldest
  runCommand ("chmod ugo+rX -R "++publishDir++"/*") >>= waitForProcess
  putStrLn$ chatterTag++"Done copying.  Next sending emails."
  
  forM_ emails $ \ email -> do
     let mailcmd = "mail "++email++" -s '[ArBB-VM] Nightly Test "++victory++ "' < " ++ logFile
     putStrLn$ chatterTag++"Running: "++mailcmd
     proc <- runCommand mailcmd
     waitForProcess proc
     Just code <- getProcessExitCode proc
     case code of 
       ExitSuccess   -> putStrLn$ "Mail command exited successfully."
       ExitFailure n -> putStrLn$ "Mail command failed with code: "++ show n
     
  return ()


mprint str = 
  do NC{..} <- readIORef currentCfg
     mprintWTag chatterTag str

mprintWTag tag str = 
  do NC{..} <- readIORef currentCfg
     let tagged = tag ++ str 
     putStrLn  tagged
     withFile logFile AppendMode $ \ h -> do 
       hPutStrLn h tagged

newline = mprintWTag "" ""

  
mrun cmd = 
  do 
     NC{..} <- readIORef currentCfg
     putStrLn ""
     withFile logFile AppendMode $ \h -> hPutStrLn h ""

     mprint$ "Running command: "++ show cmd
     withFile logFile AppendMode $ \ logh -> do 
       (hin,hout,herr, phand) <- runInteractiveCommand cmd

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

runNightlyTest cfg@NC{..} action = 
  do 
     writeIORef currentCfg cfg

     -- Initialization:
     b <- doesFileExist logFile
     when b $ do 
       putStrLn$ chatterTag++"Logfile already exists, deleting."
       removeFile logFile
       putStrLn$ chatterTag++"Deleted"

     mprint "Running full nightly regression tests in the current directory."
     mprint$ "Reporting results to email addresses: "++ show emails

     action
     publish

-- TODO: This should log things per-section to report a SUMMARY
section title = 
 do newline
    newline 
    mprint "============================================================"
    mprint ("  "++title)
    mprint "============================================================"


--------------------------------------------------------------------------------
-- Extra reporting routines:


gitReportHead = do
  -- Grab the commit hash:
  line :: String <- runSL "git log" 
  let [_,hash]  = words line
      shorthash = take 10 hash
  mprint$ "Git reports HEAD commit hash: "++ hash
  cfg <- readIORef currentCfg
  writeIORef currentCfg (cfg{ revID = Just shorthash })


reportMachineInfo = do
  mprint "============================================================"
  mprint "  Machine information:"
  mprint "============================================================"
  mrun "hostname"
  when (os == "linux") $ do
    mrun "cat /etc/issue"
    mrun "cat /proc/cpuinfo | head -n 50"
  mrun "top | head -n 50"


--------------------------------------------------------------------------------
-- Helper functions.

echoLoop indent inh outh= 
  do mbln <- System.IO.Error.try$ hGetLine inh
     case mbln of 
       Left e -> if isEOFError e then return () else ioError e
       Right ln -> do 
	 let ln' = indent ++ ln
	 putStrLn       ln'
	 hPutStrLn outh ln'
	 echoLoop indent inh outh

-- Convenience function:
forkJoin ls = 
  do sync <- newChan 
     forM_ ls $ \m -> forkIO$ do m; writeChan sync ()
     forM_ ls $ \_ -> readChan sync
