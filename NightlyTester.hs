{-# LANGUAGE RecordWildCards, ScopedTypeVariables, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fwarn-unused-imports #-}

module NightlyTester 
 (
   NightlyConfig(..),
   mkDefaultConfig, runNightlyTest,
   section, newline,
   mrun, mprint, publish,  group,

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
-- import Data.Maybe
import Control.Monad
import Control.Concurrent
import Control.Exception
import Data.IORef
import Data.Typeable
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

failureLog = unsafePerformIO$ newIORef []

sectionLog :: IORef [(String,[String])]
sectionLog = unsafePerformIO$ newIORef []
inGroup    = unsafePerformIO$ newIORef False
currentSec = unsafePerformIO$ newIORef ""

rootDir    = unsafePerformIO$ newIORef (error "rootDir uninitialized!")


padR n str = 
  let len = length str in
  if len < n
  then str ++ (take (n - len) (repeat ' ')) 
  else str

padL n str = 
  let len = length str in
  if len < n
  then (take (n - len) (repeat ' ')) ++ str
  else str


publish = do 
  NC{..} <- readIORef currentCfg
  p      <- readIORef passed
  let victory = if p then "PASSED" else "FAILED"
      rev     = maybe "" id revID

  newline
  mprint$ "SUMMARY:"
--  mprint$ "________________________________________________________________________________"
  mprint$ "+------------------------------------------------------------------------------+"
  log <- readIORef sectionLog
  forM_ (reverse log) $ \ pr -> 
    case pr of 
     ("",[])     -> return ()
     (sec,[])    -> do mprint$ (padR 65 $ "| "++sec++ ": ") ++ (padL 13 "passed") ++ " |"
     (sec,[_])   -> do mprint$ (padR 65 $ "| "++sec++ ": ") ++ (padL 13$ "1 failure") ++ " |"
     (sec,fails) -> do mprint$ (padR 65 $ "| "++sec++ ": ") ++ (padL 13$ show (length fails) ++" failure(s)") ++ " |"
  mprint$ "|______________________________________________________________________________|"
--  mprint$ "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
  newline
  newline

  mprint$ "Done with all regression testing ("++ victory ++").  Next publishing results."

  b <- doesDirectoryExist publishDir
  when (not b) $ do
    mprint$ "Publishing destination "++show publishDir++" does not exist, attempting to create..."
    mrun$ "mkdir -p "++show publishDir

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


-- Could use ContT monad here... but this is quick and dirty.
data AbortGroup = AbortGroup
  deriving (Show, Typeable)
instance Exception AbortGroup

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
	 ExitFailure n -> do 
			     hClose logh
			     mprint$ "ERROR: subprocess exited with code: "++show n
			     writeIORef passed False
			     modifyIORef failureLog (cmd:)
			     ing <- readIORef inGroup
			     when (ing) (throw AbortGroup)

runNightlyTest cfg@NC{..} action = 
  do 
     writeIORef currentCfg cfg
     curdir <- getCurrentDirectory
     writeIORef rootDir curdir

     -- Initialization:
     b <- doesFileExist logFile
     when b $ do 
       putStrLn$ chatterTag++"Logfile "++ logFile ++" already exists, deleting."
       removeFile logFile
       putStrLn$ chatterTag++"Deleted"

     mprint "Running full nightly regression tests in the current directory."
     mprint$ "Reporting results to email addresses: "++ show emails

     action
     endSection -- Cap the final section.
     publish

-- TODO: This should log things per-section to report a SUMMARY
section title = 
 do endSection -- End the previous section.
    writeIORef currentSec title
    writeIORef failureLog []
    -- Every section starts out in the root directory.
    returnToRootDir
    newline
    newline 
    mprint "============================================================"
    mprint ("  "++title)
    mprint "============================================================"

endSection = 
 do prev  <- readIORef currentSec
    fails <- readIORef failureLog
    modifyIORef sectionLog ((prev,fails):)

returnToRootDir = 
 do root <- readIORef rootDir
    setCurrentDirectory root

-- A series of commands that must be executed together, if one fails,
-- the group must be aborted.
group :: IO () -> IO ()
group action = do
 -- This currently catches ANY exception:
  writeIORef inGroup True
  let fail e = 
       do mprint$ "Group of commands failed with exception:\n  "++show (e::SomeException)
          writeIORef passed False
	  mprint$ "Continuing after failed group..."
	  returnToRootDir

  -- Catch AbortGroup specifically and other exceptions generally:
  handle (\e -> do fail (toException (e::SomeException))
	           modifyIORef failureLog ("HaskellException":)) $ 
    handle (\ AbortGroup -> fail (toException AbortGroup))
	   action
  writeIORef inGroup False


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
  mrun "date"
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
