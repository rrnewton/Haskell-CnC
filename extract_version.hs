#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

-- A simple script to extract the version number from the .cabal file and stick it in the code.

import System.IO
import Data.Time
import HSH
import HSH.ShellEquivs

dubquote str = "\"" ++ str ++ "\""

main = do 
  fh <-  openFile "Intel/Cnc/Spec/Version.hs" WriteMode 

  date <- getCurrentTime
  
  version_lines :: [String] <- run $ catFrom ["haskell-cnc.cabal"] -|- grep "Version" 
  let version = words (head version_lines) !! 1 

  --let version   = "0.1.3.100"

  hPutStrLn fh$ "-- WARNING: This file is generated from the .cabal file."
  hPutStrLn fh$ "module Intel.Cnc.Spec.Version where"
  hPutStrLn fh$ "version = " ++ dubquote version
  hPutStrLn fh$ "builddate = " ++ dubquote (show date)

  hClose fh
  
