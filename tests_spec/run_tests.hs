#!/usr/bin/env runhaskell

import HSH

root = ".."
cnc = root ++ "/dist/build/cnc/cnc"

dubquote str = "\"" ++ str ++ "\""

trans file = 
  do 
     putStrLn$ "\nTranslating .cnc file: "++ file
     putStrLn$ "================================================================================"
     let cmd = cnc ++ " trans " ++ dubquote file
     putStrLn$ "Running command: " ++ cmd
     runIO$ cmd

main = do
  putStrLn$ "Translating several test .cnc files:"

  trans "tagfuns1.cnc"
  trans "tagfuns2.cnc"
  trans "tagfuns3.cnc"

  trans "cholesky.cnc"
  trans "mandel.cnc"
  trans "eigensolver.cnc"



-- TODO: line set comparison
