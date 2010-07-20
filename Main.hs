
module Main where

import CncLexer hiding (main)
import CncGrammar
import AST
import GatherGraph

import Text.PrettyPrint.HughesPJClass
import Data.Generics.Serialization.SExp
import Data.Generics.Serialization.Streams

import System.Environment
import System.Console.GetOpt

import System.IO
--import GHC.IO.Handle
--import GHC.IO.Handle.FD

-- import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
    
data Flag 
    = Verbose  | Version 
    | Input String | Output String | LibDir String
      deriving Show
    
options :: [OptDescr Flag]
options =
     [ Option ['v']     ["verbose"] (NoArg Verbose)       "verbose translator output to stdout"
     , Option ['V']     ["version"] (NoArg Version)       "show version number"
     ]
    
-- Here we test our parser.
main = do 
 argv <- getArgs
 let header = "\nUsage: cnctrans [OPTION...] files..."
     --defaultErr errs = ioError (userError (concat errs ++ usageInfo header options))
     defaultErr errs = error $ "ERROR!\n" ++ (concat errs ++ usageInfo header options)
 (opts,files) <- 
     case getOpt Permute options argv of
       (o,n,[]  ) -> return (o,n)
       (_,_,errs) -> defaultErr errs
 case files of 
   [file] -> do h <- openFile file ReadMode; run h
   []     -> defaultErr ["\nNo files provided!\n"]
   ls     -> defaultErr ["\nCurrently the translator expects exactly one input file.\n"]


run handle = do
 s <- hGetContents handle

 putStrLn "Lexed: "
 sequence_ $ map print $ scan_to_list s

 let parsed = runCnc s
 putStrLn "\nParsed:"
 print parsed

 putStrLn "\n Ok how bout sexp:"
 sequence_ $ map (\stmt -> putStrLn $ buildList $ sexpSerialize stmt) parsed

 putStrLn "\nPretty:"
 putStrLn$ renderStyle style $ hcat $ map pPrint parsed

 putStrLn "\nGRAPH:"
 let graph = coalesceGraph parsed
 print graph

 putStrLn "\n Done.."    



