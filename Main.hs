
module Main where

import CncLexer hiding (main)
import CncGrammar
import AST
import GatherGraph

import Text.PrettyPrint.HughesPJClass
import Data.Generics.Serialization.SExp
import Data.Generics.Serialization.Streams



-- Here we test our parser.
main = do 
 s <- getContents
 putStrLn "Lexed: "
 sequence_ $ map print $ scan_to_list s

 let parsed = runCnc s
 putStrLn "\nParsed:"
 print parsed

 putStrLn "\n Ok how bout sexp:"
 sequence_ $ map (\stmt -> putStrLn $ buildList $ sexpSerialize stmt) parsed

 putStrLn "\nPretty:"
 putStrLn$ renderStyle style $ hcat $ map pPrint parsed

 putStrLn "\n Done.."    



