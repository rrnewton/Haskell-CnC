
module Main where

import Intel.Cnc.Spec.CncLexer hiding (main)
import Intel.Cnc.Spec.CncGrammar
import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.GatherGraph
import Intel.Cnc.Spec.Codegen.CppOld

import Text.PrettyPrint.HughesPJClass
import Data.Generics.Serialization.SExp
import Data.Generics.Serialization.Streams
import Data.List

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
  deriving (Show, Eq)
    
options :: [OptDescr Flag]
options =
     [ Option ['v']     ["verbose"] (NoArg Verbose)       "verbose translator output to stdout"
     , Option ['V']     ["version"] (NoArg Version)       "show version number"
     ]
  

when b action = if b then action else return ()
  
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

  let file = 
       case files of 
        [file] -> file
        []     -> defaultErr ["\nNo files provided!\n"]
        ls     -> defaultErr ["\nCurrently the translator expects exactly one input file.\n"]
      verbose = Verbose `elem` opts

  handle <- openFile file ReadMode
  str <- hGetContents handle

  when verbose$ putStrLn "\nAll Lexed Tokens: "
  when verbose$ putStrLn "================================================================================"
  --when verbose$ print $ hcat $ intersperse (text ", ") $ map (\ (L _ cl str) -> text (show cl) <+> pp str) $ scan_to_list str
  when verbose$ print $ sep $ map (\ (L _ cl str) -> text (show cl) <+> pp str) $ scan_to_list str
       --filter (not . is_comment) $ scan_to_list str -- Even filtering the long lines still doesn't `sep` to do the right thing.

  let parsed = runCnc str
  when verbose$ putStrLn "\nParsed AST (detailed):"
  when verbose$ putStrLn "================================================================================"
  when verbose$ sequence_ $ map (print . stripDecor) parsed

  when verbose$ putStrLn "\nParsed AST rendered as a SExp:"
  when verbose$ putStrLn "================================================================================"
  when verbose$ sequence_ $ map (\stmt -> putStrLn $ buildList $ sexpSerialize stmt) parsed

  putStrLn "\nPretty printed parsed AST:"
  putStrLn "================================================================================"
  putStrLn$ renderStyle style $ hcat $ map pPrint parsed

  putStrLn "\nCoalesced CnC Graph:"
  putStrLn "================================================================================"
  let graph = coalesceGraph parsed

  putStrLn "\n \n"
  print $ pp graph


  putStrLn "\nFinally, generating header:"
  putStrLn "================================================================================"

  putStrLn$ emitCppOld graph

  putStrLn "\n Done.."    

