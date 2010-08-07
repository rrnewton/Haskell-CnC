{-# LANGUAGE CPP, NamedFieldPuns #-}
module Main where

import Intel.Cnc.Spec.CncLexer hiding (main)
import Intel.Cnc.Spec.CncGrammar
import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.CncGraph
import Intel.Cnc.Spec.GatherGraph
import Intel.Cnc.Spec.Util
import Intel.Cnc.Spec.Codegen.CppOld
import Intel.Cnc.Spec.Codegen.Haskell

import Intel.Cnc.Spec.ReadHarch

import Text.PrettyPrint.HughesPJClass
import Data.Maybe ( fromMaybe )
import Data.IORef
import Control.Monad hiding (when)

import System.Environment
import System.Console.GetOpt
import System.FilePath.Posix
import System.IO
import System.IO.Unsafe
import System.Exit

import Intel.Cnc.Spec.Vacuum

-- These expand the file size quite a bit.  Not committing to include right now:
-- #define CNCVIZ
#ifdef CNCVIZ 
import Intel.Cnc.Spec.CncViz
#endif

-- TODO: It would be nice to get this from the .cabal file.
-- That would require some quasiquoting/templating or the C preprocessor.
version = "0.1.3.99"
    
data Flag 
    = Verbose  | Version 
    | Cpp | CppOld | Haskell
 -- | Input String  | LibDir String
    | Output String
    | HarchPart String
    | HarchViz String
    | NullOpt
    | DotOpt
    | VizOpt
    | UbigraphOpt
    | VacuumViz
    | Vacuum
  deriving (Show, Eq)
    
options :: [OptDescr Flag]
options =
     [ Option ['v']     ["verbose"] (NoArg Verbose)       "verbose translator output to stdout"
     , Option ['V']     ["version"] (NoArg Version)       "show version number"

     , Option []        []          (NoArg NullOpt)  ""
     , Option []        []          (NoArg NullOpt)  ""
     , Option []        []          (NoArg NullOpt)  "Translating to and from .cnc specification files:"
     , Option []        ["----------------"]  (NoArg$ error "internal problem")  "----------------------------------------------------------------------"
     , Option ['h']     ["haskell"] (NoArg Haskell)      "translate spec to Haskell code"
     , Option []        ["cpp"]     (NoArg Cpp)          "translate spec to C++ code"
     , Option ['c']     ["cppold"]  (NoArg CppOld)       "translate spec to C++ code (legacy C++ API) [default]"
     , Option ['o']     ["output"]  (ReqArg Output "FILE") "direct output to FILE instead of default"


#ifdef CNCVIZ
     , Option []        []          (NoArg NullOpt)  ""
--     , Option []        ["dot"]      (NoArg DotOpt)   "output CnC graph in graphviz .dot format as well"
     , Option []        ["dot"]      (NoArg DotOpt)   "output CnC graph in graphviz .dot format instead of translating"
     , Option []        ["viz"]      (NoArg VizOpt)   "similar to --dot, a shortcut to visualize a CnC graph in a X11 window"
     , Option []        ["ubigraph"] (NoArg UbigraphOpt)  "like --viz, but visualize on a local Ubigraph server"
#endif

     , Option []        []          (NoArg NullOpt)  ""
     , Option []        ["vacuum"]  (NoArg Vacuum)  "suck up the output of CnC::debug::trace (on stdin) to create a .cnc spec"
#ifdef CNCVIZ
     , Option []        ["vacuumviz"] (NoArg VacuumViz) "use trace output to visualize graph execution in realtime using ubigraph"
#endif
     , Option []        []          (NoArg NullOpt)  ""
     , Option []        []          (NoArg NullOpt)  ""
     , Option []        []          (NoArg NullOpt)  "Options to control Harch (the hierarchical partitioner):"
     , Option []        ["----------------"]  (NoArg$ error "internal problem")  "----------------------------------------------------------------------"
     , Option []        ["harch"]     (ReqArg HarchPart "FILE")   "read Harch graph metadata from FILE (used for translation)"
     , Option []        ["harchpart"] (ReqArg HarchPart "FILE") "perform graph partitioning on FILE (set output with -o)"
#ifdef CNCVIZ
     , Option []        ["harchviz"]  (ReqArg HarchViz "FILE")  "visualize the graph stored in FILE with Harch clustering"
#endif

#ifdef CNCVIZ
     -- , Option []        []          (NoArg NullOpt)  ""
     -- , Option []        []          (NoArg NullOpt)  ""
     -- , Option []        []          (NoArg NullOpt)  "Visualizing CnC and Harch graphs:"
     -- , Option []        ["----------------"]  (NoArg$ error "internal problem")  "----------------------------------------------------------------------"
#endif

     ]

printHeader = do
  putStrLn$ "Intel(R) Concurrent Collections Spec Translator, Haskell CnC Edition version "++ version
  putStrLn$ "Copyright 2010 Intel Corporation."

when b action = if b then action else return ()



------------------------------------------------------------------------------------------------------------------------
-- The translator front-end: parse a file, convert to graph:

readCnCFile :: Bool -> String -> IO CncSpec
readCnCFile verbose file = do 
  handle <- openFile file ReadMode
  str <- hGetContents handle

  when verbose$ putStrLn "================================================================================"
  when verbose$ putStrLn "\nAll Lexed Tokens: "
  --when verbose$ print $ hcat $ intersperse (text ", ") $ map (\ (L _ cl str) -> text (show cl) <+> pp str) $ scan_to_list str
  when verbose$ print $ sep $ map (\ (L _ cl str) -> text (show cl) <+> pp str) $ scan_to_list str
       --filter (not . is_comment) $ scan_to_list str -- Even filtering the long lines still doesn't `sep` to do the right thing.

  let parsed = runCncParser file str

  when verbose$ putStrLn "================================================================================"
  when verbose$ putStrLn "\nParsed AST (detailed):"
  when verbose$ sequence_ $ map (print . stripDecor) parsed

  -- when verbose$ putStrLn "\nParsed AST rendered as a SExp:"
  -- when verbose$ putStrLn "================================================================================"
  -- when verbose$ sequence_ $ map (\stmt -> putStrLn $ buildList $ sexpSerialize stmt) parsed

  putStrLn "================================================================================"
  putStrLn "\nPretty printed parsed AST:"
  putStrLn$ renderStyle style $ hcat $ map pPrint parsed

  -- [2010.07.23] Lazy parsing complicates this, it must happen after IO that touches the parse:
  hClose handle -- Cleaner to do this than to wait for garbage collection.

  putStrLn "================================================================================"
  putStrLn "\nCoalesced CnC Graph:"
  -- The name of the module is derived from the file name:	   
  let appname = takeBaseName file
      graph = coalesceGraph appname parsed

  putStrLn ""
  print $ pp graph

  putStrLn "================================================================================"
  return graph


  
------------------------------------------------------------------------------------------------------------------------
main = 
 do argv <- getArgs
    main2 argv

main2 argv = do  
  let usage = "\nUsage: cnctrans [OPTION...] files..."
      defaultErr errs = unsafePerformIO$ 
			do --printHeader
			   error $ "ERROR!\n" ++ (concat errs ++ usageInfo usage options)

  ----------------------------------------------------------------------------------------------------
  -- Read and process option flags:
  ----------------------------------------------------------------------------------------------------

  (opts,files) <- 
     case getOpt Permute options argv of
       (o,n,[]  ) -> return (o,n)
       (_,_,errs) -> defaultErr errs

  let mode_option o = o `elem` [Cpp, CppOld, Haskell] 
      mode = case filter mode_option opts of
               [] -> CppOld
  	       [o] -> o 
  	       ls -> defaultErr ["\nAsked to generate output in more than one format!  Not allowed presently. "++show ls++"\n"]
      verbose = Verbose `elem` opts

  let file = 
       case files of 
        [file] -> file
        []     -> defaultErr ["\nNo files provided!\n"]
        ls     -> defaultErr ["\nCurrently the translator expects exactly one input file.\n"]

  ------------------------------------------------------------	       
  forM_ opts $ \opt -> 
   case opt of 
     HarchPart file -> error "--harchpart not implemented yet"

     Version -> do printHeader
                   --putStrLn$ version
                   exitSuccess

#ifdef CNCVIZ
     HarchViz file -> 
	 do putStrLn$ "Reading (and visualizing) harch file from: "++ file
	    g <- readHarchFile file
	    simple_graphviz name g
	    putStrLn$ "Done with visualization, exiting without performing any .cnc spec translation."
	    exitSuccess
     UbigraphOpt -> 
	 do CncSpec{graph} <- readCnCFile verbose file
	    cncUbigraph True graph
	    putStrLn$ "Done with visualization, exiting without performing any .cnc spec translation."
	    exitSuccess
#endif

     m | mode_option m -> return ()
     o -> defaultErr ["Internal error: Currently unhandled option: "++ show o ++"\n"]
  ------------------------------------------------------------	       
 
  -- Force evaluation to make sure we hit the error:
  case mode of 
     Cpp -> return ()
     _   -> return ()

  ----------------------------------------------------------------------------------------------------
  -- Now do the actual translation (if we get to here):
  ----------------------------------------------------------------------------------------------------
  
  graph <- readCnCFile verbose file 
  let appname = takeBaseName file  

  case mode of
    CppOld -> 
       do let outname = takeDirectory file </> appname ++ ".h"
	  outhand <- openFile outname WriteMode
	  putStrLn$ "\nGenerating header, output to: " ++ outname
	  writeSB outhand $ (emitCppOld graph :: SimpleBuilder ())
	  hClose outhand
    Cpp ->        
       do let outname = takeDirectory file </> appname ++ ".h"
	  outhand <- openFile outname WriteMode
	  putStrLn$ "\nGenerating header, output to: " ++ outname
	  writeSB outhand $ (emitCppOld graph :: SimpleBuilder ())
	  hClose outhand
--error "New C++ API Not implemented yet!"
    Haskell -> 
       do let outname = takeDirectory file </> appname ++ "_header.hs"
	  outhand <- openFile outname WriteMode
	  putStrLn$ "\nGenerating header, output to: " ++ outname
	  writeSB outhand $ (emitHaskell graph :: SimpleBuilder ())
	  hClose outhand
   

  putStrLn "Done."




------------------------------------------------------------------------------------------------------------------------
-- Testing 

{-

testread = 
-- do file <- openFile "/Users/newton/cnc/experimental/graphPartitioner/test.harch" ReadMode 
 do file <- openFile "/Users/newton/cnc/experimental/graphPartitioner/test2.harch" ReadMode 
-- do file <- openFile "/Users/newton/cnc/experimental/graphPartitioner/outputs/pipes.harch.partitioned" ReadMode 
    txt <- hGetContents file
    let ls = run harchfile txt
    --sequence_$ map print ls

    putStrLn "\n Now partitions: \n"
    let part = extractPartitions ls
    --print part
	  
    let gr = convertHarchGraph ls

    print gr
    simple_graphviz name gr

    --sequence_$ map print ppaths
    return part

-}