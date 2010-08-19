{-# LANGUAGE CPP, NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

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
import Data.Maybe ( fromMaybe, fromJust )
import Data.IORef
import Data.List

import Control.Monad hiding (when)
import Control.Exception 
import Control.Concurrent

import System.Environment
import System.Console.GetOpt
import System.Console.ANSI
import System.FilePath.Posix
import System.IO
import System.IO.Unsafe
import System.Exit

import Intel.Cnc.Spec.TraceVacuum

-- These expand the file size quite a bit.  Not committing to include right now:
-- #define CNCVIZ
#ifdef CNCVIZ 
import Intel.Cnc.Spec.CncViz as Viz
#endif

-- TODO: It would be nice to get this from the .cabal file.
-- That would require some quasiquoting/templating or the C preprocessor.
version = "0.1.3.99"
    
data Flag 
    = Verbose Int | Version | Help | Debug | GenTracing | GenStepDefs
    | Cpp | CppOld | Haskell
 -- | Input String  | LibDir String
    | Output String
    | HarchPart String
    | HarchViz String
    | NullOpt
    | DotOpt
    | VizOpt
    | UbigraphOpt
    | VacuumViz  | Vacuum
    | SynthSpec String    
  deriving (Show, Eq)
    

run_modes :: [(String, [OptDescr Flag], String)]
run_modes = 
  [ ("translate", translate_options, "Translates .cnc specification files to host language code")
  , ("trace"    , trace_options, "Works with Traces, e.g. the output of CnC::debug::trace.\n"++
                                 " (Reads a trace from an input file, if provided, or stdin.)")
  , ("harchpart", harchpart_options, "Uses Harch, the hierarchical graph partitioner for CnC.\n"++
                                     " (This mode expects one .harch as input and produces a .part.harch)")
  ]

common_options :: [OptDescr Flag]
common_options = 
     [ Option ['V']     ["version"] (NoArg Version)   "show version number"
     , Option ['v']     ["verbose"] (OptArg (\x -> case x of Just n  -> Verbose (read n); 
					                     Nothing -> Verbose (-1)) "N") 
                                    "verbosity level {0,1,2} default 1 (-v with no arg increments)"

     , Option []     ["help"]       (NoArg Help)        "print this help information"
     ]

translate_options ::  [OptDescr Flag]
translate_options = 
     [ 
       Option []        ["cpp"]     (NoArg Cpp)          "translate spec to C++ code [default]"
     , Option ['c']     ["cppold"]  (NoArg CppOld)       "translate spec to C++ code (legacy 0.5 API)"
     , Option ['h']     ["haskell"] (NoArg Haskell)      "translate spec to Haskell code"
     , Option ['o']     ["output"]  (ReqArg Output "FILE")    "use FILE as a prefix for output header/codinghints"
     , Option []        ["harch"]   (ReqArg HarchPart "FILE") "read Harch graph metadata from FILE (used for translation)"
     , Option []        ["debug"]   (NoArg Debug)             "generate extra code for correctness checking"
     , Option []        ["trace"]   (NoArg GenTracing)        "generate code in which tracing is on by default"

--     , Option []        ["defsteps"] (NoArg GenTracing)$  "[c++] rather than the user defining custom types for each step,\n"++
--	                                                  "      emit default versions within the generated header"
--		                                          "      create a standard definition in the generate header"
     , Option []        ["defsteps"] (NoArg GenStepDefs)$ "[c++] define default step structs in the generated header \n"++
		                                          "      (rather than custom, user-defined type definitions)"

#ifdef CNCVIZ
     , Option []        []           (NoArg NullOpt)  ""
     , Option []        ["dot"]      (NoArg DotOpt)   "output CnC graph in graphviz .dot format instead of translating"
     , Option []        ["viz"]      (NoArg VizOpt)   "similar to --dot, a shortcut to visualize a CnC graph in a X11 window"
     , Option []        ["ubigraph"] (NoArg UbigraphOpt)  "like --viz, but visualize on a local Ubigraph server"
#endif

     ]

trace_options ::  [OptDescr Flag]
trace_options = 
     [ 
        Option ['o']    ["output"] (ReqArg Output "FILE") "write the captured trace to FILE in compressed binary format"
#ifdef CNCVIZ
     ,  Option []       ["viz"] (NoArg VacuumViz) "use trace to visualize graph execution using ubigraph"
#endif
     ,  Option []       ["synth"] (ReqArg SynthSpec "FILE") "use trace to synthesize a draft .cnc Spec for the program"
     ]


harchpart_options ::  [OptDescr Flag]
harchpart_options = 
     [ 
       Option ['o']     ["output"] (ReqArg Output "FILE") "output the graph-partitioned harchfile to FILE"
#ifdef CNCVIZ
     , Option []        ["viz"]    (ReqArg HarchViz "FILE")  "visualize the graph stored in FILE with Harch clustering"
#endif
     ]



printHeader = do
  putStrLn$ "Intel(R) Concurrent Collections Spec Translator, Haskell CnC Edition version "++ version
  putStrLn$ "Copyright 2010 Intel Corporation."

when b action = if b then action else return ()



------------------------------------------------------------------------------------------------------------------------
-- The translator front-end: parse a file, convert to graph:
------------------------------------------------------------------------------------------------------------------------

readCnCFile :: Int -> String -> IO CncSpec
readCnCFile verbosity file = do 
  handle <- openFile file ReadMode
  str <- hGetContents handle

  when (verbosity > 2)$ putStrLn "================================================================================"
  when (verbosity > 2)$ putStrLn "\nAll Lexed Tokens: "
  --when verbose$ print $ hcat $ intersperse (text ", ") $ map (\ (L _ cl str) -> text (show cl) <+> pp str) $ scan_to_list str
  when (verbosity > 2)$ print $ sep $ map (\ (L _ cl str) -> text (show cl) <+> pp str) $ scan_to_list str
       --filter (not . is_comment) $ scan_to_list str -- Even filtering the long lines still doesn't `sep` to do the right thing.

  let parsed = runCncParser file str

  when (verbosity > 2)$ putStrLn "================================================================================"
  when (verbosity > 2)$ putStrLn "\nParsed AST (detailed):"
  when (verbosity > 2)$ sequence_ $ map (print . stripDecor) parsed

  -- when verbose$ putStrLn "\nParsed AST rendered as a SExp:"
  -- when verbose$ putStrLn "================================================================================"
  -- when verbose$ sequence_ $ map (\stmt -> putStrLn $ buildList $ sexpSerialize stmt) parsed

  when (verbosity > 1)$ putStrLn "================================================================================"
  when (verbosity > 1)$ putStrLn "\nPretty printed parsed AST:"
  when (verbosity > 1)$ putStrLn$ renderStyle style $ hcat $ map pPrint parsed

  -- [2010.07.23] Lazy parsing complicates this, it must happen after IO that touches the parse:
  evaluate (length str)
  hClose handle -- Cleaner to do this than to wait for garbage collection.

  when (verbosity > 1)$ putStrLn "================================================================================"
  when (verbosity > 1)$ putStrLn "\nCoalesced CnC Graph:"
  -- The name of the module is derived from the file name:	   
  let appname = takeBaseName file
      graph = coalesceGraph appname parsed

  when (verbosity > 1)$ putStrLn ""
  when (verbosity > 1)$ print $ pp graph

  when (verbosity > 1)$ putStrLn "================================================================================"
  return graph


  
------------------------------------------------------------------------------------------------------------------------
main = 
 do 
--    putStrLn "TEMP PRINTING STRINGMAP"
--    print Viz.foo
    argv <- getArgs
    main2 argv

baseline = [SetColor Background Dull Black]
withCol viv col act = 
    do setSGR [SetColor Foreground viv col]
       act 
       setSGR []
       --setSGR baseline

main2 argv = do  
  let 

      print_usage_common = 
         do withCol Vivid Black$ putStr$ "Common Options (all modes):"
            putStr$ usageInfo "" common_options 		    

      mode_usage (mode, opts, help) = do 
	    withCol Dull Green$ putStr$ "\n '"++ mode ++"' mode:\n "
            --withCol Dull Cyan  $ putStr$ help ++":\n"
	    withCol Vivid Black  $ putStr$ help ++":\n"
            withCol Dull Red   $ putStr$ take 80$ repeat '-'
	    putStr$ usageInfo ""  opts
            setSGR []

      print_usage_all = 
       do withCol Dull Green$ putStr$ "Usage: "++hcnc_name++" mode [OPTION...] files..." 
	  putStr$ "\n\n"
	  print_usage_common
          forM_ run_modes mode_usage

      defaultErr mode errs = 
       do 
          --setSGR baseline
          withCol Vivid Red$ putStr$ "ERROR!  " ++ (if null mode then "" else "("++hcnc_name++" "++mode++" mode)  ") ++errs++"\n"
	  print_usage_all
          putStrLn ""
          error $ "ERROR!  "

      -- simpleErr mode msg = do 
      -- 	 withCol Vivid Red$ 
      -- 	   case mode of 
      -- 	     "" -> 

      simpleErr "" msg = error$ "ERROR!\n  " ++ msg
      simpleErr mode msg = error$ "ERROR! ("++hcnc_name++" "++ mode ++" mode)\n  " ++ msg

  ----------------------------------------------------------------------------------------------------
  -- Read and process option flags:
  ----------------------------------------------------------------------------------------------------


  -- Do a little pre-processing of the opts so we can catch --version:
  let (common_opts,_,_) = getOpt Permute common_options argv
  when (Version `elem` common_opts) $  do printHeader; exitSuccess

  when (null argv) $ defaultErr ""$ "First argument to "++hcnc_name++" must specify a mode!\n"
  
  let helpmode = Help `elem` common_opts
      (first:__rest) = argv
      (__mode, __mode_opts, _) = case filter (isPrefixOf first . fst3) run_modes of  
	       [] | helpmode -> unsafePerformIO$ do { print_usage_all; exitSuccess }
	       []  -> simpleErr ""$ first ++ " does not correspond to any "++hcnc_name++" mode\n"
	       [m] -> m
	       ls  -> simpleErr ""$ "Prefix '"++first++"' could refer to multiple modes:  "
	                          ++ concat (intersperse ", "$ map fst3 ls)
      verbosity = foldl (\ lvl opt -> case opt of Verbose (-1) -> lvl+1; Verbose n -> n; _ -> lvl) 1 common_opts
      -- This is annoying but I can't think of a systematic way:							      
      uncommmon (Verbose _) = False
      uncommmon (Version  ) = False
      uncommmon (Help     ) = False
      uncommmon _           = True

      -- HACKish SHORTCUT to make things easy for people.  A ".cnc" file is allowed as the first argument.
      (mode, mode_opts, rest) = 
	if takeExtension first == ".cnc" 
	   -- Switch us implicitly into translate mode:
	then let Just (a,b,_) = find ((== "translate") . fst3) run_modes in 
	     (a,b, argv)
	else (__mode, __mode_opts, __rest)

  evaluate mode -- Force the above unsafePerformIO 

  (opts,files) <- 
     --case getOpt' Permute (mode_opts) rest of
     -- Need common options to avoid error messages:
     case getOpt' Permute (common_options ++ mode_opts) rest of 
       (o,n,[], [] ) -> return (filter uncommmon o,n)
       (o,n,bad,[] ) -> simpleErr ""$ "Unrecognized options in "++mode++" mode: "++ concat (intersperse " " bad)
       (_,_,_,errs)  -> simpleErr mode$ concat errs

  when helpmode $ 
    do --putStrLn "Mode specific options:"
       let do_mode = mode_usage$ fromJust$ find ((== mode) . fst3) run_modes 
       case mode of 
         "harchpart" -> do_mode
         "trace"     -> do_mode
         "translate" -> do_mode
         _ -> error "Really egregious internal error"
       putStrLn ""
       print_usage_common
       exitSuccess

  ------------------------------------------------------------	       
  case mode of 

   "harchpart" -> do 
      forM_ opts $ \ opt -> 
        case opt of 
#ifdef CNCVIZ
	 HarchViz file -> 
	     do putStrLn$ "Reading (and visualizing) harch file from: "++ file
		g <- readHarchFile file
		simple_graphviz name g
		putStrLn$ "Done with visualization, exiting without performing any .cnc spec translation."
		exitSuccess
#endif
	 Output file -> error "Internal error, trace output not implemented yet"
	 _ -> error$ "Internal error, not handled: "++ show opt

      -- HarchPart file -> 
      error "harchpart not implemented yet"
      return ()

   ------------------------------------------------------------	       
   "trace" -> do 
      forM_ opts $ \ opt -> 
        case opt of 
#ifdef CNCVIZ
	 VacuumViz -> 
	     do handle <- if null files 
			    then do putStrLn$ cnctag++"Reading trace from stdin."
				    return stdin 
			    else do putStrLn$ cnctag++"Reading trace from file "++ show(head files)
			            (openFile (head files) ReadMode)
		str <- hGetContents handle
		let thetrace = tracefile $ lines str
		    guiactions = traceToGUI thetrace

		when (verbosity>1) $ do (forkIO $ mapM_ print guiactions); return ()

		playback emptyGUIState guiactions
		 
		putStrLn$ "Done with trace visualization.  Exiting."
		exitSuccess
#endif
	 SynthSpec file -> error "Internal error, spec synthesis not implemented yet"
	 Output    file -> error "Internal error, trace output not implemented yet"

	 _ -> error$ "Internal error, not handled: "++ show opt

      defaultErr mode "Trace mode given nothing to do!"

   ------------------------------------------------------------	       
   "translate" -> do 
      let codegenmode_option o = o `elem` [Cpp, CppOld, Haskell] 
	  codegenmode = case filter codegenmode_option opts of
               [] -> Cpp
  	       [o] -> o 
  	       ls -> simpleErr mode ("\nAsked to generate output in more than one format!  Not allowed presently. "++show ls++"\n")
	  file = case files of 
	           [file] -> file
		   []     -> simpleErr mode$ "\nNo files provided!\n"
		   ls     -> simpleErr mode$ "\nCurrently the translator expects exactly one input file, got "
			                     ++show (length ls)++":\n  " ++ concat (intersperse ", " ls) ++ "\n"

      -- Force evaluation to make sure we hit the error:
      case codegenmode of 
	 Cpp -> return ()
	 _   -> return ()

      -- Process options for this mode:
      forM_ opts $ \opt -> do {
       case opt of 

#ifdef CNCVIZ
	 UbigraphOpt -> 
	     do CncSpec{graph} <- readCnCFile verbosity file
		cncUbigraph True graph
		putStrLn$ "Done with visualization, exiting without performing any .cnc spec translation."
		exitSuccess
#endif

         Help        -> error "unimplemented"
         Debug       -> error "unimplemented"
         GenTracing  -> error "unimplemented"
         GenStepDefs -> return() --error "unimplemented"

	 m | codegenmode_option m -> return ()
	 o -> simpleErr mode ("Internal error: Currently unhandled option: "++ show o ++"\n")
      }

      ------------------------------------------------------------
      -- Now do the actual translation (if we get to here):
      ------------------------------------------------------------

      graph <- readCnCFile verbosity file 
      let appname = takeBaseName file  
	  base = takeDirectory file </> appname 
	  toFile outname msg str = 
	   do outhand <- openFile outname WriteMode
	      when (verbosity>0)$ putStrLn$ cnctag++"Generating "++msg++", output to: " ++ outname
	      writeSB outhand $ str
	      hClose outhand

      case codegenmode of
	CppOld -> 
	   toFile (base ++ ".h") "header (legacy CnC 0.5 API)"
		  (emitCpp True (GenStepDefs `elem` opts) graph :: SimpleBuilder ())

	Cpp ->        
	   toFile (base ++ ".h") "header"
		  (emitCpp False (GenStepDefs `elem` opts) graph :: SimpleBuilder ())

	Haskell -> 
	   toFile (base ++ "_heaader.hs") "header"
		  (emitHaskell graph :: SimpleBuilder ())

	_ -> error$ "Not a codegen mode: "++ show codegenmode

   _ -> error "Really egregious internal error"
  --------------------------------------------------------------------------------
  -- Finished with mode dispatch
  --------------------------------------------------------------------------------
  when (verbosity>0)$ putStrLn$ cnctag++"Done."


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