{-# LANGUAGE CPP, NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import Intel.Cnc.Spec.CncLexer hiding (main)
import Intel.Cnc.Spec.CncGrammar
import Intel.Cnc.Spec.SrcLoc
import Intel.Cnc.Spec.CncGraph
import Intel.Cnc.Spec.GatherGraph
import Intel.Cnc.Spec.Util hiding(t)
import Intel.Cnc.Spec.TraceVacuum

import Intel.Cnc.Spec.Codegen.CodegenShared
import Intel.Cnc.Spec.Codegen.CppOld


import qualified  Intel.Cnc.EasyEmit as EE -- TEMPTOGGLE

import Intel.Cnc.Spec.Codegen.Haskell
import Intel.Cnc.Spec.Passes.TypeDef
import Intel.Cnc.Spec.Passes.ReadHarch
import Intel.Cnc.Spec.Version

import Data.Maybe ( fromJust )
import qualified Data.Map as M
import Data.List
--import Data.Function

import Control.Monad hiding (when)
import Control.Exception 

import System.Environment
import System.Console.GetOpt
import System.Console.ANSI
import System.FilePath.Posix
import System.IO
import System.IO.Unsafe
import System.Exit

import Text.Printf
import Text.PrettyPrint.HughesPJClass
import Test.HUnit
import Debug.Trace

-- These expand the file size quite a bit.  Not committing to include right now:
-- #define CNCVIZ
#ifdef CNCVIZ 
import Intel.Cnc.Spec.CncViz as Viz
#endif

----------------------------------------------------------------------------------------------------

data Flag 
    = Verbose Int | Version | Help | Debug 
    | GenTracing | GenDepends | NoStepDefs | SelfTest
    | AutoDone | AutoDoneDbg
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
--                                     " (This mode expects one .harch as input and produces a .part.harch)")
                                     " (This mode enriches a .harch file with partition info.)")
  ]

common_options :: [OptDescr Flag]
common_options = 
     [ Option ['V']     ["version"] (NoArg Version)   "show version number"
     , Option ['v']     ["verbose"] (OptArg (\x -> case x of Just n  -> Verbose (read n); 
					                     Nothing -> Verbose (-1)) "N") 
                                    "verbosity level {0,1,2} default 1 (-v with no arg increments)"
     , Option []     ["selftest"]   (NoArg SelfTest)    "run internal unit tests"
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
     , Option []        ["tracing"] (NoArg GenTracing)        "generate code in which tracing is on by default"
     , Option []        ["depends"] (NoArg GenDepends)        "generate depends functions from spec where possible"
    , Option []        ["autodone"] (NoArg AutoDone)          "track collection completion and signal reduction-completion automatically"
    , Option []        ["autodonedbg"] (NoArg AutoDoneDbg)    "print additional messages for debugging autodone facility"

     -- , Option []     ["autodone"] (OptArg 
     -- 				   (\ arg -> case arg of 
     -- 				              Just str -> trace str $ AutoDone
     -- 				              Nothing -> AutoDone)
     -- 				   "mode")
     -- 		                  "track collection completion and signal reduction-completion automatically"

--     , Option []        ["defsteps"] (NoArg GenTracing)$  "[c++] rather than the user defining custom types for each step,\n"++
--	                                                  "      emit default versions within the generated header"
--		                                          "      create a standard definition in the generate header"

     -- , Option []        ["defsteps"] (NoArg GenStepDefs)$ "[c++] define default step structs in the generated header \n"++
     -- 		                                          "      (rather than custom, user-defined type definitions)"

     , Option []        ["customsteps"] (NoArg NoStepDefs)$ "[c++] Don't define default step structs in the generated header \n"++
		                                              "      (Instead the user provides custom type definitions.)"


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
     ,  Option []       ["debug"] (NoArg Debug)             "print parsed trace to validate parsing"
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
--  putStrLn$ "Intel(R) Concurrent Collections Specification Tool, version "++ version
  putStrLn$ "The fabulous, multi-purpose CnC Specification Tool, version "++ version
  putStrLn$ "Part of the Intel(R) Concurrent Collections (CnC) Project"
  putStrLn$ "Built on: " ++ builddate
  putStrLn$ "Copyright 2010 Intel Corporation."

when b action = if b then action else return ()


------------------------------------------------------------------------------------------------------------------------
-- The translator front-end: parse a file, convert to graph:
------------------------------------------------------------------------------------------------------------------------

-- This invokes the parser and creates the graph coelescence.

readCnCFile :: Int -> String -> IO CncSpec
readCnCFile verbosity file = do 
  handle <- openFile file ReadMode
  str <- hGetContents handle

  when (verbosity > 2)$ putStrLn "\n                            All Lexed Tokens "
  when (verbosity > 2)$ putStrLn "================================================================================"

  --when verbose$ print $ hcat $ intersperse (text ", ") $ map (\ (L _ cl str) -> text (show cl) <+> pp str) $ scan_to_list str
  when (verbosity > 2)$ print $ sep $ map (\ (L _ cl str) -> text (show cl) <+> pp str) $ scan_to_list str
       --filter (not . is_comment) $ scan_to_list str -- Even filtering the long lines still doesn't `sep` to do the right thing.

  let parsed = runCncParser file str

      ----------------------------------------
      -- Pass 1: desugar type
      p1 = desugarTypeDefs parsed

      ----------------------------------------
      final_statements = p1

  ----------------------------------------
  when (verbosity > 2)$ putStrLn "\n                          Parsed AST (detailed)"
  when (verbosity > 2)$ putStrLn "================================================================================"
  when (verbosity > 2)$ sequence_ $ map (print . stripDecor) parsed

  -- when verbose$ putStrLn "\nParsed AST rendered as a SExp:"
  -- when verbose$ putStrLn "================================================================================"
  -- when verbose$ sequence_ $ map (\stmt -> putStrLn $ buildList $ sexpSerialize stmt) parsed

  when (verbosity > 1)$ putStrLn "\n                         Pretty printed parsed AST"
  when (verbosity > 1)$ putStrLn "================================================================================"
  when (verbosity > 1)$ putStrLn$ renderStyle style $ hcat $ map pPrint parsed

  -- [2010.07.23] Lazy parsing complicates this, it must happen after IO that touches the parse:
  evaluate (length str)
  hClose handle -- Cleaner to do this than to wait for garbage collection.

  when (verbosity > 1)$ putStrLn "                            Coalesced CnC Graph"
  when (verbosity > 1)$ putStrLn "================================================================================"
  -- The name of the module is derived from the file name:	   
  let appname = takeBaseName file
      graph = coalesceGraph appname final_statements

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
  when (SelfTest `elem` common_opts) $ 
    do cncRunAllTests
       exitSuccess

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
      uncommmon (SelfTest ) = False
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
		spec@(HarchSpec g _) <- readHarchFile file
--		simple_graphviz name g
		harch_graphviz name spec
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
      handle <- if null files 
		then do putStrLn$ cnctag++"Reading trace from stdin."
			return stdin 
		else do putStrLn$ cnctag++"Reading trace from file "++ show(head files)
			(openFile (head files) ReadMode)
      str <- hGetContents handle
      let thetrace = tracefile $ lines str
	  fn deb opt = 
            case opt of 
	       VacuumViz      -> deb
	       Debug          -> True
	       SynthSpec file -> error "Internal error, spec synthesis not implemented yet"
	       Output    file -> error "Internal error, trace output not implemented yet"
	       _ -> error$ "Internal error, not handled: "++ show opt
          debug = foldl fn False opts
	  alldone = do putStrLn$ cnctag++"Reached end of trace.  Exiting."
		       exitSuccess

      -- TEMPTOGGLE -- this is just for testing:
      when (debug) $ do
	    let fn (GetI _ (_,tag)) = map fst (reads tag :: [((Int,Int,Int),String)])
		fn _ = [] 

                -- TEMP: TODO: GENERALIZE
                getname (GetI _ (nm,_)) = [nm]
		getname _ = []
                names = concat$ map getname thetrace

                tags = concat$ map fn thetrace


		collected = foldl (\ acc tup -> M.insertWith (+) tup 1 acc) M.empty tags
		keys = M.keys collected
		bounds keys = ( foldl1 min $ map fst3 keys
			      , foldl1 min $ map snd3 keys
			      , foldl1 min $ map thd3 keys
			      , foldl1 max $ map fst3 keys
			      , foldl1 max $ map snd3 keys
			      , foldl1 max $ map thd3 keys)
		(l1,l2,l3,u1,u2,u3) = bounds keys
		biggest = foldl1 max $ M.elems collected
		-- Amount of space needed for biggest number:
		size = 1 + (ceiling$ log (fromIntegral biggest) / log 10.0)
		padtail size str = str ++ take (size - length str) (repeat ' ') 

            print collected
	    putStrLn$ "LOWER BOUNDS: " ++ show (l1,l2,l3)
	    putStrLn$ "UPPER BOUNDS: " ++ show (u1,u2,u3)

	    putStrLn$ "\n\nTags for collection " ++ show (names !! 3) ++ " have schema (Int,Int,Int)."
	    putStrLn$ "Observed getcounts were:"
            forM_ [l3..u3] $ \k -> do
	        let (i1,i2,_,j1,j2,_) = bounds $ filter ((== k) . thd3) keys
		--putStrLn$ "\n i/j getcounts grid for k = " ++ show k ++ " i in ["++ show l1 ++","++ show u1 ++"], j in ["++ show l2 ++","++ show u2 ++"]:"
	        printf "\n i/j getcounts grid for k = %d,  i in [%d,%d], j in [%d,%d]:\n" k i1 i2 j1 j2
		putStr$ "+-------------------"
		forM_ [l1..u1] $ \i -> do
		    putStr$ "\n|"
		    forM_ [l2..u2] $ \j -> do
		      case M.lookup (i,j,k) collected of 
			Nothing -> putStr$ take size (repeat ' ')
			Just n  -> putStr$ padtail size (show n)
		putStrLn$ ""
	    putStrLn$ "\n"

	    --mapM_ print tags
	    --mapM_ print (sortBy (compare `on` snd) $ M.toList collected)

	    alldone

      -- In debugging mode we just print the parsed trace:
      when (debug) $ do
	    mapM_ print thetrace
	    alldone

#ifdef CNCVIZ
      when (VacuumViz `elem` opts) $ do
	    let guiactions = traceToGUI thetrace

	    -- This prints it out ASAP so it doesn't follow the actual playback unfortunately...
	    -- TODO: Push this functionality down into "playback".
	    when (verbosity>1) $ do (forkIO $ mapM_ print guiactions); return ()
	    when (verbosity>1) $ do (forkIO $ mapM_ print thetrace); return ()

	    playback emptyGUIState guiactions
	    alldone
#endif


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
      config <- foldM (\ cfg opt -> do 
       case opt of 
         Help        -> return cfg
#ifdef CNCVIZ
	 UbigraphOpt -> 
	     do CncSpec{graph} <- readCnCFile verbosity file
		cncUbigraph True graph
		putStrLn$ "Done with visualization, exiting without performing any .cnc spec translation."
		exitSuccess
#endif
         Debug       -> return cfg{ gendebug=True, wrapall=True }
         GenTracing  -> return cfg{ gentracing=True } 
         GenDepends  -> return cfg{ gendepends=True } 
         AutoDone    -> return cfg{ plugins= reductionDonePlugin False : plugins cfg } 
         AutoDoneDbg -> return cfg{ plugins= reductionDonePlugin True : plugins cfg } 
         NoStepDefs  -> return cfg{ genstepdefs=False } 

	 m | codegenmode_option m -> return cfg
	 o -> simpleErr mode ("Internal error: Currently unhandled option: "++ show o ++"\n")
       ) default_codegen_config opts

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
		  (emitCpp config graph :: EE.EasyEmit ())
--True (GenStepDefs `elem` opts)
	Cpp ->        
	   toFile (base ++ ".h") "header"
		  (emitCpp config graph :: EE.EasyEmit ())
--False (GenStepDefs `elem` opts)

	Haskell -> 
	   toFile (base ++ "_heaader.hs") "header"
		  (emitHaskell graph :: EE.EasyEmit ())

	_ -> error$ "Not a codegen mode: "++ show codegenmode

   _ -> error "Really egregious internal error"
  --------------------------------------------------------------------------------
  -- Finished with mode dispatch
  --------------------------------------------------------------------------------
  when (verbosity>0)$ putStrLn$ cnctag++"Done."


------------------------------------------------------------------------------------------------------------------------
-- Testing 
------------------------------------------------------------------------------------------------------------------------

-- Aggregate the unit tests from all the modules that provide them:
all_unit_tests = 
 --test $ 
 TestLabel "All cnc tool unit tests" $ TestList $
 [ test_desugarTypeDefs
 , test_traceVacuum
 , EE.tests_easyemit
 , test_readharch
 , tests_codegenshared
 ]

cncRunAllTests = 
--    runTestText (putTextToHandle stdout True) all_unit_tests
    runTestText (PutText myPut ()) all_unit_tests
 where myPut msg True  () = do putStrLn ""; putStrLn$ msg
       myPut msg False () = return ()

t = cncRunAllTests -- Lazy shorthand

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