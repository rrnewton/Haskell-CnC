{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- UNFINISHED!!!  

{- HOWTO RUN (current as of [2011.01.23]):
   ---------------------------------------
   For now this is an executable entrypoint itself (Main).

     runhaskell Intel/Cnc/BenchSynth.hs

   The resulting file can be built with:

     g++ -ltbb -ltbbmalloc test.cpp -o test.exe

   Assuming you've got the environment set up right for tbb....
       
 -} 
----------------------------------------------------------------------------------------------------


-- module Intel.Cnc.BenchSynth where
module Main where

{- 
   The goal of this utility is to generate graph benchmarks with a certain set of features.

   This encompasses both kernel generation and graph generation.
 -}

import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.Util hiding (app)
import Intel.Cnc.Spec.MainExecutable as Comp
import Intel.Cnc.Spec.Codegen.CodegenShared
import Intel.Cnc.Spec.Codegen.CppOld

import Control.Monad
-- import Data.ByteString.Char8 hiding (putStrLn)
import Text.PrettyPrint.HughesPJ
import Text.PrettyPrint.HughesPJClass 
import StringTable.Atom

--import GHC.Exts -- For IsString

-- FGL graphs:
import qualified Data.Graph.Inductive as G
import qualified Intel.Cnc.EasyEmit as EE
-- import Intel.Cnc.EasyEmit hiding (app, not, (&&), (==), (||))
import Intel.Cnc.EasyEmit hiding (not, (&&), (==), (||))

import Prelude hiding ((<), (>=))

-- type StringT = ByteString
type StringT = String

----------------------------------------------------------------------------------------------------

-- Rough draft #1:

-- A kernel takes an argument as input and produces a body given 
data Kernel = Krn { arg :: Doc
		  , body :: Emitter -> Doc
		  }

-- Could make these safer if desired:
type NodeName    = Atom
type CommandSnip = Doc
type ExprSnip    = Doc

-- For each backend, the Emitter produces output to a given stream.
-- Given a destination, a expression codesnippet (argument), it
-- produces a command codesnippet.
type Emitter = NodeName -> ExprSnip -> CommandSnip

data GraphNode = ComputeNode Kernel
	       | SharedMemNode

-- A graph consists of compute nodes and shared memory nodes
-- (e.g. steps and item collections).
type BenchGraph = G.Gr NodeName GraphNode

----------------------------------------------------------------------------------------------------
-- Rough draft #2

-- Doing chains of reductions first, then we'll try to generalize from there.

-- The end goal is a convenient set of combinators that allow the
-- description of arbitrary benchmark generators.  Then on top of that
-- will be randomized/automatic benchmark generation.


data TBBBenchConfig = TBBBenchConfig
      { freeReduces :: Bool 
      }
  deriving Show

default_tbb_bench_config = 
  TBBBenchConfig
     {  freeReduces = False
     }


debug_benchsynth = True

----------------------------------------------------------------------------------------------------
-- Reusable infrastructure -- not backend-specific.
----------------------------------------------------------------------------------------------------

type VarName = Syntax

-- A numeric computation on a single scalar.
-- Takes the name of a variable.  
-- Emits a block of code and returns the name of a variable that contains the result.
-- data ScalarKernel = ScalarKernel (Syntax -> EasyEmit Syntax)
type ScalarKernel  = (VarName -> EasyEmit VarName)
type ScalarKernel2 = (VarName -> VarName -> EasyEmit VarName)

-- Perform approximately n flops (where n is an expression returning int).
simpleFlops :: Type -> Syntax -> ScalarKernel
simpleFlops ty n = 
  -- ScalarKernel $ 
  \ inp -> do 
    tmp <- tmpvarinit ty inp
    comm$ " Simple "++ synToStr n ++" FLOPs kernel, input "++ 
	   synToStr inp ++" output "++ synToStr tmp
--    forLoopSimple (0, fromInt (n `quot` 2)) $ \ ind -> do
    forLoopSimple (0, n/2) $ \ ind -> do
      set tmp ("2.0000001" * tmp)
      set tmp (tmp - inp)
    return tmp

-- A version with two arguments:
simpleFlops2 :: Type -> Syntax -> ScalarKernel2
simpleFlops2 ty n = 
  \ x y -> 
   do comm$ "Simple TWO argument "++ synToStr n ++" FLOPs kernel (combining two separate kernels):"
      x1 <- simpleFlops ty (n/2) x
      y1 <- simpleFlops ty (n/2) y
      comm$ "END TWO argument kernel, output = " ++ synToStr (x1 + y1)
      return (x1 + y1)

-- funappKern  f x   = function f [x]
-- funappKern2 f x y = function f [x,y]

noopKern :: ScalarKernel
noopKern x = return x

-- Use a for loop to go over a blocked range:
iteratorFor range = forLoopSimple (range `dot` "begin()", range `dot` "end()") 

-- | Put together a complete benchmark .cpp file.  
--   The interface is a bit weird, taking in a function to produce
--   top-level code and another to produce code inside the main
--   method, BUT with a value communicated between these two pieces of
--   code.
makeBenchFile :: StringT -> (Int,Int,Int) -- length, width flops
	      -> ((Syntax, Syntax, Syntax) -> EasyEmit a)
	      -> ((Syntax, Syntax, Syntax, Syntax) -> a -> EasyEmit ())
	      -> EasyEmit ObjFun
makeBenchFile header (_numstages, _width, _flops) restfile mainbody  =
 do 
    putS header
    comm "Create global variables for benchmark dimensions/parameters:"
    threads <- var TInt "threads"
    length  <- var TInt "length"
    width   <- var TInt "width"
    flops   <- var TInt "flops"
    pkg <- restfile (length, width, flops)

    putS ""
    funDef TInt "main" [TInt, TPtr$ TPtr TChar] $ \ (argc :: Syntax, argv :: Syntax) -> do
      comm "Parse command line arguments:"
      if_ (argc >= 5)
	  (do app (function "printf") [stringconst "Reading command line arguments: threads, length, width, op flops\n"]
              set threads$ function "strtol" [argv `arrsub` 1, 0, 10]
	      set length $ function "strtol" [argv `arrsub` 2, 0, 10]
	      set width  $ function "strtol" [argv `arrsub` 3, 0, 10]
	      set flops  $ function "strtol" [argv `arrsub` 4, 0, 10])
	  (do app (function "printf") [stringconst "No command line arguments: using default benchmark params.\n"]
	      set threads $ 1
	      set length  $ fromInt _numstages
	      set width   $ fromInt _width
	      set flops   $ fromInt _flops)

      mainbody (threads, length, width, flops) pkg



----------------------------------------------------------------------------------------------------
-- TBB backend, generate code to construct a TBB graph and TBB execute methods.
----------------------------------------------------------------------------------------------------

-- Emit an entire graph as the complete text of a .cpp file.
tbbEmitGraph :: GraphNode -> Doc 
tbbEmitGraph = 
   undefined


-- Create code for a single kernel
tbbEmitKernel :: Kernel -> Doc 
tbbEmitKernel = 
   undefined

--------------------------------------------------------------------------------

-- rTy = TFloat
rTy = TDouble -- TODO: generalize
vTy = tyConstructor "concurrent_vector" rTy
tname_reduce  = "Reducer"
tname_produce = "Producer"


rangeTy = tyConstructor "blocked_range" TInt

tbb_parProduceStruct :: Syntax -> ScalarKernel2 -> EasyEmit ()
tbb_parProduceStruct tname kernel =  do     
    cppStruct tname "" $ do
      prev   <- var rTy "prev"
      outbuf <- var (TPtr vTy) "outbuf"

      constFunDef voidTy "operator()" [TConst$ TRef rangeTy] $ \ range -> do
	iteratorFor range $ \ i -> do
	   result <- kernel i prev
	   arrset (dereference outbuf) i result

      return ()

tbb_parProduceFun :: Syntax -> EasyEmit ObjFun
tbb_parProduceFun tname_produce = do
  comm "This stage expands the single reduced scalar from the previous reduce "
  comm "stage back into an array of values."
  funDef rTy "ParProduceStage" [TInt, TRef rTy, TRef vTy] $ \ (size, inp, outp) -> do
    app (function$ outp `dot` "grow_to_at_least") [size]
    producer <- tmpvar (litType tname_produce)
    fieldset producer "prev" inp
    fieldset producer "outbuf" (addressOf outp)
    app (function "parallel_for")
	[function "blocked_range<int>" [0, methcall outp "size" []],
	 producer]

----------------------------------------

-- Create the struct encapsulating the reduction operator for use with parallel_reduce:
tbb_parReduceStruct :: Syntax -> ScalarKernel2 -> EasyEmit ()
tbb_parReduceStruct tname kernel = 
 do {
    cppStruct tname "" $ do
      val  <- var rTy "value"
      inp  <- var (TPtr vTy) "input_vals"
      outp <- var (TPtr vTy) "output_vals"
      
      cppConstructor tname [] [("value",0)] $ return ()
      cppConstructor tname [(TRef$ litType tname, "s"),
			   (TSym "split", "_")] -- Ignored
			  [("value",0)] $ 
        do set val 0
	   set inp $ "s" `dot` inp
	   set outp$ "s" `dot` outp

      funDef voidTy "operator()" [TConst$ TRef rangeTy] $ \ range -> do
	tmp <- tmpvar rTy
	set tmp val
	forLoopSimple (range `dot` "begin()", range `dot` "end()") $ \ i -> do 
	   result <- kernel tmp (dereference inp `arrsub` i)
	   set tmp result
	set val tmp

      funDef voidTy "join" [TRef$ litType tname] $ \ arg -> do
	result <- kernel val (arg `dot` val)
	set val result
      return ()
  }

-- Create the function that calls parallel_reduce
--parReduceFun :: Syntax -> EasyEmit ((Syntax,Syntax) -> EasyEmit Syntax)
tbb_parReduceFun :: Syntax -> EasyEmit ObjFun
tbb_parReduceFun tname = 
    funDef voidTy "ParReduceStage" [TRef vTy, TRef rTy] $ \ (inp, outp) -> 
     do 
        total <- tmpvar (litType tname)
        set (total `dot` "input_vals")  (addressOf inp)
	let size = inp `dot` "size()"
	app (function "parallel_reduce")
	    [function (Syn$ pPrint rangeTy) [0,size] , total]

	final <- tmpvarinit rTy$  total `dot` "value" / size
        set outp final

tbb_header :: StringT
tbb_header = "\
 \ #include <stdio.h>                      \n\
 \ #include <iostream>                     \n\
 \ #include \"tbb/task_scheduler_init.h\"  \n\
 \ #include \"tbb/parallel_reduce.h\"      \n\
 \ #include \"tbb/parallel_for.h\"         \n\
 \ #include \"tbb/blocked_range.h\"        \n\
 \ #include \"tbb/concurrent_vector.h\"    \n\
 \ using namespace tbb;                    \n\
 \ \n"

-- Execute a linear chain of reductions:
tbb_produceReduceChain dofree (length, width, flops) 
		   prodFun redFun  =
   do 
      let printf = function "printf"
      app printf [stringconst "Running pipe of length %d, width %d, op flops %d\n", 
			       length, width, flops]
      app printf [stringconst "------------------------------------------------------------\n"]
      
      comm "Create a series of reduction steps, separated by buffers, as well"
      comm "as 'spreader' production steps that fan the reduced values back out."

      val0 <- varinit (rTy) "val0" 1 
      forLoopSimple (0, length) $ \i -> do
        when debug_benchsynth $ 
	    app printf [stringconst "Round %d, initial value = %lf\n", i, val0]
      	intmdt <- varinit (TPtr vTy) "intermediate_buf" (new vTy [])
	app prodFun [width, val0, dereference intmdt]

        when debug_benchsynth $ 
	  do app printf [stringconst "Round %d, buf: [ ", i]
             forLoopSimple (0, width) $ \j -> do 
	       app printf [stringconst "%lf ", (dereference intmdt) `arrsub` j]
	     app printf [stringconst "]\n"]

        val1   <- var (rTy) "val1" 
	app redFun  [dereference intmdt, val1]
	when dofree $ app (function "delete") [intmdt]
	set val0 val1

      putS$ "printf(\"Final spot check sum: %lf\\n\", "++ synToStr val0 ++");"
      ret 0



-- Temp: run everything and generate one benchmark config to a file:
genTbbFile filename = 
   Prelude.writeFile filename $
    render$
    execEasyEmit$
     makeBenchFile tbb_header (10,10,10) -- Defaults if no command line options are provided.
	      (\ (length, width, flops) -> 
		 do tbb_parReduceStruct tname_reduce (simpleFlops2 rTy flops);     putS ""
		    redfn <-  tbb_parReduceFun tname_reduce ; 	                  putS ""
		    tbb_parProduceStruct tname_produce (simpleFlops2 rTy flops);   putS ""
		    prodfn <-  tbb_parProduceFun tname_produce;                    putS ""
		    return (prodfn,redfn)
	      )
	      (\ (threads, length, width, flops) (prodfn,redfn) -> do
		 tmpclassvar (TSym "task_scheduler_init") [threads]
		 app (function "printf") [stringconst "Initialized num threads to %d\n", threads]
		 tbb_produceReduceChain False (length, width, flops) prodfn redfn
	      )

-- Temporary entrypoint:
main = 
  do putStrLn$ "Generating files"
     genTbbFile "test_tbb.cpp"
     genCnCFile "test_cnc.cpp"
     putStrLn$ "Finished generating"


----------------------------------------------------------------------------------------------------
-- CnC backend
----------------------------------------------------------------------------------------------------
-- UNFINISHED:

-- Here we would like to reuse the main spec compiler code to do some of the work for us.

genCnCFile filename = do 
   cnc_header <- readFile "./Intel/Cnc/BenchSynth/cnc_reduce_header.cpp"
   spec <- cnc_spec
   Prelude.writeFile filename $
     render$
     execEasyEmit$ do 
       comm "DO NOT MODIFY -- GENERATED CnC code\n"
       makeBenchFile  (cnc_header)
	       (10,10,10) -- Defaults if no command line options are provided.
	       (\ (length, width, flops) -> 
		  do 
		     comm ""
		     comm "First declare some types/functions that are needed *before* the spec:"
		     cnc_reduceOp flops
		     comm ""
		     comm "And here's an unfortunate (hopefully temporary) global variable:" 
		     var rTy "prev"
		     comm ""
		     comm "Next emit code for the CnC spec in the usual way:"
		     comm "(But inline it right into this merged benchmark file.)"
		     comm "================================================================================"
		     emitCpp default_codegen_config spec
		     comm ""
		     comm "================================================================================"
                     comm "Next add the step implementations."
		     cnc_parProduceReduceStep (simpleFlops2 rTy flops)
		     return ()
	       )
	       (\ (threads, length, width, flops) () -> 
		  -- tbb_produceReduceChain False (length, width, flops) prodfn redfn
		  
		  return ()
	       )

-- cnc_parProduceReduceStep :: Syntax -> EasyEmit ObjFun
-- cnc_parProduceReduceStep tname_produce = do
cnc_parProduceReduceStep kernel = do 
  putS ""
--  cppStruct "S1" "" $ do     
  putS "template< typename ctxt >"
  constFunDef TInt "S1::execute" [TConst$ TRef$ TInt, TRef$ TSym "ctxt"] $ 
      \ (ind :: Syntax, ctxt :: Syntax) -> 
      do          
	 result <- kernel ind "prev" 
	 -- reduc_scalar_t result = simpleFlops2(n, n, c.prev);

         -- CONUNDRUM: How do test the strategy of NOT using the inefficient reduction collection?
--	 app (function$ ctxt `dot` "reducer" `arrow` "put") [result]
	 app (function$ ctxt `dot` "R1" `dot` "put") [0, result]

	 putS "return CnC::CNC_Success;"
  return ()

cnc_reduceOp flops = do
  putS ""
  funDef rTy "reduce_op" [rTy,rTy] $ \ (x,y) -> do
    x <- simpleFlops2 rTy flops x y
    ret x

-- Emit the CnC context definition.
-- Ideally we would use the translator itself to generate this!
--
-- However, right now we want to be able to use individual reducers,
-- which is not efficent currently.
cnc_produceReduceContext :: EasyEmit() 
cnc_produceReduceContext = 
  undefined


-- This is one of those incomplete specs that doesn't include the
-- produce/consume edges.
cnc_spec = 
  readCnCFromStr (-1) "benchsynth_reduce.cnc"
  " tags<int>       T1;                          \n\
  \ steps           S1;                          \n\
  \ reductions<int, double> R1(reduce_op, 0 );   \n\
  \ T1 :: S1;                                    \n\
  \ env -> T1;                                   \n\
  \ "







--------------------------------------------------------------------------------
-- Cilk/Nabbit? 


--------------------------------------------------------------------------------
