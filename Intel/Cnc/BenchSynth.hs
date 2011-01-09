{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- UNFINISHED!!!  (Barely started... this is a placeholder.)
----------------------------------------------------------------------------------------------------


-- module Intel.Cnc.BenchSynth where
module Main where

{- 
   The goal of this utility is to generate graph benchmarks with a certain set of features.

   This encompasses both kernel generation and graph generation.
 -}

import Control.Monad
import Data.ByteString.Char8
import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.Util hiding (app)
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




----------------------------------------------------------------------------------------------------

-- A numeric computation on a single scalar.
-- Takes the name of a variable.  Returns the name of a variable that contains the result.
-- data ScalarKernel = ScalarKernel (Syntax -> EasyEmit Syntax)
type ScalarKernel  = (Syntax -> EasyEmit Syntax)
type ScalarKernel2 = (Syntax -> Syntax -> EasyEmit Syntax)

-- Perform approximately n flops:
simpleFlops :: Type -> Syntax -> ScalarKernel
simpleFlops ty n = 
  -- ScalarKernel $ 
  \ inp -> do 
    tmp <- tmpvarinit ty inp
--    forLoopSimple (0, fromInt (n `quot` 2)) $ \ ind -> do
    forLoopSimple (0, n/2) $ \ ind -> do
      set tmp ("2.0000001" * tmp)
      set tmp (tmp - inp)
    return tmp

-- A version with two arguments:
simpleFlops2 :: Type -> Syntax -> ScalarKernel2
simpleFlops2 ty n = 
  \ x y -> 
   do x1 <- simpleFlops ty (n/2) x
      y1 <- simpleFlops ty (n/2) y
      return (x1 + y1)

noopKern :: ScalarKernel
noopKern x = return x

--------------------------------------------------------------------------------
-- TBB backend, generate code to construct a TBB graph and TBB execute methods.


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
rTy = TDouble
vTy = tyConstructor "concurrent_vector" rTy
tname_reduce  = "Reducer"
tname_produce = "Producer"


rangeTy = tyConstructor "blocked_range" TInt

-- Use a for loop to go over a blocked range:
rangeFor range = forLoopSimple (range `dot` "begin()", range `dot` "end()") 

parProduceStruct :: Syntax -> ScalarKernel2 -> EasyEmit ()
parProduceStruct tname kernel =  do     
    cppStruct tname "" $ do
      prev   <- var rTy "prev"
      outbuf <- var (TPtr vTy) "outbuf"

      constFunDef voidTy "operator()" [TConst$ TRef rangeTy] $ \ range -> do
	rangeFor range $ \ i -> do
	   result <- kernel i prev
	   arrset (dereference outbuf) i result

      return ()

parProduceFun :: Syntax -> EasyEmit ObjFun
parProduceFun tname_produce = 
  -- This expands
  funDef rTy "ParProduceStage" [TInt, TRef rTy, TRef vTy] $ \ (size, inp, outp) -> do
    app (function$ outp `dot` "grow_to_at_least") [size]
    producer <- tmpvar (litType tname_produce)
    fieldset producer "prev" inp
    fieldset producer "outbuf" (addressOf outp)
    app (function "parallel_for")
	[function "blocked_range<int>" [0, methcall outp "size" []],
	 producer]

----------------------------------------

-- Create the struct encapsulating the reduction operator:
parReduceStruct :: Syntax -> ScalarKernel2 -> EasyEmit ()
parReduceStruct tname kernel = 
 do 
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

-- Create the function that calls parallel_reduce
--parReduceFun :: Syntax -> EasyEmit ((Syntax,Syntax) -> EasyEmit Syntax)
parReduceFun :: Syntax -> EasyEmit ObjFun
parReduceFun tname = 
    funDef voidTy "ParReduceStage" [TRef vTy, TRef rTy] $ \ (inp, outp) -> 
--    funDef rTy "ParReduceStage" [TRef vTy, TRef vTy] $ \ (inp, outp) -> 
     do 
        total <- tmpvar (litType tname)
        set (total `dot` "input_vals")  (addressOf inp)
--        set (total `dot` "output_vals") (addressOf outp)
	let size = inp `dot` "size()"
	app (function "parallel_reduce")
	    [function (Syn$ pPrint rangeTy) [0,size] , total]
--	app (function$ outp `dot` "grow_to_at_least") [size]

	final <- tmpvarinit rTy$  total `dot` "value" / size
        -- comm "Then in serial populate the next stage:"
	-- forLoopSimple (0, size) $ \ i -> do 
	--    set (outp `arrsub` i) (final + i)
--	ret final
        set outp final

--        comm "Then invoke a parallel produce stage:"




--tbb_header :: ByteString
tbb_header :: String
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
produceReduceChain dofree (length, width, flops) 
		   prodFun redFun  =
   do 
      app (function "printf") [stringconst "Running pipe of length %d, width %d, op flops %d\n", 
			       length, width, flops]

      comm "Create a series of reduction steps, separated by buffers, as well"
      comm "as 'spreader' production steps that fan the reduced values back out."

      buf0 <- var (TPtr vTy) "buf0"
      set "buf0" (new vTy [])

      comm "We start off the computation with an input domain here:"
      forLoopSimple (0, width) $ \ i -> do 
        app (function$ (dereference "buf0") `dot` "push_back") [i];

      forLoopSimple (0, length) $ \i -> do
	buf1 <- var (TPtr vTy) "buf1"
	set buf1 (new vTy [])

        intmdt <- tmpvar rTy
	app redFun  [dereference buf0, intmdt]
	app prodFun [width, -- methcall buf0 "size"[], 
		     intmdt, dereference buf1]

	when dofree $ app (function "delete") [buf0]
	set buf0 buf1

      sum <- tmpvarinit rTy 0
      forLoopSimple (0, width) $ \i -> 
	set sum (sum + "(*buf0)" `arrsub` i)

      putS$ "printf(\"Final spot check sum: %lf\\n\", "++ synToStr sum++");"
      when dofree $ app (function "delete") [buf0]
      ret 0


-- Put together a complete, executable benchmark file:
makeBenchFile (_numstages, _width, _flops) restfile mainbody  =
 do 
    putS tbb_header
    comm "Create global variables for benchmark dimensions/parameters:"
    threads <- var TInt "threads"
    length  <- var TInt "length"
    width   <- var TInt "width"
    flops   <- var TInt "flops"
    pkg <- restfile (length, width, flops)

    funDef TInt "main" [TInt, TPtr$ TPtr TChar] $ \ (argc :: Syntax, argv :: Syntax) -> do
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

      tmpclassvar (TSym "task_scheduler_init") [threads]
      app (function "printf") [stringconst "Initialized num threads to %d\n", threads]

      mainbody (length, width, flops) pkg


-- Temp: run everything and generate one benchmark config to a file:
genFile = 
   Prelude.writeFile "test.cpp" $
   render$
   execEasyEmit$
    makeBenchFile (10,10,10) -- Defaults if no command line options are provided.
	     (\ (length, width, flops) -> 
	        do parReduceStruct tname_reduce (simpleFlops2 rTy flops)
	           putS ""
		   redfn <-  parReduceFun tname_reduce 
	           putS ""
	           parProduceStruct tname_produce (simpleFlops2 rTy flops)
	           putS ""
		   prodfn <-  parProduceFun tname_produce
	           putS ""
	           return (prodfn,redfn)
	     )
             (\ (length, width, flops) (prodfn,redfn) -> 
	        produceReduceChain False (length, width, flops) prodfn redfn
	     )

main = genFile

 
--------------------------------------------------------------------------------
-- CnC backend



-- tags<int>       T1;
-- steps           S1;
-- reductions<int,int> R1(plus, 0);
-- env -> T1 :: S1 -> R1 -> env;



-- int plus(int x, int y) { return x + y; }

-- #include<reduction_test.h>

-- template < class ctxt > 
-- int S1::execute( const int & tag, ctxt & c) const 
-- {
--     printf("Step(%d) begun\n", tag);
--     c.R1.put(0, tag);
--     printf(" Step(%d) reducer put finished\n", tag);
--     return CnC::CNC_Success;    
-- }

-- int main () 
-- {
--     reduction_test_context context;
--     for(int i=0; i<10; i++) context.T1.put(i);
--     context.wait();
--     int n;
--     //    context.R1.done(0);
--     // context.R1.all_done();
--     context.R1.get(0, n);
--     printf("Retrieve reducer result at 0: %d\n", n);
--     return 0;
-- }



--------------------------------------------------------------------------------
-- Cilk/Nabbit? 


--------------------------------------------------------------------------------

id_kernel = Krn { arg= "x", 
		  body= \emit -> emit "A" "x" }

foo :: Doc
foo = "foo"

bar :: Atom
bar = "bar"



-- class IsString a where
--     fromString :: String -> a

--------------------------------------------------------------------------------
