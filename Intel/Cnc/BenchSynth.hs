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

import Control.Monad
import Data.ByteString.Char8 hiding (putStrLn)
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

noopKern :: ScalarKernel
noopKern x = return x

makeBenchFile :: (Int,Int,Int) -- length, width flops
	      -> ((Syntax, Syntax, Syntax) -> EasyEmit a)
	      -> ((Syntax, Syntax, Syntax) -> a -> EasyEmit ())
	      -> EasyEmit ObjFun
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
rTy = TDouble
vTy = tyConstructor "concurrent_vector" rTy
tname_reduce  = "Reducer"
tname_produce = "Producer"


rangeTy = tyConstructor "blocked_range" TInt

-- Use a for loop to go over a blocked range:
rangeFor range = forLoopSimple (range `dot` "begin()", range `dot` "end()") 

tbb_parProduceStruct :: Syntax -> ScalarKernel2 -> EasyEmit ()
tbb_parProduceStruct tname kernel =  do     
    cppStruct tname "" $ do
      prev   <- var rTy "prev"
      outbuf <- var (TPtr vTy) "outbuf"

      constFunDef voidTy "operator()" [TConst$ TRef rangeTy] $ \ range -> do
	rangeFor range $ \ i -> do
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
genFile = 
   Prelude.writeFile "test.cpp" $
   render$
   execEasyEmit$
    makeBenchFile (10,10,10) -- Defaults if no command line options are provided.
	     (\ (length, width, flops) -> 
	        do tbb_parReduceStruct tname_reduce (simpleFlops2 rTy flops)
	           putS ""
		   redfn <-  tbb_parReduceFun tname_reduce 
	           putS ""
	           tbb_parProduceStruct tname_produce (simpleFlops2 rTy flops)
	           putS ""
		   prodfn <-  tbb_parProduceFun tname_produce
	           putS ""
	           return (prodfn,redfn)
	     )
             (\ (length, width, flops) (prodfn,redfn) -> 
	        tbb_produceReduceChain False (length, width, flops) prodfn redfn
	     )

-- Temporary entrypoint:
main = 
  do putStrLn$ "Generating test.cpp file"
     genFile
     putStrLn$ "Finished generating"

 
--------------------------------------------------------------------------------
-- CnC backend

-- UNFINISHED:

-- Here we would like to reuse the main spec compiler code to do some of the work for us.

cnc_parProduceFun :: Syntax -> EasyEmit ObjFun
cnc_parProduceFun tname_produce = 
  -- This expands
  funDef rTy "ParProduceStage" [TInt, TRef rTy, TRef vTy] $ \ (size, inp, outp) -> do
    app (function$ outp `dot` "grow_to_at_least") [size]
    producer <- tmpvar (litType tname_produce)
    fieldset producer "prev" inp
    fieldset producer "outbuf" (addressOf outp)
    app (function "parallel_for")
	[function "blocked_range<int>" [0, methcall outp "size" []],
	 producer]

cnc_parProduceStruct :: Syntax -> ScalarKernel2 -> EasyEmit ()
cnc_parProduceStruct tname kernel =  do     
    cppStruct tname "" $ do
      prev   <- var rTy "prev"
      outbuf <- var (TPtr vTy) "outbuf"

      constFunDef voidTy "operator()" [TConst$ TRef rangeTy] $ \ range -> do
	rangeFor range $ \ i -> do
	   result <- kernel i prev
	   arrset (dereference outbuf) i result

      return ()

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
ttt = readCnCFromStr (-1) "foo.cnc"
  " tags<int>       T1;   \n\
  \ steps           S1;   \n\
  \ "


{-
struct my_context : public CnC::context< my_context >
{
    CnC::tag_collection< int, CnC::Internal::strided_range< int > > m_tags;
    CnC::step_collection<FindPrimes> m_findPrimesStepC;
    CnC::eager_reducer<int>* reducer;
    CnC::item_collection<int,int> m_items; // THIS IS UGLY.
   
    my_context() 
        : CnC::context< my_context >(),
          m_findPrimesStepC( this, "FindPrimes" ),
          m_tags( this )
        , m_items(this)
    {

        reducer = new CnC::eager_reducer<int>(0, &sum_int,  &m_items);
        prescribe( m_tags, m_findPrimesStepC );
        produce ( this->m_env, m_tags );
    }
};

--------------------------------------------------------------------------------

tags<int>       T1;
steps           S1;
reductions<int,int> R1(plus, 0);
env -> T1 :: S1 -> R1 -> env;



int plus(int x, int y) { return x + y; }

  #include<reduction_test.h>

template < class ctxt > 
int S1::execute( const int & tag, ctxt & c) const 
{
    printf("Step(%d) begun\n", tag);
    c.R1.put(0, tag);
    printf(" Step(%d) reducer put finished\n", tag);
    return CnC::CNC_Success;    
}

int main () 
{
    reduction_test_context context;
    for(int i=0; i<10; i++) context.T1.put(i);
    context.wait();
    int n;
    //    context.R1.done(0);
    // context.R1.all_done();
    context.R1.get(0, n);
    printf("Retrieve reducer result at 0: %d\n", n);
    return 0;
}


-}






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
