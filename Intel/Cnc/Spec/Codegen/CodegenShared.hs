{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NamedFieldPuns #-}

module Intel.Cnc.Spec.Codegen.CodegenShared where


import StringTable.Atom
import qualified Data.Graph.Inductive as G
import Data.List
import Data.Maybe
import Control.Monad

import Intel.Cnc.Spec.AST 
import Intel.Cnc.Spec.CncGraph
import qualified Intel.Cnc.EasyEmit as E
--import Intel.Cnc.EasyEmit  hiding (app, not, (&&), (==), (||))
import Intel.Cnc.EasyEmit  hiding (not, (||))
import Intel.Cnc.Spec.Util hiding (app)
import Intel.Cnc.Spec.CncGraph (ColName) -- hiding (app, not, (&&), (==))
import Prelude hiding ((&&), (==), (<=))
import qualified Prelude as P
import Debug.Trace

data CodeGenConfig = CodeGenConfig 
      { cgverbosity :: Bool 
      , old_05_api  :: Bool
      , genstepdefs :: Bool
      , gentracing  :: Bool
      , gendepends  :: Bool
      , gendebug    :: Bool
      , wrapall     :: Bool -- wrap ALL collections no matter what
      , plugins     :: [CodeGenPlugin]
      }
  deriving Show

default_codegen_config = 
 CodeGenConfig 
     {  cgverbosity = False 
      , old_05_api  = False
      , genstepdefs = True
      , gentracing  = False
      , gendepends  = False
      , gendebug    = False
      , wrapall     = False
--      , plugins     = []
      , plugins     = [doneCountingPlugin, testplugin]
     }


----------------------------------------------------------------------------------------------------
-- Here's a convenient Plugin architecture that uses EasyEmit
----------------------------------------------------------------------------------------------------

-- This represents a collection of hooks that can be used by a codegen to inject
-- code before/after steps and puts/gets.  This is an attempt to make
-- some of our different extensions into modular plugins.

-- Each hook takes two groups of argument:
--  (1) Graph context: the CnCGraph and names of relevant collections
--                     PLUS two bits of syntax that refer to the private/main contexts.
--  (2) Arguments: bits of syntax that refer to the relevant values.
--
-- Graph context.  In the following the two collection names represent
-- the "from" and "to" collections.
type GrCtxt1 = (CncSpec, Syntax, Syntax, ColName)
type GrCtxt2 = (CncSpec, Syntax, Syntax, ColName, ColName) -- Includes from and to collection.

type Hook grphCtxt args = grphCtxt -> args -> EasyEmit ()

--type Hook1 = Hook GrCtxt1 Syntax
--type Hook2 = Hook GrCtxt1 (Syntax,Syntax)

data CodeGenPlugin = CodeGenPlugin 
    {
      -- This predicate determines which steps to which the hooks are applied.
      hooksPredicate :: ColName -> Bool,

      -- I vacillated on whether or not to put all of these hooks
      -- (functions) inside a Maybe.  That would make it more clear
      -- what a plugin DOESN'T implement, but I think it makes the
      -- plugins more clunky to use, and makes the belowe combineHooks
      -- function harder to write.  Besides, there is a sensible
      -- default hook -- it emits nothing.

      -- Declare & initialize (respectively) global state for a collection, stored in global context.
      addGlobalState :: ColName -> (EasyEmit (), EasyEmit ()),
      -- Declare & initialize (respectively) local state, stored for a given step collection in TLS.
      addLocalState :: ColName -> (EasyEmit (), EasyEmit ()),

      -- Two arguments: reference to tag, reference to item.
      beforeItemPut :: Hook GrCtxt2 (Syntax,Syntax),
      afterItemPut  :: Hook GrCtxt2 (Syntax,Syntax),
      beforeItemGet :: Hook GrCtxt2 (Syntax,Syntax),
      afterItemGet  :: Hook GrCtxt2 (Syntax,Syntax),

      beforeReducerPut :: Hook GrCtxt2 (Syntax,Syntax),
      afterReducerPut  :: Hook GrCtxt2 (Syntax,Syntax),
      beforeReducerGet :: Hook GrCtxt2 (Syntax,Syntax),
      afterReducerGet  :: Hook GrCtxt2 (Syntax,Syntax),

      -- In contrast to the above, here we only need one piece of syntax (the tag):
      beforeTagPut :: Hook GrCtxt2 Syntax,
      afterTagPut  :: Hook GrCtxt2 Syntax,

      -- Step hooks take three syntax arguments:
      --   (1) name of the step's tag as input
      --   (2) name of a variable holding a pointer to the private
      --       context -- a struct that has the state added by
      --       "addLocalState" before.
      --   (3) name of a variable holding a reference to the main
      --       context -- containing the state added by "addGlobalState"
      beforeStepExecute :: Hook GrCtxt1 (Syntax,Syntax,Syntax),
      afterStepExecute  :: Hook GrCtxt1 (Syntax,Syntax,Syntax)
    }
  deriving Show

--instance Show CodeGenPlugin where
instance Show (a -> b) where
  show _ = "<fun>"


defaultCodeGenPlugin = 
  -- The default hooks just do nothing:
  let twoarg   =        const$ const$ return ()
  in CodeGenPlugin
  {
      hooksPredicate    = const False, -- And they apply to nothing.
      addGlobalState    = const$ (return (), return ()),
      addLocalState     = const$ (return (), return ()),

      beforeItemPut     = twoarg,
      afterItemPut      = twoarg,
      beforeItemGet     = twoarg,
      afterItemGet      = twoarg,

      beforeReducerPut  = twoarg,
      afterReducerPut   = twoarg,
      beforeReducerGet  = twoarg,
      afterReducerGet   = twoarg,

      beforeTagPut      = twoarg,
      afterTagPut       = twoarg,
      beforeStepExecute = twoarg,
      afterStepExecute  = twoarg
  }

-- Combine plugins.  Both are executed against all step collections to which they apply.
-- This is probably quite inefficient.  Those Maybe's could make it a little more efficient.
combineHooks left right = 
  let lp = hooksPredicate left 
      rp = hooksPredicate right 
      combine2 project x y   = do project left x y  ; project right x y 

      combine1 project x     = 
	let (a,b) = project left x
	    (c,d) = project right x
	in (do a;c, do b;d)

  in CodeGenPlugin
  {
      hooksPredicate    = \ name -> lp name || rp name,
      addGlobalState    = combine1 addGlobalState,
      addLocalState     = combine1 addLocalState,

      beforeItemPut     = combine2 beforeItemPut,
      afterItemPut      = combine2 afterItemPut,
      beforeItemGet     = combine2 beforeItemGet,
      afterItemGet      = combine2 afterItemGet,

      beforeReducerPut  = combine2 beforeReducerPut,
      afterReducerPut   = combine2 afterReducerPut,
      beforeReducerGet  = combine2 beforeReducerGet,
      afterReducerGet   = combine2 afterReducerGet,

      beforeTagPut      = combine2 beforeTagPut,
      afterTagPut       = combine2 afterTagPut,
      beforeStepExecute = combine2 beforeStepExecute,
      afterStepExecute  = combine2 afterStepExecute
  }



----------------------------------------------------------------------------------------------------
-- These should probably be moved to their own file, but I'm going to put small plugins here.
----------------------------------------------------------------------------------------------------

testplugin =
  let 
    boilerplate msg = 
	\ (_,_,_,from,to) (tag,val) -> 
	    putS$ "// [testhooks] "++ show from ++ ": " ++ msg ++" "++ show to 
		  ++", args: " ++ show (deSyn tag) ++" "++ show (deSyn val)

  in
  defaultCodeGenPlugin 
  { hooksPredicate = const True
  , addLocalState  = \ name -> (comm "[testhooks] Local state declaration goes here.",
				comm "[testhooks] Local state initialization goes here.")
  , addGlobalState = \ name -> (comm "[testhooks] Global state declaration goes here.",
				comm "[testhooks] Global state initialization goes here.")

  , beforeItemPut = boilerplate "before putting Item to"
  , afterItemPut  = boilerplate "after putting Item to"
  , beforeItemGet = boilerplate "before getting Item from"
  , afterItemGet  = boilerplate "after getting Item from"

  , beforeReducerPut = boilerplate "before putting Reducer to"
  , afterReducerPut  = boilerplate "after putting Reducer to"
  , beforeReducerGet = boilerplate "before getting Reducer from"
  , afterReducerGet  = boilerplate "after getting Reducer from"

  , beforeTagPut = \ (g,_,_,stpC,tgC) tag -> putS$ "// [testhooks] before putting Tag: " ++ show (deSyn tag)
  , afterTagPut  = \ (g,_,_,stpC,tgC) tag -> putS$ "// [testhooks] after putting Tag: " ++ show (deSyn tag) 

  , beforeStepExecute = \ _ (tag,priv,main) -> 
      putS$ "// [testhooks] before step execute on tag reference: " ++ show (deSyn tag) ++" "++ show (deSyn priv) ++" "++ show (deSyn main)
  , afterStepExecute  = \ _ (tag,priv,main) -> 
      putS$ "// [testhooks] after step execute on tag reference: " ++ show (deSyn tag) ++" "++ show (deSyn priv) ++" "++ show (deSyn main)

  }
  
doneCountingPlugin =
  let countername name = (atomToSyn name) +++ "_done_counter" 
      flagname    name = (atomToSyn name) +++ "_done_flag" 
  in
  defaultCodeGenPlugin 
  { hooksPredicate = const True
  , addGlobalState = \ name -> 
    (do comm "[autodone] Maintain two pieces of state per step collection: done counter & flag:"
        var (TSym "tbb::atomic<int>") (countername name)
        var (TSym "tbb::atomic<int>") (flagname name)
	return ()
    -- TEMP: FIXME: Need to set env done to 1 on wait() call.  
    -- No support for that yet so here's a hack:
    , set (flagname name) (if name P.== "env" then 1 else 0) 
    )

  , afterStepExecute = \ (spec, _,_, stpC) (tag,priv,main) -> 
      do comm "[autodone] Decrement the counter that tracks these instances:"
	 x <- tmpvar TInt 
         set x (function (main `dot` (countername stpC) `dot` "fetch_and_decrement") [])
	 app (function "printf") [stringconst$ " [autodone] "++show stpC++" Decremented refcount from %d\n", x]
	 if_ (x <= 1) -- TEMP FIXME!!!!! CHANGE ME BACK TO (==)
	     (do comm " If we transition to a zero count we check our upstreams to see if we are really done."
	         let upstream = filter isStepC $ upstreamNbrs spec (CGSteps stpC)
	         if_ (if null upstream
		      then error$ "doneCountingPlugin: missing upstream steps/environment for step collection "++ show stpC
		      else foldl1 (&&) $
		           map (\ (CGSteps stp) -> main `dot` (flagname stp)) upstream)
	             (do comm "Upstream are all done, and now so are we:"
		         set (main `dot` (flagname stpC)) 1
		         app (function$ "printf") [stringconst$ " [autodone] "++show stpC++" Yep, we're done, deps met..."
						   ++show (upstreamNbrs spec (CGSteps stpC))++"\n"])
	             (app (function "printf") [stringconst$ " [autodone] "++show stpC++" NOT DONE\n"]))
	     (return ())
  , beforeTagPut = \ (spec, priv,main, stpC,tgC) tag -> 
      do comm "[autodone] Increment the counter that tracks downstream step instances:"
	 let downstream = map graphNodeName $ filter isStepC $ downstreamNbrs spec (CGTags tgC)
	 forM_ downstream $ \ destC -> do
	    app (function "printf") [stringconst$ " [autodone] "++show stpC++" Incrementing refcount from %d\n", 
				     "(int)" +++ main `dot` (countername destC)]
            app (function (main `dot` (countername destC) `dot` "fetch_and_increment")) []

  }
