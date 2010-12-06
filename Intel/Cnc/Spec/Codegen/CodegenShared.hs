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

-- A "plugin" represents a collection of hooks that can be used by a
-- codegen backend to inject code before/after steps, puts, and gets.
-- This is an attempt to convert some of our different extensions into
-- modular plugins.
--
-- The "constructor" for each plugin object takes a spec and then the name of the step collection.
-- It returns a methodtable full of hooks IF the plugin applies to the given step, and Nothing otherwise.
type CodeGenPlugin = CncSpec -> ColName -> Maybe HooksTable

-- Each hook takes two groups of argument:
--  (1) Graph context: names of relevant collections (other than the host step)
--                     PLUS two bits of syntax that refer to the private/main contexts.
--  (2) Arguments: bits of syntax that refer to the relevant values.
type Hook grphCtxt args = grphCtxt -> args -> EasyEmit ()

-- Graph context aliases::
type GrCtxt1 = (Syntax, Syntax)
type GrCtxt2 = (Syntax, Syntax, ColName) -- Includes destination collection name.

data HooksTable = HooksTable
    {
      -- I vacillated on whether or not to put all of these hooks
      -- (functions) inside a Maybe.  That would make it more clear
      -- what a plugin DOESN'T implement, but I think it makes the
      -- plugins more clunky to use, and makes the belowe combineHooks
      -- function harder to write.  Besides, there is a sensible
      -- default hook -- it emits nothing.

      -- Declare & initialize (respectively) global state for a collection, stored in global context.
      addGlobalState :: (EasyEmit (), EasyEmit ()),
      -- Declare & initialize (respectively) local state, stored for a given step collection in TLS.
      addLocalState  :: (EasyEmit (), EasyEmit ()),

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

instance Show (EasyEmit a) where
  show _ = "<easyemit_computation>"

--------------------------------------------------------------------------------
-- The default method table does nothing.

defaultHooksTable = 
  -- The default hooks just do nothing:
  let twoarg   =        const$ const$ return ()
  in HooksTable
  {
      addGlobalState    = (return (), return ()),
      addLocalState     = (return (), return ()),

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



----------------------------------------------------------------------------------------------------
-- These should probably be moved to their own file, but I'm going to put small plugins here.
----------------------------------------------------------------------------------------------------

testplugin spec stpC =
  let 
    boilerplate msg = 
	\ (_,_,to) (tag,val) -> 
	    putS$ "// [testhooks] "++ show stpC ++ ": " ++ msg ++" "++ show to 
		  ++", args: " ++ show (deSyn tag) ++" "++ show (deSyn val)

  in
  Just$ defaultHooksTable
  { 
    addLocalState  = (comm "[testhooks] Local state declaration goes here.",
		      comm "[testhooks] Local state initialization goes here.")
  , addGlobalState = (comm "[testhooks] Global state declaration goes here.",
		      comm "[testhooks] Global state initialization goes here.")

  , beforeItemPut = boilerplate "before putting Item to"
  , afterItemPut  = boilerplate "after putting Item to"
  , beforeItemGet = boilerplate "before getting Item from"
  , afterItemGet  = boilerplate "after getting Item from"

  , beforeReducerPut = boilerplate "before putting Reducer to"
  , afterReducerPut  = boilerplate "after putting Reducer to"
  , beforeReducerGet = boilerplate "before getting Reducer from"
  , afterReducerGet  = boilerplate "after getting Reducer from"

  , beforeTagPut = \ (_,_,tgC) tag -> putS$ "// [testhooks] before putting Tag: " ++ show (deSyn tag)
  , afterTagPut  = \ (_,_,tgC) tag -> putS$ "// [testhooks] after putting Tag: " ++ show (deSyn tag) 

  , beforeStepExecute = \ _ (tag,priv,main) -> 
      putS$ "// [testhooks] before step execute on tag reference: " ++ show (deSyn tag) ++" "++ show (deSyn priv) ++" "++ show (deSyn main)
  , afterStepExecute  = \ _ (tag,priv,main) -> 
      putS$ "// [testhooks] after step execute on tag reference: " ++ show (deSyn tag) ++" "++ show (deSyn priv) ++" "++ show (deSyn main)

  }

----------------------------------------------------------------------------------------------------  

doneCountingPlugin spec stpC =
  let countername name = (atomToSyn name) +++ "_done_counter" 
      flagname    name = (atomToSyn name) +++ "_done_flag" 
  in
  Just$ defaultHooksTable
  { 
    addGlobalState = 
    (do comm "[autodone] Maintain two pieces of state per step collection: done counter & flag:"
        var (TSym "tbb::atomic<int>") (countername stpC)
        var (TSym "tbb::atomic<int>") (flagname stpC)
	return ()
    -- TEMP: FIXME: Need to set env done to 1 on wait() call.  
    -- No support for that yet so here's a hack:
    , set (flagname stpC) (if stpC P.== "env" then 1 else 0) 
    )

  , afterStepExecute = \ _ (tag,priv,main) -> 
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

  , beforeTagPut = \ (priv,main, tgC) tag -> 
      do comm "[autodone] Increment the counter that tracks downstream step instances:"
	 let downstream = map graphNodeName $ filter isStepC $ downstreamNbrs spec (CGTags tgC)
	 forM_ downstream $ \ destC -> do
	    app (function "printf") [stringconst$ " [autodone] "++show stpC++" Incrementing refcount from %d\n", 
				     "(int)" +++ main `dot` (countername destC)]
            app (function (main `dot` (countername destC) `dot` "fetch_and_increment")) []

  }


----------------------------------------------------------------------------------------------------  
-- Fusion plugin:

-- Idea: this is a kind of interesting way to do fusion.
-- We can fuse producer/consumer A and B by:
--   (1) Call B directly from A's put method.  Use "return" to avoid the REAL put.
--   (2) Use "return" in B's beforeStepExecute to disable it.

-- Actually, (2) is not necessary, beforeStepExecute should really
-- throw an exception, because B's execute should never be called in
-- this framework.

