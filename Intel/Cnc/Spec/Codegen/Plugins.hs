{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NamedFieldPuns #-}

module Intel.Cnc.Spec.Codegen.Plugins
  (
    CodeGenPlugin, HooksTable(..), MainCtxtRef(..), PrivCtxtPtr(..), DonePlugin(..),
    defaultHooksTable,  testplugin,
    composeDonePlugins, convertDonePlugin,

    autodonePlugin
  )
where

-- import Intel.Cnc.Spec.Codegen.Plugins.ReductionDone

import Intel.Cnc.Spec.GraphAnalysis
import Intel.Cnc.Spec.AST 
import Intel.Cnc.Spec.CncGraph
import qualified Intel.Cnc.EasyEmit as E
import Intel.Cnc.EasyEmit  hiding (not, (||))
import Intel.Cnc.Spec.Util hiding (app)


import StringTable.Atom
import qualified StringTable.AtomMap as AM
import Data.List as L
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad

import Prelude hiding ((&&), (==), (<=))
import qualified Prelude as P


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

-- DESIGN NOTE: We could have created a type class for plugins and
-- used existential types to pass around "objects" with their method
-- tables.  But the below record-based approach is more explicit and
-- doesn't complicate the types; so sticking with it for now.

-- Each hook takes two groups of argument:
--  (1) Graph context: names of relevant collections (other than the host step)
--                     PLUS two bits of syntax that refer to the private/main contexts.
--  (2) Arguments: bits of syntax that refer to the relevant values.
type Hook grphCtxt args = grphCtxt -> args -> EasyEmit ()

-- It's very hard to keep all these pieces of "Syntax" straight.  To
-- help a little we wrap them in new types.
--   UNFINISHED!!! Haven't applied these new types yet!
newtype PrivCtxtPtr = PrivCtxtPtr Syntax
newtype MainCtxtRef = MainCtxtRef Syntax 

-- Graph context aliases: What information does a hook need about the graph?
-- First two Syntax arguments are the names of the private/main context respectively
type GrCtxt1 = (Syntax, Syntax)
type GrCtxt2 = (Syntax, Syntax, ColName) -- Includes destination collection name.

data HooksTable = HooksTable
    {
      -- I vacillated on whether or not to put all of these hooks
      -- (functions) inside a Maybe.  That would make it more clear
      -- what a plugin DOESN'T implement, but I think it makes the
      -- plugins more clunky to use, and makes the below combineHooks
      -- function harder to write.  Besides, there is a sensible
      -- default hook -- it emits nothing.
      
      addTopLevel :: EasyEmit (),

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
     afterStepExecute  :: Hook GrCtxt1 (Syntax,Syntax,Syntax),

      -- These inject code around the context's wait() method:
      beforeEnvWait :: MainCtxtRef -> EasyEmit (),
      afterEnvWait  :: MainCtxtRef -> EasyEmit (),

      -- Done happens per-set-of-collections (cycles cause grouping)
      -- This hook is used ONLY when the done plugins are enabled.
      whenDone :: S.Set CncGraphNode -> EasyEmit ()
--      whenDone :: S.Set ColName -> EasyEmit ()
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
      addTopLevel       = return (), 
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
      afterStepExecute  = twoarg,
      beforeEnvWait = const$ return (),
      afterEnvWait  = const$ return (),

      whenDone = const$ return ()
  }


----------------------------------------------------------------------------------------------------
-- A plugin simply to test the infrastruture:
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

  , beforeEnvWait = \ (MainCtxtRef m) -> putS$ "// [testhooks] before environment wait, main ctxt ref "++ show (deSyn m)
  , afterEnvWait  = \ (MainCtxtRef m) -> putS "// [testhooks] after environment wait"

  , whenDone = \ cols -> putS "// [testhooks] done"

  -- TODO add a "done" hook and introduce a "done" method alongside the step execute method inside the step wrapper.

  }




-- =================================================================================================
--  DONE propogation plugins:
-- -------------------------------------------------------------------------------------------------

-- Done-plugins are a second kind of plugin that attaches an action
-- when collections in the graph are all finished.  Done plugins
-- essentially depend on a single, shared service of
-- in-flight-instance-tracking.  Therefore, while more than one
-- done-plugin may be used, but they must all be *combined* into a
-- single regular plugin (together with the shared service).

-- 'Done' plugins are composable with one another.
-- A done plugin is both a predicate and a codegenerator that contributes to the "whenDone" method.
-- The additional bool argument is a debug-mode flag.
newtype  DonePlugin = DonePlugin (Bool -> CncGraphNode -> Maybe (EasyEmit ()))


-- All the DonePlugins used in a run of the translator should be
-- composed together before conversion into a regular plugin.
composeDonePlugins :: DonePlugin -> DonePlugin -> DonePlugin 
convertDonePlugin  :: Bool -> DonePlugin -> CodeGenPlugin
-- | The autodone plugin introduces counters for step collections and
-- | tracks when they are completely finished ("done").
autodonePlugin      :: DonePlugin

instance Show DonePlugin where show _ = "DonePlugin"

-- Used in this file only:
countername :: Int -> Syntax
countername num = Syn$t$ "done_counter" ++ show num

--------------------------------------------------------------------------------
-- autodone: Most basic done-plugin.
--------------------------------------------------------------------------------

-- This is a basic plugin that tracks done-ness but doesn't actually DO anything.
-- See other files in the Plugins/ directory for more meaningful Done functionality.
autodonePlugin = DonePlugin $ 
	        \ debug node -> 
		    if isStepC node 
		    then Just$ return ()
		    else Nothing

--------------------------------------------------------------------------------
-- Composing and Converting done-plugins.
--------------------------------------------------------------------------------

composeDonePlugins (DonePlugin dp1) (DonePlugin dp2) = DonePlugin$ 
  \ debug cncnode -> 
    case catMaybes [dp1 debug cncnode, dp2 debug cncnode]  of 
      [] -> Nothing
      ls -> Just$ sequence_ ls


-- There's an important distinction here.  Not all nodes that
-- have done *signaled* must also decrement their downstream.
-- Passive nodes NEEDNT be marked explicitly as done for active
-- downstream nodes to be done.
isActiveCollection = isStepC -- For now only step collections are ACTIVE.

-- Don't know why this isn't in the standard lib.
set_any pred = S.fold (\ a b -> b || pred a) False 


-- | Conversion from done plugin to a normal plugin.  The first argument is a debug flag.
-- TODO: Currently debug mode is set for ALL done plugins.  We may want finer grained control.
convertDonePlugin debug_autodone (DonePlugin dpgfun)
		  (spec@CncSpec{graph, steps, items, reductions, nodemap}) 
		  stpC 
    = 
      Just this
  where 
      BasicCycleAnalysis {index_map, rev_index_map, upstream_map, downstream_map} = basicCycleAnalysis spec
 
      funname     num = Syn$t$ "decr_done_counter"  ++ show num

      -- The predicate for which nodes are tracked with done counters.
      -- ALL step collections must be tracked, and some subset of the
      -- "passive" collections may be tracked as well.
      -- (TODO: we could track only what is upstream of the collections of interest to dpgfun)
      hasDoneSignaled cncNode = isStepC cncNode || isJust (dpgfun debug_autodone cncNode)
      -- NOTE: this is in contrast with isActiveCollection above.

      -- This includes unique counters we need for the tracked subset of collections:
      numbered_nodesets :: [(Int, S.Set CncGraphNode)] = 
	  -- This is a bit odd... but if ANY node within the set is of
	  -- interest to us, we include that counter:
	  L.filter (\ (i,set) -> set_any hasDoneSignaled set) $
	  M.toList rev_index_map
      counter_lookup stp = case AM.lookup stp index_map of 
			     Just x -> x
			     Nothing -> error$ "autodonePlugin: Could not find counter corresponding to step: "++ show stp

      is_maincontext = (stpC P.== toAtom special_environment_name) 
      -- env_ind :: Int = fst $ G.mkNode_ nodemap $ CGSteps $ toAtom special_environment_name
      -- The index of the counter associated with the environment:
      env_ind :: Int = index_map AM.! (toAtom special_environment_name)
     
      -- Find up or downstream *graph nodes* (chunking cycles together).
      nbr_set filt updown_map set = 
         S.filter filt $ 
         S.unions $ 
	  map (\nd -> AM.findWithDefault S.empty (graphNodeName nd) updown_map) $
	      S.toList set
      -- Convert nodes to counters:
      nbr_set_to_counters set = 
            -- Compute a set consisting of all downstream *counters* (step groups), not steps:
	    S.toList $ 
	    S.map ((index_map AM.!) . graphNodeName) $
            set

      stpind = counter_lookup stpC

      -- Bind the "methodtable" to 'this' so it can be recursively referenced:
      this = defaultHooksTable
       {
	whenDone = \ thisset -> do
          let 
	      -- CAREFUL: we signal to ALL relevant downstream, but upstream we care only about Active.
              downcounters = nbr_set_to_counters$ 
	                     nbr_set hasDoneSignaled    downstream_map thisset
	      upstream     = nbr_set isActiveCollection upstream_map   thisset

	  when debug_autodone$ 
	    app (function$ "printf") [stringconst$ " [autodone] Node(s) done "
				      ++ show ((map graphNodeName $ S.toList thisset) :: [Atom])
					      ++", upstream deps met: " ++ 
				              show ((map graphNodeName $ 
						     filter isActiveCollection$ S.toList upstream)
						    :: [Atom]) ++"\n"]

          -- Here we do the real decrementing, but ONLY if we OURSELVES are an "active" collection:
	  if set_any isActiveCollection$ thisset
	   then do comm "[autodone] As we become done, we decrement our downstream counters."
		   forM_ downcounters $ \ cntr -> app (function$ funname cntr) []
	   else comm "[autodone] NOT decrementing downstream because this node(set) does not create control instances!"
	  -- Finally, inject code from all the DonePlugins that are active:
          sequence_ (catMaybes$ map (dpgfun debug_autodone) $ S.toList thisset)

       , addGlobalState = 
        let names_str ndset = concat (intersperse " "$ map graphNodeName (S.toList ndset)) in
	-- This only happens ONCE, not per step-collection, and it declares ALL the counters:
	if is_maincontext
	then (do comm "[autodone] Maintain a piece of state for each tracked subgraph: the done counter"
		 forM_ numbered_nodesets $ \ (ind, ndset) -> do                    
		   comm$ "Counter "++ show ind ++ ": Serves node(s): " ++ names_str ndset
			 
		   var (TSym "tbb::atomic<int>") (countername ind)
		   -- "We also introduce a procedure that transitions a group of nodes into a done state:"
		   funDef voidTy (funname ind) [] $ \() -> do 
		      x <- tmpvar TInt
                      let name = countername ind
		      set x (function (name `dot` "fetch_and_decrement") [])
		      when debug_autodone$ 
 			app (function "printf") [stringconst$ " [autodone] Decremented ("++
			                         names_str ndset ++ ") to %d\n", 
						 x-1]
		      if_ (x == 1)
			  (whenDone this ndset)
			  (when debug_autodone $
			    app (function "printf") [stringconst$ " [autodone] "++show stpC++" NOT DONE\n"])
	           comm ""

		 return ()

	     , do comm "[autodone] Initialize done counters based on number of upstream deps:"
		  when debug_autodone$ do 
		     app (function "printf") [stringconst$ " [autodone] Initializing done counters...\n"]
		  forM_ numbered_nodesets $ \ (ind, ndset) -> do
		     let nbrs        = nbr_set isActiveCollection upstream_map ndset
			 counters    = nbr_set_to_counters nbrs
			 numcounters = length counters
		     if ind P.== env_ind then 
		       alwaysAssertEq "Env should not have upstream" [] counters $ do
		       comm$ "  Counter "++ show ind ++ " represents the environment and is initialized to one."
                       set (countername ind) 1
                      else do
		       comm$ "  Counter "++ show ind ++ " (representing "++ names_str ndset
			       ++") initalized to "++ show numcounters 
			     ++" for upstream deps "++ names_str nbrs
			       -- show ((map graphNodeName $ S.toList nbrs)::[Atom])
  		       set (countername ind) (fromIntegral numcounters)
	     )
	else (return (), return ())

      , afterStepExecute = \ _ (tag,priv,main) -> 
	  do comm "[autodone] Decrement the counter that tracks these instances:"
             app (function $ main `dot` (funname stpind)) []

      , beforeTagPut = \ (priv,main, tgC) tag -> 
	  do comm "[autodone] Increment the counter that tracks downstream step instances:"
	     let downstream = map graphNodeName $ filter isActiveCollection $ 
			      downstreamNbrs spec (CGTags tgC)
	     forM_ downstream $ \ destC -> do
		let counter = main `dot` countername (counter_lookup destC)
		when debug_autodone$ 
		   app (function "printf") [stringconst$ " [autodone] "++show stpC++
					    ": Incrementing "++show destC++" refcount to %d\n", 
					    "1 + (int)" +++ counter]
		app (function (counter `dot` "fetch_and_increment")) []

      , beforeEnvWait = \ (MainCtxtRef main) ->
	 do comm "[autodone] We consider the environment 'done' at this point:"
	    when debug_autodone$
	       app (function "printf") [stringconst$ " [autodone] Environment waiting, considered done.\n"]
            app (function$ main `dot` (funname stpind)) []	    
      }




-- OTHER PLUGINS TO CONSIDER WRITING:

----------------------------------------------------------------------------------------------------
-- Dead-Item-Collection plugin:
----------------------------------------------------------------------------------------------------

-- This one will extend the autodonePlugin and add counters for
-- item collections.  When all the steps consuming from an item
-- collection are 'done' then the item collection can be freed.


----------------------------------------------------------------------------------------------------  
-- Fusion plugin:
----------------------------------------------------------------------------------------------------  

-- Idea: this is a kind of interesting way to do fusion.
-- We can fuse producer/consumer A and B by:
--   (1) Call B directly from A's put method.  Use "return" to avoid the REAL put.
--   (2) Use "return" in B's beforeStepExecute to disable it.

-- Actually, (2) is not necessary, beforeStepExecute should really
-- throw an exception, because B's execute should never be called in
-- this framework.



