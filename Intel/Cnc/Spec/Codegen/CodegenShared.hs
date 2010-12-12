{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NamedFieldPuns #-}

module Intel.Cnc.Spec.Codegen.CodegenShared where

import StringTable.Atom
import qualified StringTable.AtomSet as AS
import qualified StringTable.AtomMap as AM
import Data.List as L
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Control.Monad
import Intel.Cnc.Spec.AST 
import Intel.Cnc.Spec.CncGraph
import qualified Intel.Cnc.EasyEmit as E
import Intel.Cnc.EasyEmit  hiding (not, (||))
import Intel.Cnc.Spec.Util hiding (app)
import Prelude hiding ((&&), (==), (<=))
import qualified Prelude as P

import Test.HUnit
import qualified Data.Graph.Inductive as G
import Data.Graph.Analysis.Algorithms.Common

import Debug.Trace

----------------------------------------------------------------------------------------------------
-- This datatype stores the configuration information for backend acode generation.
----------------------------------------------------------------------------------------------------

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
--      , plugins     = [testplugin]
--      , plugins     = [autodonePlugin]
      , plugins     = []
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
      afterStepExecute  :: Hook GrCtxt1 (Syntax,Syntax,Syntax),

      -- These inject code around the context's wait() method:
      beforeEnvWait :: EasyEmit (),
      afterEnvWait  :: EasyEmit ()
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
      afterStepExecute  = twoarg,
      beforeEnvWait = return (),
      afterEnvWait  = return ()
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

  , beforeEnvWait = putS "// [testhooks] before environment wait"
  , afterEnvWait  = putS "// [testhooks] after environment wait"
  }


-- =================================================================================================
--  DONE propogation plugins:
-- -------------------------------------------------------------------------------------------------
-- Here we specify two different (well, one inherited from the other)
-- plugins that share the same graph analysis.

autodonePlugin      :: Bool -> CodeGenPlugin 
reductionDonePlugin :: Bool -> CodeGenPlugin 

-- | The autodone plugin introduces counters for step collections and
-- | tracks when they are completely finished ("done").
autodonePlugin      debug spec = fst $ __merged debug spec

-- | The reduction-done plugin extends autodone by introducing counters
-- for reduction collections and signalling all_done()
reductionDonePlugin debug spec = snd $ __merged debug spec 

-- NOTE: There must be a better method for this, but the following
-- will ensure that both these plugins share the same graph analysis.
__merged debug_autodone spec = 
   -- Bind the graph analysis a single time:
   let rec = __sharedCounterGraphAnalysis spec in 
   (__autodone  debug_autodone rec spec, 
    __reduction debug_autodone rec spec)

--------------------------------------------------------------------------------
-- | This graph analysis can be performed once and shared between
-- multiple plugins.  It collapses cycles and maps the collapsed space
-- onto unique numbers, which can be used for various purposes.
-- Further, it computes the upstream step collections on the collapsed graph.more 
__sharedCounterGraphAnalysis :: CncSpec
			   -> (AM.AtomMap Int,
			       M.Map Int (S.Set Atom),
			       AM.AtomMap (S.Set CncGraphNode))
__sharedCounterGraphAnalysis (spec@CncSpec{graph, steps, items, reductions, nodemap}) = 
    (counter_map, rev_counter_map, upstream_map)    
  where 
      nodeToName   = graphNodeName . fromJust . G.lab graph
      nameToNode   = fst . (G.mkNode_ nodemap)

      -- Remove item collections before computing cycles (interested in control only):
      pruned_graph = G.delNodes (map (nameToNode . CGItems) $ AM.keys items) $
		     G.delNodes (map (nameToNode . CGReductions) $ AM.keys reductions) $
		     graph 
      -- TODO: ALTERNATIVELY: Rebuild the graph with only step collections.
      --step_only_graph = stepOnlyGraph graph

      -- NOTE: This includes special_environment_name:
      all_step_nds = S.fromList$ map (nameToNode . CGSteps) (AS.toList steps)

      cycsets = joinCycles$ cyclesIn' pruned_graph
		      
      -- Remove all nodes that are in cycles to find those that remain:
      non_cycle_nodes = S.toList $ foldl' S.difference all_step_nds cycsets

      allnodesets :: [S.Set G.Node] = cycsets ++ (map S.singleton $ non_cycle_nodes)      

      -- Map every node onto exactly one counter.  Nodes in a cycle must have the same counter.
      counter_map :: AM.AtomMap Int  = fromSetList $ nodesets_counters
      nodesets_counters = zip (map (S.map nodeToName) allnodesets) [0..] 
      --num_counters = length allnodesets
      -- For convenience, we store a map in the other direction as well, from counter -> nodeset:
      rev_counter_map = M.fromList $ map (\ (a,b) -> (b,a)) nodesets_counters

      -- Next we compute the upstream dependencies of entire cycles taken together:
      setUpstream = S.fromList .  concat . map (upstreamNbrs spec) . S.toList 
      getSteps = S.fromList . filter isStepC . map (fromJust . G.lab graph) . S.toList 
      cycs_wname = L.map getSteps cycsets
      cyc_upstreams = L.map (\ set -> S.difference (setUpstream set) set) cycs_wname

      -- Here we build a map for upstreams filling in for anything not in a cycle:
      upstream_map :: AM.AtomMap (S.Set CncGraphNode)
	   = fromSetList $ 
	     (map dosingle non_cycle_nodes ++
	      zip (map (S.map nodeToName) cycsets) cyc_upstreams)
      dosingle nd = let name = nodeToName nd in 
		    (S.singleton name, 
		     S.fromList$ upstreamNbrs spec (CGSteps name))


--------------------------------------------------------------------------------
-- Reduction-Done plugin:
--------------------------------------------------------------------------------

__autodone debug_autodone (counter_map, rev_counter_map, upstream_map) 
           (spec@CncSpec{graph, steps, items, reductions, nodemap}) 
	   stpC =
  let 
      countername num = Syn$t$ "done_counter" ++ show num
      flagname    num = Syn$t$ "done_flag"    ++ show num
      numbered_nodesets :: [(Int, S.Set Atom)] = M.toList rev_counter_map
      counter_lookup stp = case AM.lookup stp counter_map of 
			     Just x -> x
			     Nothing -> error$ "autodonePlugin: Could not find counter corresponding to step: "++ show stp
   
      is_maincontext = (stpC P.== toAtom special_environment_name) 
  in
   Just$ defaultHooksTable
   { 
     addGlobalState = 
     -- This only happens ONCE, not per step-collection, and it adds ALL the counters/flag:
     if is_maincontext
     then (do comm "[autodone] Maintain two pieces of state for each tracked subgraph: done counter & flag:"
	      forM_ numbered_nodesets $ \ (ind, ndset) -> do 
		comm$ show ind ++ ": Serves node(s): " ++ 
		      concat (intersperse " "$ map fromAtom (S.toList ndset))
		var (TSym "tbb::atomic<int>") (countername ind)
		var (TSym "tbb::atomic<int>") (flagname ind)
	      return ()

	  , do comm "[autodone] Initialize done flags/counters:"
	       when debug_autodone$ do 
		  app (function "printf") [stringconst$ " [autodone] Initializing done flags/counters...\n"]
	       forM_ numbered_nodesets $ \ (ind, _) -> do
		  set (flagname ind) 0
		  set (countername ind) 0
	       -- when debug_autodone$ do 
	       --    let cycs = (map (map graphNodeName . S.toList) cycs_wname) :: [[Atom]]
	       --    app (function "printf") [stringconst$ "   [autodone] Cycles detected: "
	       -- 			       ++show cycs++"\n"]
	  )
     else (return (), return ())

   , afterStepExecute = \ _ (tag,priv,main) -> 
       do comm "[autodone] Decrement the counter that tracks these instances:"
	  x <- tmpvar TInt 
	  done_transition <- varinit TInt "done_transition" 0 -- HACK, this is to coordinate with reductionDonePlugin below
	  let stpind = counter_lookup stpC
	  set x (function (main `dot` (countername stpind) `dot` "fetch_and_decrement") [])
	  when debug_autodone$ 
	    app (function "printf") [stringconst$ " [autodone] "++show stpC++": Decremented own refcount to %d\n", x-1]
	  if_ (x == 1)
	      (do comm " If we transition to a zero count we check our upstreams to see if we are really done."
		  let upstream = filter isStepC $ S.toList $ AM.findWithDefault S.empty stpC upstream_map
		  if_ (if null upstream
		       then 1 -- error$ "autodone: missing upstream steps/environment for step collection "++ show stpC
		       else foldl1' (&&) $
			    map (\ (CGSteps stp) -> main `dot` (flagname$ counter_lookup stp)) upstream)
		      (do comm "Upstream are all done, and now so are we:"
			  set (main `dot` (flagname stpind)) 1
			  set done_transition 1
			  when debug_autodone$ 
			    app (function$ "printf") [stringconst$ " [autodone] "++ show stpC ++": Node(s) done "
						      ++ show (S.toList$ rev_counter_map M.! stpind)
						      ++", deps met: " ++ show upstream ++"\n"])
		      (when debug_autodone$
		       app (function "printf") [stringconst$ " [autodone] "++show stpC++" NOT DONE\n"]))
	      (return ())

   , beforeTagPut = \ (priv,main, tgC) tag -> 
       do comm "[autodone] Increment the counter that tracks downstream step instances:"
	  let downstream = map graphNodeName $ filter isStepC $ downstreamNbrs spec (CGTags tgC)
	  forM_ downstream $ \ destC -> do
	     let counter = main `dot` countername (counter_lookup destC)
	     when debug_autodone$ 
		app (function "printf") [stringconst$ " [autodone] "++show stpC++": Incrementing "++show destC++" refcount to %d\n", 
					 "1 + (int)" +++ counter]
	     app (function (counter `dot` "fetch_and_increment")) []

   , beforeEnvWait = 
      do comm "[autodone] We consider the environment 'done' at this point:"
	 let flag = flagname$ counter_lookup (toAtom special_environment_name)
	 set ("p" `dot` flag) 1
	 when debug_autodone$
	    app (function "printf") [stringconst$ " [autodone] Environment waiting, considered done.\n"]
   }


--------------------------------------------------------------------------------
-- Reduction-Done plugin:
--------------------------------------------------------------------------------

__reduction debug_autodone (counter_map, rev_counter_map, upstream_map)   
            (spec@CncSpec{graph, steps, items, reductions, nodemap}) =
  let 
      autodone_plug = autodonePlugin debug_autodone spec
      countername x = strToSyn$ "reduction_depcounter_" ++ show x

      upstream_lookup redC = 
	  case AM.lookup redC upstream_map of
	     Nothing -> error$ "reductionDonePlugin: internal error, could not find upstream for reduction collection " ++ show redC
	     Just x -> x      

      -- Count number of upstreams for each reduction collection.
      reduction_upstreams = AM.mapWithKey (\ redC (_,_,_) -> upstream_lookup redC) reductions
      reduction_upstream_counts = AM.map S.size reduction_upstreams
  in 
  \ stpC ->
  let is_maincontext = (stpC P.== toAtom special_environment_name) in
  case autodone_plug stpC of 
    Nothing -> Nothing
    Just autodone_hooks -> Just $ 
     autodone_hooks
     { 
       addGlobalState = 
        let (lft,rht) = addGlobalState autodone_hooks in
	-- This only happens ONCE, not per step-collection, and it adds ALL the counters/flag:
	if is_maincontext
	then ( do lft
		  comm "[reduction_done] Additionally maintain counters for reduction collections:"
	          forM_ (AM.toList reductions) $ \ (redC, _) ->
		     var (TSym "tbb::atomic<int>") (countername redC)
	     , do rht
		  comm "[reduction_done] Likewise initialize reduction_collection counters:"
	          forM_ (AM.toList reductions) $ \ (redC, (op, init, ty)) -> do
-- 	             set (countername redC) (strToSyn$show$ reduction_upstream_counts AM.! redC)
                     let upstrm = upstreamNbrs spec (CGReductions redC)
	             comm$ "   reduction collection "++ show redC ++" depends on "++ show upstrm ++ ":"
 	             set (countername redC) (strToSyn$show$ length upstrm)
--		  forM_ (zip [0..num_counters] allnodesets) $ \ (ind, ndset) -> do

	     )
	else (lft, rht)

     , afterStepExecute = \ cxt x@(tag,priv,main) -> 
	 do afterStepExecute autodone_hooks cxt x
	    if_ (constant "done_transition")
		(forM_ (filter isReductionC $ downstreamNbrs spec (CGSteps stpC)) $ \ (CGReductions redC) -> do
		   let counter = main `dot` countername redC
		   x <- tmpvar TInt 
		   set x ((function (counter `dot` "fetch_and_increment")) [])
		   if_ (x == 1)
		       (do comm "[reduction_done] When the dependencies of a reduction collection are done, signal all_done() on it"
			   when debug_autodone$
			     app (function "printf") [stringconst$ " [reduction_done] Signaling all_done() for "++ show redC ++".\n"]
		           app (function (main `dot` atomToSyn redC `dot` "all_done")) [])
		       (return ())
		   )		
		(return ())
     }


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


----------------------------------------------------------------------------------------------------
-- Helpers/Utilities:
----------------------------------------------------------------------------------------------------

--fromSetList :: P.Ord a => [(S.Set a, b)] -> M.Map a b
fromSetList ::[(S.Set Atom, b)] -> AM.AtomMap b
fromSetList = 
   foldl' (\ map (set,val) -> 
	   S.fold (\ nd mp -> AM.insert nd val mp)
	          map set)
          AM.empty

-- Graph related utilities used by the above:

-- Join together nodes that participate in overlapping cycles:
-- Inefficient quadratic algorithm:
joinCycles :: (P.Ord a) => [[a]] -> [S.Set a]
joinCycles cycs = foldl' foldin [] (map S.fromList cycs)
 where 
  foldin [] cyc      = [cyc]
  foldin (hd:tl) cyc = if S.null (S.intersection hd cyc)
		       then hd : foldin tl cyc 
		       else (S.union cyc hd) : tl

----------------------------------------------------------------------------------------------------  
-- Unit Tests:
----------------------------------------------------------------------------------------------------  

testg :: G.Gr () String
testg = G.mkGraph (zip [1..7] (repeat ())) 
    [(1,2,""), (2,3,""), (3,4,""), (4,5,""), (5,6,""), (6,7,""),
     -- Close some cycles.
     (4,2,""), (7,6,""), (7,3,"")
    ]
testc = cyclesIn' testg

tests_codegenshared = 
    testSet "CodegenShared" 
      [ testCase "" "joinCycles connected1"$  [S.fromList [2,3,4,5,6,7]] ~=? joinCycles testc
      , testCase "" "joinCycles connected2"$  [S.fromList [2,3,4,5,6,7]] ~=? joinCycles [[4,5,6,7,3],  [4,2,3],  [6,7]]
      , testCase "" "joinCycles connected3"$  [S.fromList [2,3,4,5,6,7]] ~=? joinCycles [[4,5,6,7,3],  [4,2,3]]
      , testCase "" "joinCycles split"$       [S.fromList [2,3,4], S.fromList [6,7]] ~=? joinCycles [[4,2,3],  [6,7]]
      ]
