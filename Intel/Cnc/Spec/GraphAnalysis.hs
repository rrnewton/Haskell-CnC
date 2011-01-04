{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NamedFieldPuns #-}

module Intel.Cnc.Spec.GraphAnalysis 
     (BasicCycleAnalysis(..), 
      basicCycleAnalysis,
      tests_graphanalysis)
where

import StringTable.Atom
import qualified StringTable.AtomMap as AM
import Data.List as L
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import Intel.Cnc.Spec.CncGraph
import Intel.Cnc.Spec.Util hiding (app)
import Prelude hiding ((&&), (==), (<=))
import qualified Prelude as P

import Test.HUnit
import qualified Data.Graph.Inductive as G
import Data.Graph.Analysis.Algorithms.Common

import Intel.Cnc.Spec.GatherGraph (exampleGraph)
import Text.PrettyPrint.HughesPJClass


----------------------------------------------------------------------------------------------------
-- Type definitions and functions for graph analyses.
----------------------------------------------------------------------------------------------------

-- | This represents the result of a basic graph analysis to determine
-- cycles.  The nodes are re-indexed according to a scheme that
-- assigns the same number to nodes within a cycle (e.g. they are
-- "grouped").
data BasicCycleAnalysis = BasicCycleAnalysis 
  {
    -- Map node names onto their new "collapsed" indices.
    index_map      :: AM.AtomMap Int,
    -- The same in reverse:
    rev_index_map  :: M.Map Int (S.Set CncGraphNode),
    -- Map nodes onto their up/downstream taking into account the
    -- grouping (e.g. a dependency on a node implies dependencies on
    -- all other nodes sharing the same group.
    downstream_map :: AM.AtomMap (S.Set CncGraphNode),
    upstream_map   :: AM.AtomMap (S.Set CncGraphNode)
  }
 deriving Show

-- Would be nice to DERIVE this:
instance Pretty BasicCycleAnalysis where 
 pPrint BasicCycleAnalysis{index_map, rev_index_map, upstream_map, downstream_map} =
   text "BasicCycleAnalysis {" $$
   nest 4 (
	   text "index_map = "      <> pPrint index_map      <> text ", " $$
	   text "rev_index_map = "  <> pPrint rev_index_map  <> text ", " $$
	   text "upstream_map = "   <> pPrint upstream_map   <> text ", " $$
	   text "downstream_map = " <> pPrint downstream_map
	  ) $$
   text "}"


-- | Performs an analysis to produce a BasicCycleAnalysis
--   For efficiency we would expect that this analysis is performed
--   once and shared between multiple plugins.
--
-- NOTE: Currently only step collections are included in the counter map!
basicCycleAnalysis :: CncSpec -> BasicCycleAnalysis
basicCycleAnalysis (spec@CncSpec{graph, steps, items, reductions, nodemap, realmap}) = 
    BasicCycleAnalysis {index_map=counter_map, rev_index_map=rev_counter_map, upstream_map, downstream_map}
  where 
      indexToNamed nd = case G.lab graph nd of 
		        Nothing -> error$ "basicCycleAnalysis: Node not found in graph: "++ show nd
		        Just x -> x
      namedToIndex   = fst . (G.mkNode_ nodemap)

      -- NOTE: Only "active" collections which can put new instances into
      -- other parts of the graph (currently only step collections) are
      -- considered for the purposes of cycle calculation.

      -- Thus remove reduction/item collections before computing cycles (interested in control only):
      pruned_graph = G.delNodes (map (namedToIndex . CGItems) $ AM.keys items) $
		     G.delNodes (map (namedToIndex . CGReductions) $ AM.keys reductions) $
		     graph 
      -- TODO: ALTERNATIVELY: Rebuild the graph with only step collections.
      --step_only_graph = stepOnlyGraph graph

      -- NOTE: This includes special_environment_name:
--      all_step_nds = S.fromList$ map (namedToIndex . CGSteps) (AS.toList steps)

      cycsets = joinCycles$ cyclesIn' pruned_graph
		      
      -- Remove all nodes that are in cycles to find those that remain:
--      non_cycle_nodes = S.toList $ foldl' S.difference all_step_nds cycsets

      all_names :: S.Set G.Node
      all_names = S.fromList$ map namedToIndex $ M.keys realmap
      non_cycle_nodes = S.toList$ foldl' S.difference all_names cycsets

      -- Combine the nodes in and out of cycles:
      allnodesets :: [S.Set G.Node] = cycsets ++ (map S.singleton $ non_cycle_nodes)      

      -- Map every node onto exactly one counter.  Nodes in a cycle must have the same counter.
      counter_map :: AM.AtomMap Int  = fromSetList $ nodesets_counters
      nodesets_counters = zip (map (S.map (graphNodeName . indexToNamed)) allnodesets) [0..] 
      --num_counters = length allnodesets
      -- For convenience, we store a map in the other direction as well, from counter -> nodeset:
      rev_counter_map = M.fromList $ 
			zip [0..] (map (S.map (fromJust . G.lab graph)) allnodesets)

      -- Next we compute the upstream dependencies of entire cycles taken together:
      cycs_wname = L.map getSteps cycsets
      -- getSteps: convert a set of FGL Nodes to a set of CGSteps
      getSteps = S.fromList . filter isStepC . map (fromJust . G.lab graph) . S.toList 
      
      -- For each node we store its up/down-stream dependencies.  If the node is part of a
      -- cycles, its up/down-stream deps are those of the entire cycle:
      upstream_map :: AM.AtomMap (S.Set CncGraphNode)   = make_map upstreamNbrs   
      downstream_map :: AM.AtomMap (S.Set CncGraphNode) = make_map downstreamNbrs 
      
      make_map getnbrs  =
             let
                 cycnbrs = L.map (\ set -> S.difference (setNbrs set) set)   cycs_wname
                 -- setNbrs: take the combined upstreams of a set of nodes:
		 setNbrs = S.fromList .  concat . map (getnbrs spec) . S.toList 
	     in 
	        fromSetList $ 
		(map (dosingle getnbrs) non_cycle_nodes ++
	        zip (map (S.map (graphNodeName . indexToNamed)) cycsets) cycnbrs)

      dosingle getnbrs nd = 
	     let name = indexToNamed nd in 
	       (S.singleton (graphNodeName name), 
		S.fromList$ getnbrs spec name)


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


-- Join together nodes that participate in overlapping cycles:
-- FIXME!!! Inefficient quadratic algorithm:
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

tests_graphanalysis = 
    testSet "CodegenShared" 
      [ testCase "" "joinCycles connected1"$  [S.fromList [2,3,4,5,6,7]] ~=? joinCycles testc
      , testCase "" "joinCycles connected2"$  [S.fromList [2,3,4,5,6,7]] ~=? joinCycles [[4,5,6,7,3],  [4,2,3],  [6,7]]
      , testCase "" "joinCycles connected3"$  [S.fromList [2,3,4,5,6,7]] ~=? joinCycles [[4,5,6,7,3],  [4,2,3]]
      , testCase "" "joinCycles split"$       [S.fromList [2,3,4], S.fromList [6,7]] ~=? joinCycles [[4,2,3],  [6,7]]
       
      , testCase "" "basic graph analysis"$ test$ do
	 putStrLn "Printing result of basic cycle analysis:"
	 print$ pPrint (basicCycleAnalysis exampleGraph)
      
      ]

