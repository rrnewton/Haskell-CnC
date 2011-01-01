{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
----------------------------------------------------------------------------------------------------
-- Data types and utilities for working with CnC Specifications (Graphs)
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------
module Intel.Cnc.Spec.CncGraph ( 
				CncSpec (..), CncGraph, 
				CncGraphNode (..), ColName, graphNodeName, isStepC, isReductionC, isTagC, isItemC,
				
                                upstreamNbrs, downstreamNbrs, builtinSteps,
				getStepPrescriber
				
			       ) where

import Intel.Cnc.Spec.Util 
import Intel.Cnc.Spec.SrcLoc
import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.TagFun
import Data.List as L
import Data.Maybe
import Data.Map
import StringTable.Atom 
import StringTable.AtomMap as AM
import StringTable.AtomSet as AS
import Text.PrettyPrint.HughesPJClass hiding (Style)
import Data.Graph.Inductive as G

-- The total "Spec" includes the graph and other metadata.
-- Collect a global set of all collection names.
data CncSpec = CncSpec {
  steps :: AtomSet,
  tags  :: AtomMap (Maybe Type),
  items :: AtomMap (Maybe (Type,Type)),
  reductions :: AtomMap (Atom, Exp (), Maybe (Type,Type)), -- Contains 'op' and type.

  graph :: CncGraph,
  appname :: String,
  -- Might as well cache this after it is extracted, used by FGL calls:
  nodemap :: NodeMap CncGraphNode,
  -- Annoyingly, FGL nodemaps are essentially unreadable and useless, hence this:
  realmap :: Map CncGraphNode Node,
  -- We store the graph in a "flat" form and separately keep a tree of partitions:
  harchtree :: ()
}
-- deriving (Eq, Ord)

type CncGraph = (Gr CncGraphNode (Maybe TagFun))

type ColName = Atom

builtinSteps = [toAtom special_environment_name]

data CncGraphNode  = 
    CGSteps      ColName 
  | CGTags       ColName 
  | CGItems      ColName 
  | CGReductions ColName 
 deriving (Eq, Ord, Show)

graphNodeName (CGSteps n) = fromAtom n
graphNodeName (CGTags  n) = fromAtom n
graphNodeName (CGItems n) = fromAtom n
graphNodeName (CGReductions n) = fromAtom n

instance Pretty CncGraphNode where
  pPrint = text . show 

instance Show CncSpec where
  show = show . pPrint

instance Pretty CncSpec where
  pPrint (CncSpec{..}) = 
      text "============= All Steps ======================"    $$ 
	   hcat (intersperse (text ", ") $ L.map (text . fromAtom) $ AS.toList steps) $$
      text "\n============= Tag Types ======================" $$ 
	   sep (L.map (\(x,y) -> pp((fromAtom x)::String,y)) $ AM.toList tags) $$
      text "\n============= Item Types =====================" $$ 
	   sep (L.map (\(x,y) -> pp((fromAtom x)::String,y)) $ AM.toList items) $$

      text "\n=========== Reduction Ops/Types ==============" $$ 
	   sep (L.map (\(x,y) -> pp((fromAtom x)::String,y)) $ AM.toList reductions) $$

      text (show graph)


----------------------------------------------------------------------------------------------------

-- TODO: Perform basic checks here.
--verifySpec :: CncSpec -> CncSpec
verifySpec spec = 
   spec
   -- All steps are prescribed.
   -- All tags/items have types (for now, for C++)
   -- 

-- Get the name of the tag collection that prescribes a given step.
getStepPrescriber :: CncSpec -> ColName -> ColName
getStepPrescriber (CncSpec{..}) atom = 
  case L.filter isTagC labs of 
    [CGTags t] -> t
    ls -> error$ "getStepPrescriber step "++ (fromAtom atom) ++ 
	         " should have exactly one prescribing tag collection, not "++ show (length ls)
 where 
    (nd,_) = mkNode_ nodemap (CGSteps atom)
    preds = pre graph nd
    labs  = catMaybes$ L.map (lab graph) preds

isTagC (CGTags _) = True
isTagC _          = False
isItemC (CGItems _) = True
isItemC _           = False
isStepC (CGSteps _) = True
isStepC _           = False
isReductionC (CGReductions _) = True
isReductionC _                = False


----------------------------------------------------------------------------------------------------

-- | Get upstream neighbors in the CnC graph.
-- This routine sees *through* tag collections.
-- Returns a list of STEP and ITEM collections.
upstreamNbrs :: CncSpec -> CncGraphNode -> [CncGraphNode]
upstreamNbrs = nbrHelper pre

-- | Get downstream neighbors in the CnC graph.
-- This routine sees *through* tag collections.
-- Returns a list of STEP and ITEM collections.
downstreamNbrs :: CncSpec -> CncGraphNode -> [CncGraphNode]
downstreamNbrs = nbrHelper suc

nbrHelper adjacent (spec@CncSpec{..}) nodelab = 
    if gelem nd graph 
    then L.concat$ L.map process labs
    else error$ "upstream/downstream: cannot find neighbors because node is not in graph: "++ show nodelab
 where 
    (nd,_) = mkNode_ nodemap nodelab
    labs  = catMaybes$ L.map (lab graph) (adjacent graph nd)
    process x@(CGSteps _) = [x]
    process x@(CGItems _) = [x]
    process x@(CGReductions _) = [x]
    process x@(CGTags _) = nbrHelper adjacent spec x


-- | Extract a subgraph of the full CnC graph that only contains step collections (including 'env').
stepOnlyGraph :: CncGraph -> Gr ColName ()
stepOnlyGraph = error "stepOnlyGraph: TODO: implement me"


----------------------------------------------------------------------------------------------------
