{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
----------------------------------------------------------------------------------------------------
-- Data types and utilities for working with CnC Specifications (Graphs)
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------
module Intel.Cnc.Spec.CncGraph where

import Intel.Cnc.Spec.AST
import Data.List as L
import Data.Maybe
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
  graph :: CncGraph,
  appname :: String,
  -- Might as well cache this after it is extracted:
  nodemap :: NodeMap CncGraphNode
}

type CncGraph = (Gr CncGraphNode (Maybe TagFun))

type ColName = Atom

builtinSteps = [toAtom "env"]

data CncGraphNode  = 
    CGSteps ColName 
  | CGTags  ColName 
  | CGItems ColName 
 deriving (Eq, Ord, Show)

graphNodeName (CGSteps n) = fromAtom n
graphNodeName (CGTags  n) = fromAtom n
graphNodeName (CGItems n) = fromAtom n

instance Show CncSpec where
  show = show . pPrint

instance Pretty CncSpec where
  pPrint (CncSpec{..}) = 
      text "  All Steps:\n========================================"    $$ 
	   hcat (intersperse (text ", ") $ L.map (text . fromAtom) $ AS.toList steps) $$
      text "\n  Tag Types:\n========================================" $$ 
	   sep (L.map (\(x,y) -> pp((fromAtom x)::String,y)) $ AM.toList tags) $$
      text "\n  Item Types:\n========================================" $$ 
	   sep (L.map (\(x,y) -> pp((fromAtom x)::String,y)) $ AM.toList items) $$
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
  case L.filter isTags labs of 
    [CGTags t] -> t
    ls -> error$ "getStepPrescriber step "++ (fromAtom atom) ++ 
	         " should have exactly one prescribing tag collection, not "++ show (length ls)
 where 
    (nd,_) = mkNode_ nodemap (CGSteps atom)
    (pred,_,l,succ) = context graph nd
    preds = pre graph nd
    labs  = catMaybes$ L.map (lab graph) preds
    isTags (CGTags _) = True
    isTags _ = False


