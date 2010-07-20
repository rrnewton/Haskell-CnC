{-# LANGUAGE RecordWildCards #-}
----------------------------------------------------------------------------------------------------
-- This module implements a pass over the parser output that coalesces
-- all the relations in a .cnc file into a more manageable graph type.
----------------------------------------------------------------------------------------------------

-- 

module GatherGraph where
import AST
import Data.Map hiding (empty)
import Data.Set as Set 
import StringTable.Atom 

import Data.Graph.Inductive as G
import Data.Graph.Inductive.Example

main :: IO ()
main = return ()

m486 :: NodeMap String
m486 = fromGraph clr486

t1 :: Gr String ()
t1 = insMapEdge m486 ("shirt", "watch", ()) clr486

t2 :: Gr String ()
t2 = insMapEdge m486 ("watch", "pants", ()) t1




t3 :: Gr Char String
t3 = run_ G.empty $
    do insMapNodeM 'a'
       insMapNodeM 'b'
       insMapNodeM 'c'
       insMapEdgesM [('a', 'b', "right"),
		     ('b', 'a', "left"),
		     ('b', 'c', "down"),
		     ('c', 'a', "up")]


blah  :: Gr String ()
blah  = mkGraph (zip [1..9] ["shorts","socks","watch","pants","shoes",
                              "shirt","belt","tie","jacket"])
                 (labUEdges [(1,4),(1,5),(2,5),(4,5),(4,7),(6,7),(6,8),(7,9),(8,9)])


type TagFun = ()
type ColName = Atom

data CncGraphNode  = 
    CGSteps ColName 
  | CGTags  ColName 
  | CGItems ColName 
 deriving (Eq, Ord, Show)


mkTagFun :: [Exp a] -> [Exp a] -> TagFun
mkTagFun _ _ = ()

t4 :: Gr CncGraphNode String
t4 = run_ G.empty $
    do let a = CGSteps (toAtom "foo")
	   b = CGTags  (toAtom "bar")
	   c = CGItems (toAtom "baz")
       insMapNodeM a 
       insMapNodeM b
       insMapNodeM c
       insMapEdgesM [(b, a, "prescribe"),
       		     (a, c, "produce"),
       		     (c, a, "get")]

t5 :: Gr CncGraphNode TagFun
t5 = run_ G.empty $
    do let a = CGSteps (toAtom "foo")
	   b = CGTags  (toAtom "bar")
	   c = CGItems (toAtom "baz")
       insMapNodeM a 
       insMapNodeM b
       insMapNodeM c
       insMapEdgesM [(b, a, ()),
       		     (a, c, ()),
       		     (c, a, ())]

--data CncGraph = CncGraph (Gr CncGraphNode TagFun)
type CncGraph =  (Gr CncGraphNode TagFun)

----------------------------------------------------------------------------------------------------

coalesceGraph :: [PStatement dec] -> CncGraph
coalesceGraph = 
   do undefined


--coalesceChain :: CollectionInstance dec -> [RelLink dec] -> CncGraph -> CncGraph
--coalesceChain prev [] graph = graph
--coalesceChain prev (hd:links) graph =
--   run_ graph $ 

data AllNodes = AllNodes {
  steps :: Set ColName,
  tags  :: Set ColName,
  items :: Set ColName
}

-- Based on global sets of nodes/items/tags classify collection references.
instToNode :: AllNodes -> CollectionInstance t -> CncGraphNode 
instToNode (AllNodes { .. }) inst = 
    let classify name | Set.member name steps = CGSteps name
		      | Set.member name tags  = CGTags  name
		      | Set.member name items = CGItems name 
                      | True                  = error$ "Collection was not declared: " ++ show name
    in case inst of 
        InstName name        -> classify $ toAtom name
	InstDataTags name _  -> 
	    case classify $ toAtom name of 
	      x@(CGItems _) -> x
	      _ -> error$ "instToNode: collection indexed with [] but was not an item collection: "++ show name


-- This is tedious, but here we simply go over the big list of statements that come out of
-- the parser and collect the declarations for items, steps, and tags.
collectNodes :: [PStatement dec] -> AllNodes
collectNodes [] = 
    AllNodes { steps= Set.empty, tags= Set.empty, items= Set.empty }
collectNodes (DeclareTags _ name _ : tl) = 
    let a@(AllNodes{..}) = collectNodes tl 
    in  a { tags= Set.insert (toAtom name) tags }

collectNodes (DeclareItems _ name _ : tl) = 
    let a@(AllNodes{..}) = collectNodes tl 
    in  a { items= Set.insert (toAtom name) items }

collectNodes (DeclareSteps _ name : tl) = 
    let a@(AllNodes{..}) = collectNodes tl 
    in  a { steps= Set.insert (toAtom name) steps }

collectNodes (_: tl) = collectNodes tl


-- Continue extending a graph with nodes from a parsed chain.
--coalesceChain :: CollectionInstance dec -> [RelLink dec] -> NodeMapM CncGraphNode TagFun Gr ()
--coalesceChain :: CncGraphNode -> [RelLink dec] -> NodeMapM CncGraphNode TagFun Gr ()
coalesceChain allnodes prev prevTF [] = return ()
coalesceChain allnodes prev prevTF (hd:links) =
   case hd of 
     ProduceLink _ insts -> 
--	 case instToNode allnodes $ head insts of 
--	   CGItems 
--let node = CGItems $ toAtom str
       case head insts of 
         InstDataTags str exps ->
	     do let node = instToNode allnodes (head insts) 
		insMapNodeM node
		-- Connect the previous node to this one using a forward edge:
		-- This is tricky because depending on whether the left or the right hand
		-- side is the step collection the tag expressions are interpreted
		-- differently.
		case prev of 
		  CGSteps _ -> insMapEdgeM (prev, node, mkTagFun prevTF exps)
		  _ -> error$ "Only a step collection may produce output to item collection "++ show str
		return ()


----------------------------------------------------------------------------------------------------
{-
data StepEntry = 
  StepEntry {
    sePrescribingTagCol :: ColName,
    -- The step collection doesn't store all the info for its connected tag/item
    -- collections.  Rather, it stores the name of each, together with the tag function
    -- that relates this particular step to each collection.
    seOutTags :: Set (ColName, TagFun),
    seInData  :: Set (ColName, TagFun),
    seOutData :: Set (ColName, TagFun)
  }
-- TagCol OutTags InData OutData

data TagEntry = 
  TagEntry {
  }

data ItemEntry = 
  ItemEntry {
  }


data CncGraph = 
  CncGraph { 
    steps :: Map ColName StepEntry, 
    tags  :: Map ColName  TagEntry, 
    items :: Map ColName ItemEntry
  }
-}
