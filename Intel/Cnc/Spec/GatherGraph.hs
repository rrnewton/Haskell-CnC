{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
----------------------------------------------------------------------------------------------------
-- This module implements a pass over the parser output that coalesces
-- all the relations in a .cnc file into a more manageable graph type.
----------------------------------------------------------------------------------------------------

-- 

module Intel.Cnc.Spec.GatherGraph where
import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.SrcLoc

import Data.Map        hiding (empty,map)
import Data.Set as Set hiding (map)
import StringTable.Atom 
import Control.Monad
import Debug.Trace

import Data.Graph.Inductive as G

----------------------------------------------------------------------------------------------------

builtinSteps = [toAtom "env"]

--data CncGraph = CncGraph (Gr CncGraphNode TagFun)
type CncGraph =  (Gr CncGraphNode (Maybe TagFun))

type ColName = Atom

data CncGraphNode  = 
    CGSteps ColName 
  | CGTags  ColName 
  | CGItems ColName 
 deriving (Eq, Ord, Show)


-- The left argument is the "formal parameter" and the right argument the "body".
-- But really it's just an equality.
-- Note, the reason these are Exp LISTS is because these are implicitly tuples.
-- The tag functions are multi-dimensional.
mkTagFun exps1 exps2 = 
 let e1s = map checkConvertTF exps1
     e2s = map checkConvertTF exps2
 in if all isTEVar e1s
    then if length exps1 == length exps2 
         then Just (TF (map unTEVar e1s) e2s)
	 else if Prelude.null exps2 
	      then Nothing -- It's ok to simply leave off a tag function (but to have some var names on the step).
	      else error$ "It is not acceptable to use the following tag expressions without\n"++
		          "the same number of corresponding tag components indexing the step: "
		          ++ (show$ pp exps2)
    else error$ "Presently the tag expressions indexing step collections must be simple variables, not: " 
	        ++ (show$ pp exps1)

isTEVar (TEVar _) = True
isTEVar _ = False

-- Avoid exhaustiveness warnings here:
unTEVar = \ (TEVar name) -> name

-- This is where we convert arbitrary Exps into more restricted tag expressions that
-- support symbolic manipulation.
checkConvertTF e = 
  case e of 
    Lit s l -> case l of 
	         LitInt i -> TEInt i; 
		 _ -> locErr s "Only integer literals supported in tag functions presently."
    Var s name -> TEVar name 
    App _ (Var _ name) rands -> TEApp name (map checkConvertTF rands)
    App s _ _  -> locErr s "Only very simple function applications allowed in tag functions presently."
    If s _ _ _ -> locErr s "Conditionals disallowed in tag functions."

----------------------------------------------------------------------------------------------------

-- Transform the not-directly-useful list of parsed statements into a real graph datatype.
coalesceGraph :: [PStatement SrcSpan] -> CncGraph
coalesceGraph parsed = 
    -- First add all the nodes to the graph
    let g1 :: CncGraph = run_ G.empty $ 
	      do mapM_ declare parsed 
		 mapM_ (insMapNodeM . CGSteps) builtinSteps 
    in
      trace ("Done collecting declares.. all nodes: " ++ show g1) $ 
      run_ g1 $ forM_ parsed collect -- Then add the edges.
 where 
  declare stmt = 
   case stmt of 
      Chain start links  -> return ()
      Function           -> return ()
      Constraints _ _ _  -> return ()
      DeclareExtern      -> return ()
      DeclareTags  _ n _ -> do insMapNodeM $ CGTags $toAtom n; return ()
      DeclareItems _ n _ -> do insMapNodeM $ CGItems$toAtom n; return ()
      DeclareSteps _ n   -> do insMapNodeM $ CGSteps$toAtom n; return ()
  allnodes = collectNodes parsed
  collect stmt = 
   case stmt of 
      Chain start links -> coalesceChain allnodes start links
      Function           -> return ()
      Constraints _ _ _  -> return ()
      DeclareExtern      -> return ()
      DeclareTags  _ _ _ -> return ()
      DeclareItems _ _ _ -> return ()
      DeclareSteps _ _   -> return ()


-- Collect a global set of all collection names.
data AllNodes = AllNodes {
  steps :: Set ColName,
  tags  :: Set ColName,
  items :: Set ColName
}



-- Based on global sets of nodes/items/tags classify collection references.
-- That is, turn "Instances" in the parse into real graph nodes.
instToNode :: AllNodes -> CollectionInstance t -> CncGraphNode 
instToNode (AllNodes { .. }) inst = 
    let classify name | Set.member name steps = CGSteps name
		      | Set.member name tags  = CGTags  name
		      | Set.member name items = CGItems name 
--		      | isBuiltin (fromAtom name) = CGSteps name
                      | True                      = error$ "Collection was not declared: " ++ show name
    in case inst of 
        InstName name        -> classify $ toAtom name
	InstDataTags name _  -> 
	    case classify $ toAtom name of 
	      x@(CGItems _) -> x
	      _ -> error$ "instToNode: collection indexed with [] but was not an item collection: "++ show name

	InstControlTags name _  -> 
	    case classify $ toAtom name of 
	      x@(CGTags  _) -> x
	      x@(CGSteps _) -> x
	      _ -> error$ "instToNode: Error, collection is an item collection but is indexed with (), not [] : "++ show name


instToExps :: CollectionInstance t -> [Exp t]
instToExps (InstName        _)      = []
instToExps (InstDataTags    _ exps) = exps
instToExps (InstControlTags _ exps) = exps
    

-- This is tedious, but here we simply go over the big list of statements that come out of
-- the parser and collect the declarations for items, steps, and tags.
-- collectNodes :: [PStatement dec] -> AllNodes
-- collectNodes [] = 
--     AllNodes { steps= Set.empty, tags= Set.empty, items= Set.empty }
-- collectNodes (DeclareTags _ name _ : tl) = 
--     let a@(AllNodes{..}) = collectNodes tl 
--     in  a { tags= Set.insert (toAtom name) tags }

-- collectNodes (DeclareItems _ name _ : tl) = 
--     let a@(AllNodes{..}) = collectNodes tl 
--     in  a { items= Set.insert (toAtom name) items }

-- collectNodes (DeclareSteps _ name : tl) = 
--     let a@(AllNodes{..}) = collectNodes tl 
--     in  a { steps= Set.insert (toAtom name) steps }

-- collectNodes (_: tl) = collectNodes tl

seedWorld =
  AllNodes { steps= Set.fromList builtinSteps,
	     tags=  Set.empty, 
	     items= Set.empty }

collectNodes :: [PStatement dec] -> AllNodes
collectNodes [] = seedWorld
collectNodes (DeclareTags  s name _ : tl) = extendTags  s name $ collectNodes tl
collectNodes (DeclareItems s name _ : tl) = extendItems s name $ collectNodes tl
collectNodes (DeclareSteps s name   : tl) = extendSteps s name $ collectNodes tl
collectNodes                     (_ : tl) = collectNodes tl

extendItems span name (a@(AllNodes{..})) =
    let atom = checkDup a name in
    a { items= Set.insert atom items }
-- Annoying duplicated code:
extendSteps span name (a@(AllNodes{..})) =
    let atom = checkDup a name in
    a { steps= Set.insert atom steps }

extendTags span name  (a@(AllNodes{..})) =
    let atom = checkDup a name in
    a { tags= Set.insert atom tags }

checkDup all name = 
    let atom = toAtom name in
    if allNodesMember atom all
    then error "DUP" 
    else atom

allNodesMember atom (AllNodes{..}) =
    Set.member atom tags  ||
    Set.member atom steps ||
    Set.member atom items
    
allDeclaredNames :: [PStatement dec] -> [String]
allDeclaredNames [] = []
allDeclaredNames (DeclareTags _ name _  : tl) = name : allDeclaredNames tl
allDeclaredNames (DeclareItems _ name _ : tl) = name : allDeclaredNames tl
allDeclaredNames (DeclareSteps _ name   : tl) = name : allDeclaredNames tl
allDeclaredNames                      (_: tl) =        allDeclaredNames tl



-- Continue extending a graph with nodes from a parsed chain.
coalesceChain :: Show dec => AllNodes -> [CollectionInstance dec] -> [RelLink dec] -> NodeMapM CncGraphNode (Maybe TagFun) Gr ()
coalesceChain allnodes start ls = loop (process start) ls 
 where 
  process = Prelude.map (\x -> (instToNode allnodes x, instToExps x)) 
  loop prevs [] = return ()
  loop prevs (hd:tl) = 
    -- FIXME: inefficent redundant classifications
    let produce prevs insts next = 
         forM_ insts $ \ (node,exps) -> 
         forM_ prevs $ \ (pnode,pexps) -> 
	   do case (pnode, node) of 
	        -- Valid combinations for a producer relation:
	        (CGItems _, CGSteps _) -> trace ("Adding "++show (pnode,node))$ insMapEdgeM (pnode, node, mkTagFun exps pexps)
	        (CGSteps _, CGItems _) -> trace ("Adding "++show (pnode,node))$ insMapEdgeM (pnode, node, mkTagFun pexps exps)
	        (CGSteps _, CGTags _)  -> trace ("Adding "++show (pnode,node))$ insMapEdgeM (pnode, node, mkTagFun pexps exps)
	        _                      -> error$ "coalesceChain: FIXME "++ show (pnode,node)
              loop next tl
    in
    case hd of 
     -- Connect the previous node to this one using a forward edge:
     ProduceLink _ insts    -> let pi = process insts in produce prevs pi pi
     RevProduceLink s insts -> let pi = process insts in produce pi prevs pi
     PrescribeLink _ insts -> 
       let processed = process insts in
       forM_ (process insts) $ \ (node,exps) -> 
       forM_ prevs $ \ (pnode,pexps) -> 
	   -- This is a bit simpler because there is only one valid prescribe:
	   do case (pnode, node) of 
	        (CGTags _, CGSteps _) -> trace ("Adding "++show (pnode,node))$ insMapEdgeM (pnode, node, mkTagFun exps pexps)	  
	        _                      -> error$ "coalesceChain: FIXME2 " ++ show (pnode,node)
              loop processed tl

-- This is tricky because depending on whether the left or the right
-- hand side is the step collection the tag expressions are interpreted
-- differently.


{-
         case this of 
           InstDataTags str exps ->
	       -- This is an item collection:
  	       do let node = instToNode allnodes this 
	  	  insMapNodeM node
		  -- This is tricky because depending on whether the left or the right
		  -- hand side is the step collection the tag expressions are interpreted
		  -- differently.
		  case prev of 
		    CGSteps _ -> insMapEdgeM (prev, node, mkTagFun prevTF exps)
		    _ -> error$ "Only a step collection may produce output to item collection "++ show str
		  return ()
-}

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
