{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
----------------------------------------------------------------------------------------------------
-- This module implements a pass over the parser output that coalesces
-- all the relations in a .cnc file into a more manageable graph type.
--
-- The main entrypoint is "assembleSpec".
----------------------------------------------------------------------------------------------------

module Intel.Cnc.Spec.GatherGraph where
import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.SrcLoc

import Data.List
import Data.Maybe
import Data.Map        hiding (empty,map)
import Data.Set as Set hiding (map)
import StringTable.Atom 
import StringTable.AtomMap as AM
import StringTable.AtomSet as AS
import Control.Monad
import Debug.Trace

import Text.PrettyPrint.HughesPJClass
import Data.Graph.Inductive as G
--import Data.Graph.Inductive.NodeMap as NM

----------------------------------------------------------------------------------------------------

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


instance Show CncSpec where
  show = show . pPrint

instance Pretty CncSpec where
  pPrint (CncSpec{..}) = 
      text "  All Steps:\n========================================"    $$ 
	   hcat (intersperse (text ", ") $ Prelude.map (text . fromAtom) $ AS.toList steps) $$
      text "\n  Tag Types:\n========================================" $$ 
	   sep (Prelude.map (\(x,y) -> pp((fromAtom x)::String,y)) $ AM.toList tags) $$
      text "\n  Item Types:\n========================================" $$ 
	   sep (Prelude.map (\(x,y) -> pp((fromAtom x)::String,y)) $ AM.toList items) $$
      text (show graph)


{-
assembleSpec :: [PStatement SrcSpan] -> CncSpec
assembleSpec ls = verifySpec $ 
  CncSpec {
    tagTypes  = foldl (\acc x -> case x of DeclareTags  _ name ty -> AM.insert name ty acc; _ -> acc) AM.empty ls,
    itemTypes = foldl (\acc x -> case x of DeclareItems _ name ty -> AM.insert name ty acc; _ -> acc) AM.empty ls,
    graph = coalesceGraph ls
  }
-}

----------------------------------------------------------------------------------------------------

-- TODO: Perform basic checks here.
--verifySpec :: CncSpec -> CncSpec
verifySpec spec = 
   spec
   -- All steps are prescribed.
   -- All tags/items have types (for now, for C++)
   -- 

----------------------------------------------------------------------------------------------------
-- The left argument is the "formal parameter" and the right argument the "body".
-- But really it's just an equality.
-- Note, the reason these are Exp LISTS is because these are implicitly tuples.
-- The tag functions are multi-dimensional.
mkTagFun exps1 exps2 = 
 let e1s = Prelude.map checkConvertTF exps1
     e2s = Prelude.map checkConvertTF exps2
 in if all isTEVar e1s
    then if length exps1 == length exps2 
         then Just (TF (Prelude.map unTEVar e1s) e2s)
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
    App _ (Var _ name) rands -> TEApp name (Prelude.map checkConvertTF rands)
    App s _ _  -> locErr s "Only very simple function applications allowed in tag functions presently."
    If s _ _ _ -> locErr s "Conditionals disallowed in tag functions."

----------------------------------------------------------------------------------------------------

-- Transform the not-directly-useful list of parsed statements into a real graph datatype.
coalesceGraph :: String -> [PStatement SrcSpan] -> CncSpec
coalesceGraph name parsed = 
   allnodes { graph=g2, appname=name, nodemap=nm }
 where 
  g1 :: CncGraph = run_ G.empty $ 
       -- First add all the nodes to the graph
       do mapM_ declare parsed 
	  mapM_ (insMapNodeM . CGSteps) builtinSteps 
  (_,(nm,g2)) = --trace ("Done collecting declares.. all nodes: " ++ show g1) $ 
       run g1 $ forM_ parsed collect -- Then add the edges.

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

-- This is tedious, but here we simply go over the big list of statements that come out of
-- the parser and collect the declarations for items, steps, and tags.
collectNodes :: [PStatement dec] -> CncSpec
collectNodes [] = seedWorld
collectNodes (DeclareTags  s name ty : tl) = extendTags  s name ty $ collectNodes tl
collectNodes (DeclareItems s name ty : tl) = extendItems s name ty $ collectNodes tl
collectNodes (DeclareSteps s name    : tl) = extendSteps s name    $ collectNodes tl
collectNodes                      (_ : tl) = collectNodes tl

extendItems span name ty (a@(CncSpec{..})) = a { items= AM.insert (checkDup a name) ty items }
extendTags  span name ty (a@(CncSpec{..})) = a { tags = AM.insert (checkDup a name) ty tags }
extendSteps span name    (a@(CncSpec{..})) = a { steps= AS.insert (checkDup a name) steps }

checkDup all atom = 
    if cncSpecMember atom all
    then error$ "Duplicate declaration of name: "++ (fromAtom atom) 
    else atom
cncSpecMember atom (CncSpec{..}) =
    AM.member atom tags  ||
    AS.member atom steps ||
    AM.member atom items


----------------------------------------------------------------------------------------------------
    
-- Continue extending a graph with nodes from a parsed chain.
coalesceChain :: Show dec => CncSpec -> [CollectionInstance dec] -> [RelLink dec] 
	      -> NodeMapM CncGraphNode (Maybe TagFun) Gr ()
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
		-- This is tricky because depending on whether the left or the right
		-- hand side is the step collection the tag expressions are interpreted
		-- differently.
	        (CGItems _, CGSteps _) -> insMapEdgeM (pnode, node, mkTagFun exps pexps)
	        (CGSteps _, CGItems _) -> insMapEdgeM (pnode, node, mkTagFun pexps exps)
	        (CGSteps _, CGTags _)  -> insMapEdgeM (pnode, node, mkTagFun pexps exps)
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
	        (CGTags _, CGSteps _) -> insMapEdgeM (pnode, node, mkTagFun exps pexps)	  
	        _                      -> error$ "coalesceChain: FIXME2 " ++ show (pnode,node)
              loop processed tl

-- Based on global sets of nodes/items/tags classify collection references.
-- That is, turn "Instances" in the parse into real graph nodes.
instToNode :: CncSpec -> CollectionInstance t -> CncGraphNode 
instToNode (CncSpec { .. }) inst = 
    let classify name | AS.member name steps = CGSteps name
		      | AM.member name tags  = CGTags  name
		      | AM.member name items = CGItems name 
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
    
----------------------------------------------------------------------------------------------------

seedWorld =
  CncSpec {  steps= AS.fromList builtinSteps
	  ,  tags=  AM.empty
	  ,  items= AM.empty
	  ,  graph  = error "CncSpec: graph uninitialized"
	  ,  nodemap= error "CncSpec: nodemap uninitialized"
	  ,  appname= error "CncSpec: appname uninitialized"
	  }

example =   
  coalesceGraph "foo" $
  Prelude.map (mapDecor (\_ -> UnhelpfulSpan "")) $
  [  DeclareTags () (toAtom "T") (Just (TSym (toAtom "int")))
  ,  DeclareSteps () (toAtom "S")
  ,  Chain [InstName "T"] [PrescribeLink () [InstName "S"]]
  ]

-- T should be the prescriber to S:
-- FIXME TODO: Set up HUnit here...
testGetStepPrescriber = getStepPrescriber example (toAtom "S") 

----------------------------------------------------------------------------------------------------

-- Get the name of the tag collection that prescribes a given step.
getStepPrescriber :: CncSpec -> ColName -> ColName
getStepPrescriber (CncSpec{..}) atom = 
  case Prelude.filter isTags labs of 
    [CGTags t] -> t
    ls -> error$ "getStepPrescriber step "++ (fromAtom atom) ++ 
	         " should have exactly one prescribing tag collection, not "++ show (length ls)
 where 
    (nd,_) = mkNode_ nodemap (CGSteps atom)
    (pred,_,l,succ) = context graph nd
    preds = pre graph nd
    labs  = catMaybes$ Prelude.map (lab graph) preds
    isTags (CGTags _) = True
    isTags _ = False

