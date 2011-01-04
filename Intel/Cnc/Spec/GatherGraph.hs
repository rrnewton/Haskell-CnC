{-# LANGUAGE RecordWildCards, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
----------------------------------------------------------------------------------------------------
-- This module implements a pass over the parser output that coalesces
-- all the relations in a .cnc file into a more manageable graph type.
--
-- The main entrypoint is "assembleSpec".
----------------------------------------------------------------------------------------------------

module Intel.Cnc.Spec.GatherGraph ( coalesceGraph,
				    exampleGraph, tests_gathergraph
				  ) where
import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.TagFun
import Intel.Cnc.Spec.CncGraph
import Intel.Cnc.Spec.SrcLoc
import Intel.Cnc.Spec.Util

import Data.Map as Map
import Data.List as L
import Data.Maybe
import StringTable.Atom 
import StringTable.AtomMap as AM
import StringTable.AtomSet as AS
import Control.Monad
import Data.Graph.Inductive as G
--import Data.Graph.Inductive.NodeMap as NM
import Debug.Trace

import Test.HUnit

----------------------------------------------------------------------------------------------------

-- Transform the not-directly-useful list of parsed statements into a real graph datatype.
coalesceGraph :: String -> [PStatement SrcSpan] -> CncSpec
coalesceGraph name parsed = 
   -- trace ("Got ALL nodes "++ show (L.map fst$ AM.toList$ tags allnodes) ++"\nsteps "++ 
   -- 	  show (AS.toList$ steps allnodes) ++"\nitems "++ show (L.map fst$ AM.toList$ items allnodes)) $ 
   allnodes { graph=g2, appname=name, nodemap=nm, realmap= rm }
 where 
  g1 :: CncGraph = run_ G.empty $ 
       -- First add all the nodes to the graph
       do --mapM_ declare parsed 
	  --mapM_ (insMapNodeM . CGSteps) builtinSteps 
          mapM_ (insMapNodeM . CGSteps) (AS.toList$ steps allnodes)
          mapM_ (insMapNodeM . CGTags)  (L.map fst$ AM.toList$ tags  allnodes)
          mapM_ (insMapNodeM . CGItems) (L.map fst$ AM.toList$ items allnodes)
          mapM_ (insMapNodeM . CGReductions) (L.map fst$ AM.toList$ reductions allnodes)

  (_,(nm,g2)) = --trace ("Done collecting declares.. all nodes: " ++ show g1) $ 
       run g1 $ forM_ parsed collect -- Then add the edges.

  rm = Map.fromList$ Prelude.map (\ (a,b) -> (b,a)) $ labNodes g2

  allnodes = collectInsts parsed $ collectDecls parsed
  collect stmt = 
   case stmt of 
      Chain start links -> coalesceChain allnodes start links
      Function           -> return ()
      Constraints _ _ _  -> return ()
      DeclareExtern      -> return ()
      DeclareTags  _ _ _ -> return ()
      DeclareSteps _ _   -> return ()
      DeclareItems _ _ _         -> return ()
      DeclareReductions _ _ _ _ _ -> return ()
      TypeDef      _ _ _ -> return ()


-- This is tedious, but here we simply go over the big list of statements that come out of
-- the parser and collect the declarations for items, steps, and tags.
collectDecls :: [PStatement dec] -> CncSpec
collectDecls [] = seedWorld
collectDecls (DeclareTags  s name ty : tl) = extendTags  name ty $ (checkDup name$ collectDecls tl)
collectDecls (DeclareItems s name ty : tl) = extendItems name ty $ (checkDup name$ collectDecls tl)
collectDecls (DeclareSteps s name    : tl) = extendSteps name    $ (checkDup name$ collectDecls tl)
collectDecls (DeclareReductions s name op exp tys : tl) = 
       extendReductions name (op, mapDecor (const ()) exp, tys)
       $ (checkDup name$ collectDecls tl)
collectDecls (_ : tl) = collectDecls tl

-- A lot of other miscellaneous contortion here to the end of supporting legacy syntax:
--extendItems name ty (a@(CncSpec{..})) = a { items= AM.insert name (mergeTy name ty a) items }
extendItems name ty (a@(CncSpec{..})) = a { items= AM.insert name (mergeTy name ty items) items }
extendTags  name ty (a@(CncSpec{..})) = a { tags = AM.insert name (mergeTy name ty tags)  tags }
extendSteps name    (a@(CncSpec{..})) = a { steps= AS.insert name steps }
extendReductions name opty (a@(CncSpec{..})) = a { reductions= AM.insert name opty reductions }
--   a { reductions= AM.insert name (op, mergeTy name ty reductions) items }

checkDup atom all = 
    if cncSpecMember atom all
    then error$ "Duplicate declaration of name: '"++ (fromAtom atom) ++"'"
    else all
cncSpecMember atom (CncSpec{..}) =
    AM.member atom tags       ||
    AS.member atom steps      ||
    AM.member atom reductions ||
    AM.member atom items

------------------------------------------------------------
-- TEMP: For the benefit of the legacy syntax we harvest from instances as well as decls:
collectInsts (Chain hds links : tl) root = 
    let acc = foldl extendWithInstance (collectInsts tl root) hds in
    foldl extendWithLink acc links
collectInsts (_ : tl) root = collectInsts tl root
collectInsts [] root = root

extendWithInstance acc inst =
  case inst of 
    InstStepCol _ name ls -> extendSteps (toAtom name) acc
    InstItemCol _ name ls -> extendItems (toAtom name) Nothing acc
    InstTagCol  _ name ls -> extendTags  (toAtom name) Nothing acc
--    _                   -> acc
--    InstReductionCol _ name ls -> extendReductions (toAtom name) Nothing acc

-- These forms are ambiguous, so there is no extra information here:
    InstName _ _         -> acc
    InstStepOrTags _ _ _ -> acc
--    InstStepOrTags _ _ _ -> error$ "extendWithInstance: InstStepOrTags should have been desugared by now:\n " ++ show inst

extendWithLink acc link = 
  case link of 
     ProduceLink    _ insts -> foldl extendWithInstance acc insts
     RevProduceLink _ insts -> foldl extendWithInstance acc insts
     PrescribeLink  _ insts -> foldl extendWithInstance acc insts

-- Merge type info if there's already an entry:
mergeTy name ty amap = 
   case (maybeToList ty ++ 
	 maybeToList (collapseMaybe (AM.lookup name amap))) of 
     []        -> Nothing
     [ty2]     -> Just ty2
     [ty1,ty2] -> if ty1 == ty2  -- Types are nominal at the moment, no unification:
     		  then Just ty1
     		  else error$ "mergeTy: different types for "++fromAtom name++" "++ show(pp ty1) ++" and "++ show(pp ty2)
     _ -> error "mergTy, internal error"

----------------------------------------------------------------------------------------------------
    
-- Continue extending a graph with nodes from a parsed chain.
coalesceChain :: CncSpec -> [CollectionInstance SrcSpan] -> [RelLink SrcSpan] 
	      -> NodeMapM CncGraphNode (Maybe TagFun) Gr ()
coalesceChain allnodes start ls = loop (process start) ls 
 where 
  process = L.map (\x -> (instToNode allnodes x, instToExps x)) 
  loop prevs [] = return ()
  loop prevs (hd:tl) = 
    -- FIXME: inefficent redundant classifications
    let produce prevs insts next = 
         forM_ insts $ \ (node,exps) -> 
         forM_ prevs $ \ (pnode,pexps) -> 
           let 
	       insert         = insMapEdgeM (pnode, node, mkTagFun (show (node,pnode)) exps pexps) 
	       insert_flipped = insMapEdgeM (pnode, node, mkTagFun (show (pnode,node)) pexps exps) 
	   in
	   do case (pnode, node) of 
	        -- Valid combinations for a producer relation:
		-- This is tricky because depending on whether the left or the right
		-- hand side is the step collection the tag expressions are interpreted
		-- differently.
	        (CGSteps _, CGItems _)      -> insert_flipped
	        (CGSteps _, CGReductions _) -> insert_flipped
	        (CGSteps _, CGTags _)       -> insert_flipped

                -- NOTE: Tag functions always relative to the step collection.
		-- Flip the tag components for generating the tag function:
	        (CGItems _, CGSteps _)      -> insert
	        (CGReductions _, CGSteps _) -> insert

	        (l,r) -> error$ "coalesceChain: invalid put/get relation from '"++graphNodeName l++"' to '"++graphNodeName r++"'"
              loop next tl
    in
    case hd of 
     -- Connect the previous node to this one using a forward edge:
     ProduceLink _ insts    -> let pi = process insts in produce prevs pi pi
     -- These mean the same thing but are written backwards:
     RevProduceLink s insts -> let pi = process insts in produce pi prevs pi
     PrescribeLink _ insts -> 
       let processed = process insts in
       forM_ (process insts) $ \ (node,exps) -> 
       forM_ prevs $ \ (pnode,pexps) -> 
	   -- This is a bit simpler because there is only one valid prescribe:
	   do case (pnode, node) of 
	        (CGTags _, CGSteps _) -> 
		    insMapEdgeM (pnode, node, mkTagFun (show (node,exps)) exps pexps)
	        (l,r) -> error$ "coalesceChain: invalid prescribe relation from '"++graphNodeName l++"' to '"++graphNodeName r++"'"
              loop processed tl


-- FIXME: classification should happen as a previous pass that pays more attention to context:
--
-- Based on global sets of nodes/items/tags classify collection references.
-- That is, turn "Instances" in the parse into real graph nodes.
instToNode :: CncSpec -> CollectionInstance t -> CncGraphNode 
instToNode (CncSpec { .. }) inst = 
    let classify name | AS.member name steps = CGSteps name
		      | AM.member name tags  = CGTags  name
		      | AM.member name items = CGItems name 
		      | AM.member name reductions = CGReductions name 
--		      | isBuiltin (fromAtom name) = CGSteps name
                      | True                      = error$ "Collection was not declared: " ++ show name
    in case inst of 
        InstName    _ name    -> classify $ toAtom name
	InstItemCol _ name _  -> 
	    case classify $ toAtom name of 
	      x@(CGItems _) -> x
	      _ -> error$ "instToNode: collection indexed with [] but was not an item collection: "++ show name

-- FIXME: these are currently only for the LEGACY syntax:
	InstStepCol _ name _  -> CGSteps$ toAtom name
	InstTagCol  _ name _  -> CGTags$  toAtom name

--        InstReductionCol _ _ _ -> TODO

	InstStepOrTags _ name _  -> 
	    case classify $ toAtom name of 
	      x@(CGTags  _) -> x
	      x@(CGSteps _) -> x
	      _ -> error$ "instToNode: Error, collection is an item collection but is indexed with (), not [] : "++ show name


instToExps :: CollectionInstance t -> [Exp t]
instToExps inst =
  case inst of 
   InstName       _ _      ->  []
   InstStepOrTags _ _ exps -> exps
   InstStepCol    _ _ exps -> exps
   InstItemCol    _ _ exps -> exps
   InstTagCol     _ _ exps -> exps
    
----------------------------------------------------------------------------------------------------

seedWorld =
  CncSpec {  steps= AS.fromList builtinSteps
	  ,  tags=  AM.empty
	  ,  items= AM.empty
	  ,  reductions= AM.empty
	  ,  graph    = error "CncSpec: graph uninitialized"
	  ,  nodemap  = error "CncSpec: nodemap uninitialized"
	  ,  appname  = error "CncSpec: appname uninitialized"
	  ,  realmap  = error "CncSpec: realmap uninitialized"
	  ,  harchtree= error "CncSpec: harchtree uninitialized"
	  }

-- A very simple graph for testing:
exampleGraph =   
  coalesceGraph "foo" $
  L.map (mapDecor (\_ -> UnhelpfulSpan "")) $
  [  DeclareTags () (toAtom "T") (Just (TSym (toAtom "int")))
  ,  DeclareSteps () (toAtom "S")
  ,  DeclareItems () (toAtom "I") Nothing
  ,  Chain [InstName () "T"] [PrescribeLink () [InstName () "S"]]
  ,  Chain [InstName () "S"] [ProduceLink () [InstName () "I"]]
  ]


tests_gathergraph = 
    testSet "GatherGraph" 
      [ testCase "" "getStepPrescriber: T should be the prescriber to S "$  
	(toAtom "T") ~=? getStepPrescriber exampleGraph (toAtom "S") 
      ]


----------------------------------------------------------------------------------------------------

