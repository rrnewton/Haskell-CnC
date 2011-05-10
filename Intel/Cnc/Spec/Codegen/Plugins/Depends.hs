{-# LANGUAGE CPP, NamedFieldPuns, ScopedTypeVariables #-}

module Intel.Cnc.Spec.Codegen.Plugins.Depends
  (
    dependsPlugin,
    all_tagfuns_tractible,
    tuner_name 
  )
where

import Control.Monad
import qualified Data.Map as M
import Data.Graph.Inductive hiding (empty)
import Data.Maybe

import Intel.Cnc.Spec.TagFun
import Intel.Cnc.Spec.Codegen.Plugins
import Intel.Cnc.Spec.CncGraph

import Text.PrettyPrint.HughesPJClass 
-- import Intel.Cnc.EasyEmit  hiding (not, (||))
import qualified Intel.Cnc.EasyEmit as EE
import Intel.Cnc.EasyEmit hiding (app, not, (&&), (==), (||), (/=))

import Intel.Cnc.Spec.Util 
import Intel.Cnc.Spec.AST
import StringTable.Atom
import qualified StringTable.AtomMap as AM

dependsPlugin :: CodeGenPlugin
dependsPlugin (spec @ CncSpec{appname, steps, tags, items, reductions, graph, realmap})
	      stpname = 
  if all_tagfuns_tractible graph stpname && 
     stpname /= toAtom special_environment_name
  then 
     Just$ defaultHooksTable 
     {  
       addTopLevel = \ (privCtx, Syn mainCtx) () -> do
         comm "[depends] Generate tuner with depends() function:"
	 cppStruct (Syn$ tuner_name stpname)
	 	   (Syn$ t"public CnC::default_tuner< "  <>
	 		 cppType tagty <> t", "<> 
	 		 mainCtx <> t" >") $
	  (do putS "bool preschedule() const { return false; }"
	      putS "template< class dependency_consumer >" 
	      constFunDef voidTy (s"depends") 
		     [TConst$ TRef$ tagty, 
		      TRef$ TSym$ toAtom$ render mainCtx, 
		      TRef$ TSym$ toAtom$ "dependency_consumer"] $ 
		     \ (Syn tag, contextref, deps) -> do
		       let dep = function$ Syn$t$ synToStr$ deps `dot` s"depends"
		       -- Now iterate through all data collections connected to the step collection:
		       forM_ (lpre graph$ realmap M.! (CGSteps stpname)) $ \ (nd, tgfn) -> 
			 case lab graph nd of 
			  Just (CGItems itC) -> do
			    comm$ "Dependency on collection "++ (graphNodeName$ fromJust$ lab graph nd) ++", tagfun " ++ show tgfn
			    case tgfn of 
			      Just fn -> forM_ (applyTagFun fn tag) $ \ resulttag -> 
					   EE.app dep [contextref `dot` Syn (toDoc itC), Syn resulttag]
			      Nothing -> comm "Ack.. tag function not available..."
			  _ -> return ()
	      return ()
	    )
         return ()
     }
  else Nothing
 where
   tagC = getStepPrescriber spec stpname
   tagty = case tags AM.! tagC of
	     Nothing -> error ""
	     Just ty -> ty 

-- shorthands: 
s = Syn . text 

-- This predicate determines whether we can successfully determine all
-- the data dependencies of a step, for example to generate a depends function.
all_tagfuns_tractible :: CncGraph -> Atom -> Bool
all_tagfuns_tractible graph step =
  True
 -- FIXME -- TODO -- FINISH THIS!!!


tuner_name stp = toDoc stp <> t"_tuner"




#if 0
   maybeComment [t"", t"Automatically generated tuners, where possible."]$
     when gendepends $ 
       forM_ stepls_with_types $ \ (stp,ty) -> 
	 when (AS.member stp tractible_depends_steps) $

	    cppStruct (Syn$ tuner_name stp)
		      (Syn$ t"public CnC::default_tuner< "  <>
			    cppType ty <> t", "<> 
			    maincontext <> t" >")
	     (do putS "bool preschedule() const { return false; }"
	         putS "template< class dependency_consumer >" 
		 constFunDef voidTy (s"depends") 
			[TConst$ TRef$ ty, 
			 TRef$ TSym$ toAtom$ render maincontext, 
			 TRef$ TSym$ toAtom$ "dependency_consumer"] $ 
			\ (Syn tag, contextref, deps) -> do
			  let dep = function$ Syn$t$ synToStr$ deps `dot` s"depends"
			  -- Now iterate through all data collections connected to the step collection:
			  forM_ (lpre graph$ realmap M.! (CGSteps stp)) $ \ (nd, tgfn) -> 
			    case lab graph nd of 
			     Just (CGItems itC) -> do
			       comm$ "Dependency on collection "++ (graphNodeName$ fromJust$ lab graph nd) ++", tagfun " ++ show tgfn
			       case tgfn of 
				 Just fn -> forM_ (applyTagFun fn tag) $ \ resulttag -> 
					      EE.app dep [contextref `dot` Syn (toDoc itC), Syn resulttag]
				 Nothing -> comm "Ack.. tag function not available..."
			     _ -> return ()
                 return ()
	       )

#endif