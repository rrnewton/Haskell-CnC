{-# LANGUAGE CPP, NamedFieldPuns, ScopedTypeVariables #-}


module Intel.Cnc.Spec.Codegen.Plugins.TagFunCorrectness
  (
   tagfunCorrectnessPlugin
  )
where

import Control.Monad
import qualified Data.Map as M
import Data.Graph.Inductive hiding (empty)

import Intel.Cnc.Spec.TagFun
import Intel.Cnc.Spec.Codegen.Plugins
import Intel.Cnc.Spec.CncGraph

import Text.PrettyPrint.HughesPJClass 
-- import Intel.Cnc.EasyEmit  hiding (not, (||))
import qualified Intel.Cnc.EasyEmit as EE
import Intel.Cnc.EasyEmit hiding (app, not, (&&), (==), (||))

tagfunCorrectnessPlugin :: CodeGenPlugin
tagfunCorrectnessPlugin (spec @ CncSpec{appname, steps, tags, items, reductions, graph, realmap})
			stpname = 
  Just$ defaultHooksTable 
  {
     beforeItemPut    = boilerplate CGItems      lpre' "Item Put",
     beforeItemGet    = boilerplate CGItems      lsuc' "Item Get",
     beforeReducerPut = boilerplate CGReductions lpre' "Reducer Put",
     beforeReducerGet = boilerplate CGReductions lsuc' "Reducer Get"
  }
 where 
  fprintf = function$ Syn$t "fprintf"
  stderr  = constant "stderr"
  abort   = function$ Syn$t "abort"		
		     
  boilerplate constructor getnbrs msg (privC, mainC, to) (tag,val) = do
    let 
        target = constructor to
	stpnd  = CGSteps stpname
	itemnd = realmap M.! target
	gctxt  = context graph itemnd
	lnhbrs :: [(Node, Maybe TagFun)] = getnbrs gctxt
--	access_edges = filter (\ (nd,lab) -> nd == stpname) lnhbrs

    return ()


-- shorthands: 
s = Syn . text 
t = text

#if 0  

  doplugs project = execEasyEmit$ 
      -- FIXME: HACK:
      when (length args Prelude.>= 2) $
      forM_ (AM.findWithDefault [] stp plug_map) $ \ hooks ->
	  project hooks (Syn$ t$ snd$ head args, Syn$ t$ snd$ head$ tail args, colName)
			(Syn$ t$ snd$ head args, Syn$ t$ snd$ head$ tail args)


checkTagFun stp target getnbrs     fprintf stderr abort tagty args realmap graph =  
      -- TODO INSERT CORRECTNESS CHECKING HERE:
        stpnd  = realmap M.! (CGSteps stp)
	itemnd = realmap M.! target
	gctxt  = context graph itemnd
	lnhbrs = getnbrs gctxt-- lpre' gctxt ++ lsuc' gctxt
	ls =  filter (\ (nd,lab) -> nd == stpnd) lnhbrs
	  -- [(_,tf)] = filter (\ (nd,lab) -> nd == stpnd)$  lpre' gctxt				            
	  --[(_,tf)] = trace (" LENGTH "++ show (length ls) ++" "++ show stpnd ++" "++ show itemnd++" "++ show lnhbrs) ls
      in 
	 execEasyEmit$
	 case ls of 
	   -- If we have no relationship to that collection its an error to access it:

	   [] -> do let str = printf " [tagfun_check] CnC graph violation: should not access collection '%s' from '%s'" 
				     (graphNodeName target :: String) (show stp) 
		    EE.app fprintf [stderr, stringconst$ str]
		    EE.app abort[]

	   [(_,Nothing)] -> comm " [tagfun_check] No tag function to check..."

	   --((_,Just (TF args exps)) : []) -> 

	   ((_,Just (TF tfargs exps)) : tl) -> -- TEMPTOGGLE ... permissive, ignoring additional tag functions!
	     case (tfargs,exps) of  
	       ([arg], [exp]) -> 
		  do comm (" [tagfun_check] Checking tag function "++ show arg ++" -> "++ show exp)
		     -- TODO: use normal variable decl here:
		     putD$ tagty <+> (fromAtom arg) <+> t"=" <+> t"* m_context->tag" <> semi
		     when (not$ null args)$ assert (head args EE.== Syn (pPrint exp)) -- FIXME -- afer factoring fix this head args
	       _ -> error "internal error: tag function correctness not fully implemented yet.  Finish me."

	   _ -> error$ "internal error: tag function correctness codegen: \n w"++show ls



 putD$ (if gendebug -- Optionally include debugging assertions.
	then checkTagFun stp ((if isReduction then CGReductions else CGItems) colName) 
			 (if isPut then lpre' else lsuc') 
			 fprintf stderr abort tagty args realmap graph
	else empty)
#endif
