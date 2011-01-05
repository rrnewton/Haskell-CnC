{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NamedFieldPuns #-}
{- 

  The reduction-done plugin extends autodone by introducing counters
  for reduction collections and signalling all_done().

 -}
module Intel.Cnc.Spec.Codegen.Plugins.ReductionDone
  (
    reductionDonePlugin
  )
where
import Intel.Cnc.Spec.Codegen.Plugins
import Intel.Cnc.Spec.CncGraph
import Intel.Cnc.EasyEmit  hiding (not, (||))
import Control.Monad


reductionDonePlugin :: DonePlugin
reductionDonePlugin = DonePlugin $ 
  \ debug_autodone node -> 
    if isReductionC node then Just$ 
      do let redC = graphNodeName node

         comm "[reduction_done] When the dependencies of a reduction collection are done, signal all_done() on it"
	 when debug_autodone$
	      app (function "printf") [stringconst$ " [reduction_done] Signaling all_done() for "++ show redC ++".\n"]
	 app (function (atomToSyn redC `dot` "all_done")) []

    else Nothing

