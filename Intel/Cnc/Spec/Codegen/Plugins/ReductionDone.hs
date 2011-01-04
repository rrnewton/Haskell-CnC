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
  let 
      debug_autodone = True
  in 
  \ node -> 
    if isReductionC node then Just$ 
      do let redC = graphNodeName node

         comm "[reduction_done] When the dependencies of a reduction collection are done, signal all_done() on it"
	 when debug_autodone$
	      app (function "printf") [stringconst$ " [reduction_done] Signaling all_done() for "++ show redC ++".\n"]
	 app (function (atomToSyn redC `dot` "all_done")) []


	 --     counter = countername 9999
	 -- x <- tmpvar TInt 
	 -- set x ((function (counter `dot` "fetch_and_increment")) [])
	 -- if_ (x == 1)
	 --     (do comm "[reduction_done] When the dependencies of a reduction collection are done, signal all_done() on it"
	 -- 	 when debug_autodone$
	 -- 	   app (function "printf") [stringconst$ " [reduction_done] Signaling all_done() for "++ show redC ++".\n"]
	 -- 	 app (function (atomToSyn redC `dot` "all_done")) [])
	 --     (return ())
    else Nothing

