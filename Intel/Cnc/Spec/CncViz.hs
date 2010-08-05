

----------------------------------------------------------------------------------------------------
-- This module includes visualization code which depends on various libraries.
----------------------------------------------------------------------------------------------------
module Intel.Cnc.Spec.CncViz where


import Intel.Cnc.Spec.CncGraph
import StringTable.Atom 
import Data.Graph.Inductive as G
import Data.Graph.Inductive.Query.DFS
import Control.Monad
import Data.Maybe

import Graphics.Ubigraph
import Data.GraphViz

----------------------------------------------------------------------------------------------------
-- Graph Visualization

-- Draw a cnc spec through graphviz:
cncGraphviz = 
  -- This is easy because the graphviz wrapper uses fgl.
  undefined


cncUbigraph :: CncGraph -> IO ()
cncUbigraph gr = 
  do putStrLn$ "DRAWING UBIGRAPH, total nodes "++ show (length sorted)
     initHubigraph server_url >>= runHubigraph go
  --r $ mkRing 10
 where 
  r x = initHubigraph server_url >>= runHubigraph x

  server_url = "http://127.0.0.1:20738/RPC2"
  sorted = topsort gr
  contexts = map (context gr) sorted

  go = do 
   clear
   stepstyle <- newVStyle 0
   itemstyle <- newVStyle 0
   tagstyle  <- newVStyle 0
   mapM_ (flip setVStyleAttr stepstyle) [VColor "#3333ff", VShape Sphere,     VSize 1.0, VShapedetail 10]
   mapM_ (flip setVStyleAttr itemstyle) [VColor "#008800", VShape Cube,       VSize 0.75]
   mapM_ (flip setVStyleAttr tagstyle)  [VColor "#555555", VShape Octahedron, VSize 0.4]

   producestyle  <- newEStyle 0
   mapM_ (flip setEStyleAttr producestyle)  [EColor "#ff4444", EWidth 2.5, EOriented True]

   consumestyle <- newEStyle 0
   mapM_ (flip setEStyleAttr consumestyle)  [EColor "#44ff44", EWidth 2.5, EOriented True]

   prescribestyle  <- newEStyle 0
   mapM_ (flip setEStyleAttr prescribestyle)  [EColor "#666666", EOriented True]


   forM_ contexts $ \ (prev, id, label, _) -> 
        do newVertexWithID id 
  	   case label of 
	     CGSteps atom -> 
		do changeVStyle stepstyle id		   
		   setVAttr (VLabel$ fromAtom atom) id 
		   if fromAtom atom == "env"
		     then do setVAttr (VColor "#ffff00") id 
			     setVAttr (VLabel "env IN") id 
			     newVertexWithID (-1)
			     changeVStyle stepstyle (-1)
			     setVAttr (VLabel "env OUT") (-1)
			     setVAttr (VColor "#ffff00") (-1)
		     else return False

	     CGItems atom -> 
		do changeVStyle itemstyle id 
		   setVAttr (VLabel$ fromAtom atom) id 

	     CGTags atom -> 
		do changeVStyle tagstyle id 
		   setVAttr (VLabel$ fromAtom atom) id 

   forM_ contexts $ \ (prev, id, label, _) -> 
      forM_ prev $ \ (_,p) -> 
	case label of 
	  CGSteps a | fromAtom a == "env" -> 
	       do edge <- newEdge (p,-1)
		  changeEStyle consumestyle edge
	  _ ->
	       do edge <- newEdge (p,id)
		  (flip changeEStyle edge) 
		    (case fromJust$ lab gr p of 
		      (CGSteps _)       -> producestyle
		      (CGItems _)       -> consumestyle
		      (CGTags  _)       -> prescribestyle)




simple_graphviz :: (nd1 -> String) -> G.Gr nd1 edge -> IO RunResult
simple_graphviz lablNode gr = 
--  runGraphvizCanvas Dot dot Gtk
  runGraphvizCanvas Dot dot Xlib
 where 
  dot = graphToDot params gr
  --params ::  GraphvizParams String Int () String
  --params ::  GraphvizParams String unknown () String
  --params ::  GraphvizParams nd1 edge () nd1
  --params = defaultParams { fmtNode= nodeAttrs }
  params = nonClusteredParams { fmtNode= nodeAttrs }
  nodeAttrs (node, x) =
    [ Label $ StrLabel $ lablNode x
    , Shape Circle
  --  , Color [colors !! a]
  --  , FillColor $ colors !! a
    , Style [SItem Filled []]
    ]



