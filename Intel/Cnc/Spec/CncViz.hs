{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}

----------------------------------------------------------------------------------------------------
-- This module includes visualization code which has numerous extra
-- dependencies compared to the rest of the spec tool's code.
----------------------------------------------------------------------------------------------------
module Intel.Cnc.Spec.CncViz where

import Intel.Cnc.Spec.TraceVacuum
import Intel.Cnc.Spec.CncGraph
import Intel.Cnc.Spec.Curses
import Intel.Cnc.Spec.Util
import qualified Intel.Cnc.Spec.Passes.ReadHarch as H

import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.Query.DFS
import Data.Maybe
import Data.Char
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import qualified Data.List as L

import qualified StringTable.AtomSet as AS
import qualified StringTable.AtomMap as AM
import StringTable.Atom 

import Control.Monad
import qualified Control.Monad.Reader as R

import Graphics.Ubigraph as Ub
import qualified Data.GraphViz as Gv

import System.Posix.Unistd
import System.Posix.Env

-- There are various options for trying to improve the stringmaps used in this program:
--
--import qualified Data.Map.StringMap as SM -- from TernaryTrees
--import qualified Data.Map.TernaryMap as TM -- from TernaryTrees

--import qualified Data.ListTrie.Patricia.Map as PM

import qualified Data.Map as SM -- Data.Map serves as StringMap for now.

import Debug.Trace

default_server_url = "http://127.0.0.1:20738/RPC2"

----------------------------------------------------------------------------------------------------
-- Graph Visualization

-- Draw a cnc spec through graphviz:
cncGraphviz = 
  -- This is easy because the graphviz wrapper uses fgl.
  error "TODO: IMPLEMENT ME"


--------------------------------------------------------------------------------
-- Display a cncgraph through ubigraph:


cncUbigraph :: Bool -> CncGraph -> IO ()
cncUbigraph interactive gr = 
  do server_url <- getEnvDefault "UBIGRAPH_SERVER" default_server_url
     putStrLn$ "DRAWING UBIGRAPH, total nodes "++ show (length sorted)
     initHubigraph server_url >>= runHubigraph go
  --r $ mkRing 10
 where 
  r x = initHubigraph default_server_url >>= runHubigraph x

  sorted = topsort gr
  contexts = map (G.context gr) sorted

  go = do 
   clear
   stepstyle <- newVStyle 0
   itemstyle <- newVStyle 0
   tagstyle  <- newVStyle 0
   mapM_ (flip setVStyleAttr stepstyle) (defaultStepAttr)
   mapM_ (flip setVStyleAttr itemstyle) (defaultItemAttr)
   mapM_ (flip setVStyleAttr tagstyle)  (defaultTagAttr)

   --let eshared = [EOriented True, ESpline True, EStrength 1.0]
   --let eshared = [EOriented True, ESpline True, EStrength 0.001]
   let eshared = [EOriented True]
   -- EArrow True

   baseEstyle  <- newEStyle 0
   mapM_ (flip setEStyleAttr baseEstyle) eshared

   producestyle   <- newEStyle baseEstyle
   consumestyle   <- newEStyle baseEstyle
   prescribestyle <- newEStyle baseEstyle

   mapM_ (flip setEStyleAttr producestyle)$   [EColor "#ff4444", EWidth 2.5] 
   mapM_ (flip setEStyleAttr consumestyle)$   [EColor "#44ff44", EWidth 2.5] 
   mapM_ (flip setEStyleAttr prescribestyle)$ [EColor "#666666"] ++ eshared

   -- Currently doing a two-phased add, but I don't actually like this:
   -- (I want to see it appear with the dataflow.)

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


-- FIXME!!! DOUBLE CHECK.. do GET edges flow from the ITEM to the STEP? (re: gravity)
   forM_ contexts $ \ (prev, id, label, _) -> 
      forM_ prev $ \ (_,p) -> 
	case label of 
	  CGSteps a | fromAtom a == "env" -> 
	       do edge <- newEdge (p,-1)
		  changeEStyle consumestyle edge
	  _ ->
	       do edge <- newEdge (p,id)
		  (flip changeEStyle edge) 
		    (case fromJust$ G.lab gr p of 
		      (CGSteps _)       -> producestyle
		      (CGItems _)       -> consumestyle
		      (CGTags  _)       -> prescribestyle)

   -- ABSTRACTION VIOLATION? Is the hubigraph monad supposed to be opaque?
   -- In interactive mode we bring up a prompt... should do this with ncurses:
   R.lift$ putStrLn "Going into interactive CnC/Ubigraph visualization shell:"
   

-- | Visualize any FGL graph in a window using GraphViz.
simple_graphviz :: (nd1 -> String) -> G.Gr nd1 edge -> IO Gv.RunResult
simple_graphviz lablNode gr = 
--  runGraphvizCanvas Dot dot Gtk
  Gv.runGraphvizCanvas Gv.Dot dot Gv.Xlib
 where 
  dot = Gv.graphToDot params gr
  --params ::  GraphvizParams String Int () String
  --params ::  GraphvizParams String unknown () String
  --params ::  GraphvizParams nd1 edge () nd1
  --params = defaultParams { fmtNode= nodeAttrs }
  params = Gv.nonClusteredParams { Gv.fmtNode= nodeAttrs }
  nodeAttrs (node, x) =
    [ Gv.Label $ Gv.StrLabel $ lablNode x
    , Gv.Shape Gv.Circle
  --  , Color [colors !! a]
  --  , FillColor $ colors !! a
    , Gv.Style [Gv.SItem Gv.Filled []]
    ]

-- | Using GraphViz, display a CnC graph with Harch partitioning info.
harch_graphviz ::  (H.HarchNode -> String) -> H.HarchSpec -> IO Gv.RunResult
harch_graphviz lablNode (H.HarchSpec gr tree) = 
     Gv.runGraphvizCanvas Gv.Dot (Gv.graphToDot params gr) Gv.Xlib
 where 
  params :: Gv.GraphvizParams H.HarchNode () [Int] H.HarchNode
  params = Gv.defaultParams 
	   { Gv.fmtNode= nodeAttrs
	   , Gv.clusterBy = lookup_clusters
	   , Gv.clusterID = \ ls -> Just$ Gv.Str (show ls)
	   , Gv.fmtCluster = clusterAttrs
	   }

  -- We must take care here... there are two different numbering
  -- schemes.  The FGL graph has a node ID, an the HarchNode has the
  -- number from the original harch file.  It would be nice to
  -- guarantee these are the same, or to make them disjoint so that
  -- confusion is impossible.
  lookup_clusters :: (G.LNode H.HarchNode) -> Gv.LNodeCluster [Int] H.HarchNode
  lookup_clusters (ind, nd) = 
      case IM.lookup (H.num nd) all_clusters of
	Nothing -> error$ "harch_graphviz: node that did not appear in the harch tree: "++ show nd
	Just set -> 
	  -- Convert the set using the C/N constructors:
	  S.fold (Gv.C) (Gv.N (ind,nd)) set

  all_clusters = walk_tree [] tree

  -- Walk over the tree to build a map from nodes -> partitions.
  -- Partitions are named by the tree-index.
  walk_tree :: [Int] -> H.HarchTreeOrdered -> IM.IntMap (S.Set [Int])
  walk_tree ind (H.HT part children) = 
    let 
	chldmaps = L.zipWith (\ i -> walk_tree (ind++[i]) ) 
	           [0..] children
	combined = L.foldl' (IM.unionWith S.union) IM.empty chldmaps

	insert acc node = IM.insertWith S.union node (S.singleton ind) acc
    in L.foldl' insert combined part

  clusterAttrs intls = 
     [ Gv.GraphAttrs [Gv.Label$ Gv.StrLabel$ H.showTreePath intls ] ]

  nodeAttrs (node, x) =
    [ Gv.Label $ Gv.StrLabel $ lablNode x
    , Gv.Shape Gv.Circle
  --  , Color [colors !! a]
  --  , FillColor $ colors !! a
    , Gv.Style [Gv.SItem Gv.Filled []]
    ]



----------------------------------------------------------------------------------------------------
-- Rewind/fast-forward support

-- What's the best way to create a reversible transaction log?
-- Should I create Data.Sequence of some kind of actions and interpret it?


data GUIAction =
    ChangeV NameTag [VAttr]
  | ChangeE Atom Atom EAttr
  | AddV NameTag
  | AddE NameTag NameTag
  | WaitAction -- We don't sleep between ALL actions, let's make them explicit.
 deriving Show

-- This is a little more involved than I would like, but we may need
-- to track the whole state of the drawing at each point in time.
--
-- The GUI state also lets us know what's in the graph at any given point.
data GUIState = GS {
   nodes :: AM.AtomMap GUINodeState, 
   edges :: AM.AtomMap (AM.AtomMap GUIEdgeState),
   -- Map steps to the tag collections that prescribe them.
   prescribedBy :: AM.AtomMap Atom
 }


-- The V/EAttr types are sum types that we wish to convert to a product type here...
{-
data GUINodeState = GNS {
    color :: String, 
    label :: String,
    shape :: Ub.Shape,
    size :: Float
    -- ...
  }
-}
-- We could get into SYB generic programming here, but easier is to
-- use the toPair function provided by the ubigraph library.
--type GUINodeState = SM.StringMap (String)
type StringMap a = M.Map String a 
-- The following StringMaps map the name ("size", "label") of each attribute onto its value.
type GUIEdgeState = StringMap EAttr
-- I also add a "count" to nodes to track how many instances they contain.
data GUINodeState = GNS { count :: Int, props :: StringMap VAttr }
  deriving Show

--type LogEvent = (GUIAction, GUIState)

instance Show VAttr where 
--  show vat = show (toPair vat)
  show vat = let (hd:tl,b) = toPair vat in "V"++ (toUpper hd : tl) ++" "++ b 

instance Show EAttr where 
  show eat = show (toPair eat)

--------------------------------------------------------------------------------

-- foo :: SM.StringMap Int
-- foo = SM.fromList [("foo",3), ("bar", 4)]
-- bar = TM.lookup "foo" foo
scale = 3.0
defaultStepMap = SM.fromList [("color", VColor "#3333ff"), ("shape", VShape Sphere), 
			       ("size", VSize (1.0 * scale)), ("shapedetail", VShapedetail 10), ("visible", VVisible True)]
defaultItemMap = SM.fromList [("color", VColor "#008800"), ("shape", VShape Cube), ("size", VSize (0.75 * scale)), ("visible", VVisible True)]
defaultTagMap  = SM.fromList [("color", VColor "#555555"), ("shape", VShape Octahedron), ("size", VSize (0.4 * scale)), ("visible", VVisible True)]

-- For convenience, here are the default attributes as lists:
defaultStepAttr = map snd$ SM.toList defaultStepMap
defaultItemAttr = map snd$ SM.toList defaultItemMap
defaultTagAttr  = map snd$ SM.toList defaultTagMap

-- The environment appears as a tweaked step:
defaultEnvAttr = map snd $ SM.toList $ 
  SM.insert "color" (VColor "#ffff00") $ 
  SM.insert "label" (VLabel "env IN") $ 
  defaultStepMap



named def nm = SM.insert "label" vlab $
	       -- This is a property of my own that I add for future reference:
	       --SM.insert "origname" vlab $ 
	       def
  where vlab = VLabel$ fromAtom nm

namedStep = named defaultStepMap


emptyGUIState = GS AM.empty AM.empty AM.empty

--pump_size = False
pump_size = True

--------------------------------------------------------------------------------
-- Convert a parsed trace into a series of GUI actions:
--  Two distinct behaviors.
--
--  Drawing collections ():
--    Steps are indexed with an empty ("") tag.  Each additional
--    instance added to a collection may change its appearance but
--    will not add a new node.
--    
--  Drawing instances (full_dynamic_graph):

--    Draw the "dynamic graph" of step instances.  Instances are
--    identified by a pair of their collection name and a string
--    representing a tag value.


traceToGUI :: [CncTraceEvent] -> [GUIAction]
traceToGUI trace =
      AddV envpr : ChangeV envpr defaultEnvAttr :
      loop emptyGUIState trace
 where 
  envpr = (toAtom "env", "")
  loop _ [] = []
  loop state0@GS{..} (hd:tl) = 
    let -- When drawing step collections we may "pump them up" as we get more instances:
        -- (This function also continues the loop, so it's called as a continuation.)
        pump_up_instance (nm,tag) gns@GNS{..} = 
            if full_dynamic_graph
	    then keep_going
	    else ChangeV (nm,tag) [VLabel$ oldlab ++" #"++ show (count+1), newsize] : keep_going
          where 
	     keep_going = loop state0{ nodes= newnodes } tl 
	     newnodes = AM.insert nm gns{count=count+1, props=props'} nodes

	     VLabel oldlab = props SM.! "label"  
	     -- Experimenting with growing the size too:
	     VSize oldsize = props SM.! "size"
	     newsize = VSize$ oldsize + 0.1
	     props' = if pump_size then SM.insert "size" newsize props else props

        newstate nm attrs = state0{ nodes = AM.insert nm (GNS 1 attrs) nodes }
    in
    case hd of 
      Prescribe tags step -> 
	let state1 = state0{ prescribedBy= AM.insert step tags prescribedBy } in
	-- When drawing
	if full_dynamic_graph
	then loop state1 tl
	else AddV (step,"") : loop state1 tl

      ------------------------------------------------------------
      StartStep pr@(nm,tg) -> 
	 case AM.lookup nm prescribedBy of 
	  Nothing -> error$ "traceToGUI: no Prescribe relation corresponding to step "++show nm
	  Just tags -> 	    
	    -- Add an edge connecting the tag [collection] to the step [collection]:
	    let edge = AddE (tags,tg) pr
		vertedge = [AddV pr, edge, ChangeV pr defaultStepAttr, WaitAction] in
	    (if full_dynamic_graph then vertedge else []) ++
            case AM.lookup nm nodes of
   	      Nothing  -> loop (newstate nm$ namedStep nm) tl
	      Just gns -> pump_up_instance pr gns 

      ------------------------------------------------------------
      PutT (stepnm,stag) tpr@(tgnm,_) -> 
        let edge = AddE (stepnm,stag) tpr 
	    vertedge = [AddV tpr, edge, ChangeV tpr defaultTagAttr, WaitAction] in
        case AM.lookup tgnm nodes of 
	  Nothing  -> vertedge ++ loop (newstate tgnm$ named defaultTagMap tgnm) tl
	  Just gns -> (if full_dynamic_graph then vertedge else [])
		      ++ pump_up_instance tpr gns
			 
      ------------------------------------------------------------
      PutI (stepnm,stag) ipr@(inm,_) -> 
        let edge = AddE (stepnm,stag) ipr 
	    vertedge = [AddV ipr, edge, ChangeV ipr defaultItemAttr, WaitAction] in
        case AM.lookup inm nodes of 
	  Nothing  -> vertedge ++ loop (newstate inm$ named defaultItemMap inm) tl
	  Just gns -> (if full_dynamic_graph then vertedge else [])
		      ++ pump_up_instance ipr gns

      ------------------------------------------------------------
      GetI (stepnm,stag) ipr@(inm,_) -> 
        let edge = AddE (stepnm,stag) ipr in
        case AM.lookup inm nodes of 
	  Nothing -> --AddV ipr : 
		     --ChangeV ipr defaultItemAttr :
	             edge : loop (newstate inm$ named defaultItemMap inm) tl
	  _ ->       edge : loop state0 tl


      _ -> loop state0 tl
   -- EndStep   NameTag 
   -- FAIL String




t29 = traceToGUI $ tracefile sample_trace

t30 = playback emptyGUIState t29 

----------------------------------------------------------------------------------------------------
-- Another way to do it would be to construct a reverse-log as we go,
-- for each attribute set, store a command which woud set it back to
-- the old attribute.
----------------------------------------------------------------------------------------------------

-- playback takes a forward and reverse sequence of actions.  To play
-- forward it reads from one tape, and reverse the other.  
-- It also must model the state of the GUI to be able to reverse actions.

--playback :: GUIState -> [GUIAction] -> [GUIAction] -> IO ()

playback :: GUIState -> [GUIAction] -> IO ()

-- Should we actually create a node for every dynamic instance?
full_dynamic_graph = False


playback state fwd = 
  do server_url <- getEnvDefault "UBIGRAPH_SERVER" default_server_url
     putStrLn$ cnctag++"Visualizing trace using ubigraph."
     initHubigraph server_url >>= runHubigraph initialize
 where 
  r x = initHubigraph default_server_url >>= runHubigraph x

  initialize = do 
   clear

   setVStyleAttr (VVisible False) 0
   setVStyleAttr (VColor "#ff0000") 0

   stepstyle <- newVStyle 0
   itemstyle <- newVStyle 0
   tagstyle  <- newVStyle 0

   mapM_ (flip setVStyleAttr stepstyle) (defaultStepAttr)
   mapM_ (flip setVStyleAttr itemstyle) (defaultItemAttr)
   mapM_ (flip setVStyleAttr tagstyle)  (defaultTagAttr)

   --let eshared = [EOriented True, ESpline True, EStrength 0.001]
   let eshared = [EOriented True, ESpline True, EStrength 0.0]
   --let eshared = [EOriented True]
   baseEstyle  <- newEStyle 0
   mapM_ (flip setEStyleAttr baseEstyle) eshared

   producestyle   <- newEStyle baseEstyle
   consumestyle   <- newEStyle baseEstyle
   prescribestyle <- newEStyle baseEstyle

   mapM_ (flip setEStyleAttr producestyle)$   [EColor "#ff4444", EWidth 2.5] 
   mapM_ (flip setEStyleAttr consumestyle)$   [EColor "#44ff44", EWidth 2.5] 
   mapM_ (flip setEStyleAttr prescribestyle)$ [EColor "#666666"] ++ eshared

   --------------------------------------------------------------------------------
   -- Main loop
   --------------------------------------------------------------------------------
   let step_forward idmap fwd = 
	case fwd of 
	 [] -> do R.lift$ putStrLn "playback finished: no more actions!"
		  return idmap
	 hd:tl -> 
                case hd of 
  	         AddV pr@(atom, tag) -> 
		  do id <- newVertex
 		     setVAttr (VLabel$ fromAtom atom ++" "++ tag) id 
		     return$  M.insert pr id idmap

  	         ChangeV pr updates -> 
	 	  do let id = idmap M.! pr
	 	     mapM_ (flip setVAttr id) updates
		     return idmap


  	         AddE from to -> 
		  do --R.lift$ putStrLn$ "ADDING EDGE "++ show from ++" "++ show to
		     let from' = M.lookup from idmap 
			 to'   = M.lookup to idmap
		     case (M.lookup from idmap, M.lookup to idmap) of 
		       (Just from', Just to') -> do 
			  id <- newEdge (from', to')
			  changeEStyle producestyle id 
			  return idmap
		       (Nothing,_) -> do R.lift$ putStrLn$ ("Warning: Missing source of AddE edge! "++show from)
		                         return idmap
		       (_,Nothing) -> do R.lift$ putStrLn$ ("Warning: Missing destination of AddE edge! "++show to)
		                         return idmap

  	         WaitAction -> do R.lift$ usleep (300 * 1000) -- 0.1 second sleep.
				  return idmap
		 _ -> return idmap
		 --x -> error$ "playback: unhandled GUIAction: "++ show x

    -- For now just play forward, maximum speed:
   let loop idmap state rvrs fwd = 
         do newidmap <- step_forward idmap fwd 
	    --R.lift$ usleep (100 * 1000)
	    --Control.Concurrent.threadDelay
	    loop newidmap
		 (error "no state atm") --(updateState state hd) 
  		 (error "no rev action") --(buildRevAction state hd : rvrs) 
		 (tail fwd)
   loop (M.fromList [((toAtom "env",""), envID)]) state [] fwd

-- This simply needs to not conflict with the auto-assigned Ubigraph ids:
envID = 1


deJust msg Nothing = error msg
deJust _ (Just x) = x

updateState = error "updateState"
buildRevAction = error "buildRevAction"

  -- It also must model the state 

--  loop state 

