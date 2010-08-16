{-# LANGUAGE RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}

----------------------------------------------------------------------------------------------------
-- This module includes visualization code which depends on various libraries.
----------------------------------------------------------------------------------------------------
module Intel.Cnc.Spec.CncViz where

import Intel.Cnc.Spec.TraceVacuum
import Intel.Cnc.Spec.CncGraph
import Intel.Cnc.Spec.Curses

import qualified Data.Graph.Inductive as G
import Data.Graph.Inductive.Query.DFS
import Data.Maybe
import Data.Char
import qualified Data.Map as M

import qualified StringTable.AtomSet as AS
import qualified StringTable.AtomMap as AM
import StringTable.Atom 

import Control.Monad
import qualified Control.Monad.Reader as R

import Graphics.Ubigraph as Ub
import qualified Data.GraphViz as Gv

import System.Posix.Unistd

-- There are various options for trying to improve the stringmaps used in this program:
--
--import qualified Data.Map.StringMap as SM -- from TernaryTrees
--import qualified Data.Map.TernaryMap as TM -- from TernaryTrees

--import qualified Data.ListTrie.Patricia.Map as PM

import qualified Data.Map as SM -- Data.Map serves as StringMap for now.

import Debug.Trace

----------------------------------------------------------------------------------------------------
-- Graph Visualization

-- Draw a cnc spec through graphviz:
cncGraphviz = 
  -- This is easy because the graphviz wrapper uses fgl.
  undefined


--------------------------------------------------------------------------------
-- Display a cncgraph through ubigraph:

cncUbigraph :: Bool -> CncGraph -> IO ()
cncUbigraph interactive gr = 
  do putStrLn$ "DRAWING UBIGRAPH, total nodes "++ show (length sorted)
     initHubigraph server_url >>= runHubigraph go
  --r $ mkRing 10
 where 
  r x = initHubigraph server_url >>= runHubigraph x

  server_url = "http://127.0.0.1:20738/RPC2"
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

-- First, convert a parsed trace into a series of GUI actions:
traceToGUI :: [CncTraceEvent] -> [GUIAction]
traceToGUI trace =
      AddV envpr : ChangeV envpr defaultEnvAttr :
      loop emptyGUIState trace
 where 
  envpr = (toAtom "env", "")
  loop _ [] = []
  loop state0@GS{..} (hd:tl) = 
    let pump_up_instance (nm,tag) gns@GNS{..} = 
	 let VLabel oldlab = props SM.! "label"  
	     -- Experimenting with growing the size too:
	     VSize oldsize = props SM.! "size"
	     newsize = VSize$ oldsize + 0.1
	     props' = if pump_size then SM.insert "size" newsize props else props

	     newnodes = AM.insert nm gns{count=count+1, props=props'} nodes
         in ChangeV (nm,tag) [VLabel$ oldlab ++" #"++ show (count+1), newsize]
	    : loop state0{ nodes= newnodes } tl 
 
        newstate nm attrs = state0{ nodes = AM.insert nm (GNS 1 attrs) nodes }
    in
    case hd of 
      Prescribe tags step -> 
	let state1 = state0{ prescribedBy= AM.insert step tags prescribedBy } in
	if full_dynamic_graph
	then loop state1 tl
	else AddV (step,"") : loop state1 tl

      ------------------------------------------------------------
      StartStep pr@(nm,tg) -> 
	 case AM.lookup nm prescribedBy of 
	  Nothing -> error$ "traceToGUI: no Prescribe relation corresponding to step "++show nm
	  Just tags -> 	    
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
full_dynamic_graph = True


playback state fwd = 
  do putStrLn$ "[cnc] Vizualizing trace using ubigraph."
     initHubigraph server_url >>= runHubigraph initialize
 where 
  r x = initHubigraph server_url >>= runHubigraph x
  server_url = "http://127.0.0.1:20738/RPC2"

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
		     let from' = deJust ("Missing source of AddE edge! "++show from)    $ M.lookup from idmap 
			 to'   = deJust ("Missing destination of AddE edge! "++show to) $ M.lookup to idmap
		     id <- newEdge (from', to')
		     changeEStyle producestyle id 
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

