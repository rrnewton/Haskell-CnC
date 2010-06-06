{-# LANGUAGE ExistentialQuantification
   , ScopedTypeVariables
   , BangPatterns
   , NamedFieldPuns, RecordWildCards
  #-}
{-# OPTIONS_HADDOCK prune #-}
{-
 - Intel Concurrent Collections for Haskell
 - Copyright (c) 2010, Intel Corporation.
 -
 - This program is free software; you can redistribute it and/or modify it
 - under the terms and conditions of the GNU Lesser General Public License,
 - version 2.1, as published by the Free Software Foundation.
 -
 - This program is distributed in the hope it will be useful, but WITHOUT
 - ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 - FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 - more details.
 -
 - You should have received a copy of the GNU Lesser General Public License along with
 - this program; if not, write to the Free Software Foundation, Inc., 
 - 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
 -
 -}

-- |This module is an alternative implementation exposing the same inteface as "Intel.Cnc".
#ifndef INCLUDEMETHOD
module Intel.CncPure(
		  Step, TagCol, ItemCol,
		  StepCode(..), GraphCode,
		  newItemCol, newTagCol, prescribe, 
		  putt, put, get,
		  initialize, finalize,

                  runGraph, 
		  stepPutStr, cncPutStr, cncVariant,

                  tests, 
		 )
    where
#endif

import Data.Array as Array

import Data.List as List
import Data.Set as Set
import Data.Map as Map
import Data.Maybe
import Data.IORef
import qualified Data.IntMap as IntMap
import Data.Word
import Data.Complex

import Control.Concurrent
import GHC.Conc
import Control.Monad
--import System
import Debug.Trace
import Unsafe.Coerce

import Intel.CncUtil hiding (tests)

import System.IO.Unsafe
import System.Random

import Test.HUnit

-- README
------------------------------------------------------------
-- How to do a *PURE* CnC?

-- Well, this is a bit tricky because the a cnc step is a function
-- from a heterogeneous set of collections to a set of new tags and
-- items.  We could use various methods:

-- (1) We could require that the user construct a sum-type including
--     all the item types that will occur in their program.  (And
--     another for the tag types.)

-- (2) We could use existential types to pack various sorts of output
--     items and tags into one list.  Together with an unsafe cast we 
--     could build a safe interface into heterogeneous collections.

-- This file implements (2).  This is fairly inefficent because our
-- primary representation is a Map of Maps.  We have the overhead of
-- that double indirection times the cost of the pure data structures.

-- Below you will see two interfaces, the "raw" functional interface
-- (functions prefixed with "_") and a nicer monadic interface.

------------------------------------------------------------
-- Toggles

-- Should we remember which tags have been invoked?
#ifdef MEMOIZE
#warning "Memoization enabled"
memoize = True
#else
memoize = False
#endif


#ifndef CNC_SCHEDULER
#warning  "CncPure.hs -- CNC_SCHEDULER unset, defaulting to scheduler 2 "
#define CNC_SCHEDULER 2
#endif


#if CNC_SCHEDULER == 1
scheduler = simpleScheduler
#elif CNC_SCHEDULER == 2
scheduler = betterBlockingScheduler
#elif CNC_SCHEDULER == 3
#warning "Enabling parallel scheduler..."
scheduler = parallelScheduler

#elif CNC_SCHEDULER == 4
scheduler = parSched2

-- #elif CNC_SCHEDULER == 5
--scheduler = distScheduler 

#else
#error "CncPure.hs -- CNC_SCHEDULER is not set to one of {1,2,3}"
#endif

cncVariant = "pure/" ++ show CNC_SCHEDULER


{- 
Notes on Schedulers:

[2009.08.12] {Initial testing of betterBlockingScheduler}
Ok, for the sched_tree.hs test, enabling betterBlockingScheduler slows
it down from 1.19 user (200,000 limit) to 1.26 user.  And that's with
no blocking!  Just the extra cost of checking to see if there are
blocked steps hanging off of new items.

  primes 100K - makes no difference (heavyweight steps)
  mandel 100 100 1000 - makes no difference (heavyweight)
  threadring_onestepcollection 1M - 3.67 vs 3.6

-}

------------------------------------------------------------
-- Type definitions:

-- The central abstraction is a (heterogeneous) group of item and tag collections.
type Collections = (Int, MatchedTagMap, MatchedItemMap)

data MatchedItemMap = forall a b. MI !(IntMap.IntMap (ItemColInternal a b))
data MatchedTagMap  = forall a.   MT !(IntMap.IntMap (TagColInternal a))

-- We pass around HANDLES to the real item/tag collections, called "ID"s:
data TagCol  a   = TCID Int deriving (Ord, Eq, Show)
data ItemCol a b = ICID Int deriving (Ord, Eq, Show)
type TagColInternal    a   = Set a
type ItemColInternal   a b = Map a b

-- A step either produces a batch of new data to write, or blocks:
type Step a = a -> Collections -> StepResult
data StepResult = Done [NewTag] [NewItem]
                | forall a b. (Ord a, Show a) => Block (ItemCol a b) a
data NewTag  = forall a.   Ord a => NT (TagCol  a)   a
data NewItem = forall a b. (Ord a,Show a) => NI (ItemCol a b) a b

-- Need it to be a map... but this type is not truly polymorphic enough:
data Graph = forall a. G (IntMap.IntMap [Step a])

------------------------------------------------------------
-- (Optional) type signatures for operations:

-- The raw functional interface:
_newWorld   :: Int -> Collections
_newTagCol  :: Collections -> (TagCol ma, Collections)
_newItemCol :: Collections -> (ItemCol a b, Collections)

-- These are called by the step code and produce outbound tags and items:
_put  :: (Show a,Ord a) => ItemCol a b -> a -> b -> NewItem
_putt :: Ord a => TagCol  a   -> a      -> NewTag
_get  :: Ord a => Collections -> ItemCol a b -> a -> Maybe b

_prescribe :: Ord a => TagCol a -> Step a -> Graph -> Graph

--------------------------------------------------------------------------------
-- Implementation:

-- A "world" is a (heterogeneous) set of collections.
-- A world keeps a counter that is used to uniquely name each collection in that world:
_newWorld n = (n, MT IntMap.empty, MI IntMap.empty)
_newTagCol (cnt, MT tags, items) = 
    (TCID cnt, (cnt+1, MT newtags, items))
  where newtags = IntMap.insert cnt Set.empty tags 

_newItemCol (cnt, tags, MI items) =
    (ICID cnt, (cnt+1, tags, MI newitems))
  where newitems = IntMap.insert cnt Map.empty items

magic :: ItemCol a b -> ItemColInternal c d -> ItemColInternal a b
magic id = (unsafeCoerce)

_get (_, _, MI imap) id tag = 
  let ICID n = id 
      badcol  = (IntMap.!) imap n
      goodcol = magic id badcol
   in
   case Map.lookup tag goodcol of
    Nothing -> Nothing
    Just d  -> Just d

-- INTERNAL USE ONLY: Remove an item from an item collection.
_rem  :: Ord a => Collections -> ItemCol a b -> a -> Collections
_rem (cnt,tmap,MI imap) id tag =	
  let ICID n = id in
   (cnt, tmap,
    MI$ IntMap.adjust 
         (\col -> moremagic imap $ Map.delete tag (magic id col))
	 n imap)
--     MI$ IntMap.insert 
-- 	 n (moremagic imap $ Map.delete tag goodcol)
-- 	 imap)

--data MatchedItemMap = forall a b. MI (IntMap.IntMap (ItemColInternal a b))

_put id tag item = NI id tag item -- Just accumulate puts as data
_putt id tag     = NT id tag 

moremagic :: IntMap.IntMap (ItemColInternal a b) -> ItemColInternal c d -> ItemColInternal a b
moremagic id = (unsafeCoerce)

tmagic :: TagCol a -> TagColInternal c -> TagColInternal a
tmagic id = (unsafeCoerce)

mostmagic :: IntMap.IntMap (TagColInternal a) -> TagColInternal c -> TagColInternal a
mostmagic id = (unsafeCoerce)


-- This inserts new items and tags into a Collections object.
-- It also returns a list containing the tags that were actually new.
--
-- This is inefficient in that it looks up the tagCol/itemCol ID for
-- each update.  Ideally, steps would produce a more organized
-- "chunked" structure so that we could
--
-- Also, we could optimize this here by optimistically assuming that a
-- batch of updates are likely to the same collection.
--mergeUpdates :: IORef Collections -> [NewTag] -> [NewItem] -> IO ()
mergeUpdates :: [NewTag] -> [NewItem] -> Collections -> (Collections, [NewTag])
mergeUpdates newtags newitems (n, MT tags, MI items) =
       -- SHOULD WE USE a strict foldl' ???
       let items' = foldl (\ acc (NI id k x) -> 
  			    let ICID n = id 
			        badcol = (IntMap.!) acc n
			        goodcol = magic id badcol
 			        newcol = moremagic acc $ Map.insert k x goodcol
			    in
  			    IntMap.insert n newcol acc)
 	             items newitems in
       -- This also keeps track of what tags are new.
       let (tags',fresh) = 
	       foldl (\ (acc,fresh) nt -> 
		      case nt of 
		       NT id k ->	
  		        let 
		          TCID n = id 
		          badcol = (IntMap.!) acc n
		          goodcol = tmagic id badcol
 		          newcol = mostmagic acc $ Set.insert k goodcol
		          notnew = Set.member k goodcol
		        in
  	       	         (IntMap.insert n newcol acc, 
		          if notnew then fresh else nt:fresh))
	         (tags,[]) newtags in
       if memoize
       then ((n, MT tags', MI items'), fresh)
       else ((n, MT tags, MI items'), newtags)

megamagic :: TagCol a -> IntMap.IntMap [Step b] -> IntMap.IntMap [Step a]
megamagic id col = (unsafeCoerce col)


emptyGraph = G IntMap.empty
_prescribe id step (G gmap) = 
    case id of 
     TCID n ->
       G (IntMap.insertWith (++) n [step] $ megamagic id gmap)

-- Retrieve the steps from a graph:
getSteps  :: Graph -> TagCol a -> [Step a]
getSteps (G gmap) id = 
    case id of 
     TCID n -> IntMap.findWithDefault [] n (megamagic id gmap)


-- A "primed" step is one that already has its tag and just needs a Collections:
type PrimedStep = Collections -> StepResult

-- Looks up all the steps associated with a tag and returns a list of
-- ready-to-execute steps, just waiting for a Collections argument.
callSteps  :: Graph -> TagCol a -> a -> [PrimedStep]
callSteps (G gmap) id tag = 
    case id of 
     TCID n -> Prelude.map (\fn -> fn tag) $ 
	       IntMap.findWithDefault [] n (megamagic id gmap)

--------------------------------------------------------------------------------
-- A simple scheduler. 
-- This runs blocked steps naively and only when it runs out of other steps.
-- WARNING: this is not a CORRECT scheduler -- it can loop indefinitely 
simpleScheduler :: Graph -> [NewTag] -> Collections -> Collections
simpleScheduler graph inittags cols = schedloop cols [] inittags []
 where -- The scheduler loop takes four arguments:
       --  (1) The world (all collections).
       --  (2) Blocked steps.
       --  (3) New tags to process.
       --  (4) Steps ready to execute.
       schedloop c [] [] []  = c
       schedloop c blocked [] [] = schedloop c [] [] blocked

       schedloop c blocked (hd : tl) [] = 
	   case hd of 
	    NT id tag ->
	     schedloop c blocked tl (callSteps graph id tag)

       schedloop c blocked tags (step : tl) = 
	   case step c of
	     Block d_id tag -> schedloop c (step:blocked) tags tl
	     Done newtags newitems -> 
		 let (c2,fresh) = mergeUpdates newtags newitems c
		 in schedloop c2 blocked (fresh++tags) tl

-- Bring an ID into the alternate reality (which stores blocked steps)
magic_to_alternate :: ItemCol a b -> ItemCol a [PrimedStep]
magic_to_alternate id = unsafeCoerce id


--------------------------------------------------------------------------------
-- We reuse the typing magic of the existing collections mechanism for
-- creating a collection of blocked steps.
betterBlockingScheduler :: Graph -> [NewTag] -> Collections -> Collections
betterBlockingScheduler graph inittags world = schedloop world alternate' inittags []
 where 
       alternate' = mirrorWorld world 

       schedloop :: Collections -> Collections -> [NewTag] -> [PrimedStep] -> Collections
       schedloop w alternate [] [] = w

       schedloop w alternate (hd : tl) [] = 
	   case hd of 
	    NT id tag ->
	     schedloop w alternate tl (callSteps graph id tag)

       schedloop w alternate tags (pstep:tl) = 
	   --trace (case id of TCID n -> "      *** Executing tagcol "++ show n ++" tag: "++ show (char tag)) $ 
	   case pstep w of
	     Block (d_id) tag -> 
#ifdef VERBOSEBLOCKING
		 trace (" ... Blocked ... " ++ show (d_id,tag)) $ 
#endif
		 let alternate' = updateMirror alternate d_id tag pstep 
	         in schedloop w alternate' tags tl

	     Done newtags newitems -> 
		 let (w2,fresh) = mergeUpdates newtags newitems w
		      -- Check to see if the new items have activated any blocked actions:
		     (steps',alternate') = 
			 foldl (\ (acc,alternate) (NI (id) tag _) -> 
				     let (alternate',steps) = mirrorGet alternate id tag in 
				       --trace (" ... REACTIVATED ... " ++ show (id,tag)) $
				       (steps++acc, alternate')
				)
			    (tl,alternate) newitems
		 in schedloop w2 alternate' (fresh++tags) steps'


--------------------------------------------------------------------------------

-- Parallel scheduler: 

-- The basic idea here is that there's a single copy of the world
-- state.  Each worker thread computes some number of steps and lazily
-- tries to push the updates to the global copy.  

-- We have a choice as to the data structure for the global world
-- state -- IORef or MVar.

-- The complication in this approach is blocking steps.  By the time a
-- thread commits its output, a step may have blocked on data that has
-- since become available.

-- Unless we use a trickier data structure (some kind of sliding
-- window, storing updates at each "revision number" and processing
-- only the item updates since we last snapshotted the global state)
-- then we need to recheck all new blocked items against the new
-- global state.  I don't think we can really amortize this cost by
-- committing less often, because we then have proportinally more
-- blocked items to commit -- each new blocked needs to be looked up
-- against the global state.

-- ==============================================================================
-- Interface for maintaining a mirror of the Collections, including
-- blocked steps rather than items.

-- Duplicate all the ICIDs used in the real world.
-- (We will expect all the entries in the IntMap to be defined.)
-- However, all that's important here is that we initialize the
-- alternate reality with the same NUMBER of item collections.
mirrorWorld :: Collections -> Collections
mirrorWorld world = 
    case world of 
     (_,_,MI imap) ->
       -- HACK: we actually need to make ADDITIONAL item
       -- collections to fill in the gaps where tag collections
       -- used up ID numbers.  Wouldn't be necessary if
       -- Collections stored two counters...
       foldl (\ w _ -> snd $ _newItemCol w)
	     (_newWorld 0) 
	     [0.. foldl max 0 (IntMap.keys imap)]

updateMirror :: (Show a, Ord a) => Collections -> ItemCol a b -> a -> PrimedStep -> Collections
updateMirror mirror d_id tag val = mirror'
  where
        alt_id = magic_to_alternate d_id 
	others = 
	    case _get mirror alt_id tag of
	      Nothing -> []
	      Just ls -> ls
	new = _put alt_id tag (val:others)
	(mirror',[]) = mergeUpdates [] [new] mirror

-- Similar but takes a list of Block entries and a list of primed steps:
updateMirrorList mirror bls sls = 
    case bls of 
     [] -> mirror
     Block d_id tag : tl -> 
       updateMirrorList (updateMirror mirror d_id tag (head sls))
			tl (tail sls)

-- Destructive get operation:
mirrorGet mirror id tag =
    let alt_id = magic_to_alternate id in
     case _get mirror alt_id tag of
      Nothing    -> (mirror, [])
      Just steps -> (_rem mirror alt_id tag, steps)

------------------------------------------------------------

-- Scan new items against the existing blocked
--   Complexity m * (log n) 
--   Would be better asymptotically to intersect maps.
--   (Well, we'd pay when we built up the little map... but that would
--    be, m * log m and presumably m << n.)
-- This function returns:
--  (1) the activated steps and 
--  (2) a new collection of blocked-entries with the activated steps removed.
newItemsAgainstBlocked :: [NewItem] -> Collections -> (Collections, [PrimedStep])
newItemsAgainstBlocked newitems mirror = 
	    foldl (\ (mirror,acc) (NI (id) tag _) -> 
		   let alt_id = magic_to_alternate id in
		     case _get mirror alt_id tag of
		       Nothing -> (mirror,acc)
		       --Just [] -> (acc,mirror)
		       Just steps -> 
		         -- Remove the blocked steps from the collection:
		         trace (" ... REACTIVATED ... " ++ show (alt_id,tag)) $
		         (_rem mirror alt_id tag, steps++acc)
		  )
	    (mirror,[]) newitems


-- ==============================================================================
-- Scheduler version 3: Now in parallel.

data Bundle a = 
    B { blocked :: [StepResult]
      , bsteps  :: [PrimedStep]
      , intags  :: a
      , outtags :: [NewTag]
      , items   :: [NewItem]}
    
_GRAIN = 5  ; _NUMTHREADS = numCapabilities
-- _GRAIN = 1  ; _NUMTHREADS = 1
--_GRAIN = 2 ;_NUMTHREADS = 2

-- _NUMTHREADS = numCapabilities

parallelScheduler :: Graph -> [NewTag] -> Collections -> Collections
parallelScheduler graph inittags world = 
    -- It's safe to be unsafe here!! (If you follow the CnC rules...)
    unsafePerformIO $ 
    do global_world   <- newIORef world
       global_blocked <- newIORef (mirrorWorld world)
       -- How do we split the initial tags up?
       -- Let's split them evenly for now, ignorant of any data locality principles.
       forkJoin $ Prelude.map (threadloop global_world global_blocked []) 
		$ splitN _NUMTHREADS inittags
       -- Finally, return the quiescent world:
       readIORef global_world
 where 
       threadloop worldref blockedref primed mytags = 
	 do 
	    -- Snapshot the world:
	    world <- readIORef worldref
	    -- If the world is stale, we might block unnecessarily.
	    let B {blocked, bsteps, intags, outtags, items} = 
		  runSomeSteps graph world _GRAIN 
		     (B {blocked=[], bsteps=[], intags=mytags, outtags=[], items=[]}) 
		     primed

            len <- return $ length bsteps
	    -- Now we atomically write back changes to the world:
	    fresh <- atomicModifyIORef worldref (mergeUpdates outtags items)

            -- Atomically read blocked and unblock steps as necessary,
            -- extending blocked and returning unblocked steps.
            newprimed <- atomicModifyIORef blockedref
			 (\ oldblck -> 
			  -- NEED TO INCORPORATE blckd' into blocked
			  let newb = updateMirrorList oldblck blocked bsteps in 
			  newItemsAgainstBlocked items newb)
	    if Prelude.null intags && Prelude.null fresh && Prelude.null newprimed
	     then return ()
	     else threadloop worldref blockedref newprimed (fresh ++ intags)

-- ==============================================================================

-- Here's another variation:

parSched2 :: Graph -> [NewTag] -> Collections -> Collections
parSched2 graph inittags world = 
    -- It's safe to be unsafe here!! (If you follow the CnC rules...)
    unsafePerformIO $ 
    do worldref   <- newIORef world
       blockedref <- newIORef (mirrorWorld world)

       -- For now work-queues are indeed queues... should be stacks.
       work_queues <- mapM (\_ -> newChan) [1..10]

       -- For fast indexing:
       let queue_arr = listArray (0,length work_queues-1) work_queues

       ------------------------------------------------------------
       let --perms = let p = permutations work_queues in listArray (0,length p - 1) p
	   workerthread primed (myid, chan, mytags) = 
	       do putStrLn $ "=== Starting thread "++ show (myid) ++" with "++ show (length mytags) ++" initial tags."
		  writeList2Chan chan mytags 
	          threadloop primed 
	    where 
              -- THIS COULD DEADLOCK!! WE NEED A NONBLOCKING GET!! 
	     -- FIXME: DON'T STEAL FROM OURSELVES!! 
	     trysteal 0 = putStr "Thread giving up and dying...\n"
	     trysteal n = 
	      do _i :: Int <- randomIO 
		 let i = _i `mod` _NUMTHREADS
		 if myid == i then return () else putStrLn $ " "++ show myid ++" Stealing from " ++ show i
		 let q = (Array.!) queue_arr i
		 b <- isEmptyChan q
		 if b then trysteal (n-1)
		      else do x <- readChan q
			      putStrLn "   <STOLEN>"
			      writeChan chan x
			      threadloop []

	     threadloop primed = 
	      do
	       -- Snapshot the world:
	       world <- readIORef worldref
	       -- If the world is stale, we might block unnecessarily.
	       B {blocked, bsteps, outtags, items} <- 
		    runSomeSteps2 graph world _GRAIN chan 
		     (B {blocked=[], bsteps=[], intags=(), outtags=[], items=[]}) 
		     primed

	       -- Now we atomically write back changes to the world:
	       fresh <- atomicModifyIORef worldref (mergeUpdates outtags items)

               -- Atomically read blocked and unblock steps as necessary,
               -- extending blocked and returning unblocked steps.
               newprimed <- atomicModifyIORef blockedref
			     (\ oldblck -> 
			      -- NEED TO INCORPORATE blckd' into blocked
			      let newb = updateMirrorList oldblck blocked bsteps in 
			      newItemsAgainstBlocked items newb)

               writeList2Chan chan fresh

               --putStrLn $ "PERMS OF " ++ show (length (work_queues))
               --putStrLn $ "PERMS " ++ show ((permutations work_queues !! 10000))

               -- Are we out of work?
	       if List.null newprimed
		then do b <- isEmptyChan chan
			if b then trysteal (_NUMTHREADS * 2)
			     else threadloop newprimed
                else threadloop newprimed 

       ------------------------------------------------------------
       -- How do we split the initial tags up?
       -- Let's split them evenly for now, ignorant of any data locality principles.
       forkJoin $ Prelude.map (workerthread []) 
		$ zip3 [0.. length work_queues] work_queues
		$ splitN _NUMTHREADS inittags
       -- Finally, return the quiescent world:
       readIORef worldref
 


runSomeSteps2 :: Graph -> Collections -> Int -> Chan NewTag -> Bundle () -> [PrimedStep] -> IO (Bundle ())
runSomeSteps2 g w n c (rec @ B{..}) primed = 
 case primed of 
  [] ->
    -- If we're over our limit, we stop even if there's work left.
    -- (But we make sure to finish the already primed steps.)
    if n <= 0 then return rec else
    -- If we run out of (readily available) work we have to stop:
    do b <- isEmptyChan c
       if b then return rec else
        -- In this case we're out of primed steps, but we have more tags.
        -- We prime a batch of new steps (corresponding to the next tag).
	do hd <- readChan c 
	   case hd of 
	    NT id tag ->
	     runSomeSteps2 g w n c rec (callSteps g id tag)

  -- In this case we have primed steps and just need to do the real work:
  pstep:tl ->
   case pstep w of
   -- Accumulate blocked tokens:
    newb@(Block _ _) -> 
       runSomeSteps2 g w (n-1) c 
        rec{blocked= newb:blocked, bsteps= pstep:bsteps} tl
    -- Alas, we don't know which of these newtags are really
    -- FRESH (seen for the first time) until we merge it back
    -- into the global world state.
    Done newtags newitems ->
       runSomeSteps2 g w (n-1) c
        rec{outtags=newtags++outtags, items=newitems++items} tl


-- ==============================================================================
-- Scheduler version 5: Local world copies.

-- UNFINISHED?

-- This version is an intermediate step towards a distributed version.
-- Each thread maintains its own picture of the world.

-- Communication is via a gossip protocol.
-- In this prototype version, every thread maintains a channel with
-- every other.  However, we have a great deal of leeway wrt the
-- communication organization here.  We could, for example, try to
-- coelesce updates in various ways... The trick will be versioning
-- the updates and suppressing duplicates.

-- Initial tags are split evenly.  Work stealing balances load
-- subsquently.  The trickiest part here is managing duplicated work.

--distScheduler :: Graph -> [NewTag] -> Collections -> Collections
distScheduler graph inittags world = 
    -- It's safe to be unsafe here!! (If you follow the CnC rules...)
    unsafePerformIO $ 
    -- Open up a comm channel for every pair of workers:

    do chans <- sequence 
		[ sequence [ do c <- newChan; return (i,j,c) 
			     | j <- [1.. _NUMTHREADS], not(i == j) ] 
		  | i <- [1.. _NUMTHREADS] ]
--     do chans <- sequence [ do c <- newChan; return (i,j,c) | 
-- 			   i <- [1.. _NUMTHREADS], 
-- 			   j <- [1.. _NUMTHREADS], 
-- 			   not(i == j) ]

       -- How do we split the initial tags up?
       -- Let's split them evenly for now, ignorant of any data locality principles.
       forkJoin $ Prelude.map 
		   (\ (ch,tags) -> 
		     let (my_i,_,_):_ = ch 
		         myinbound = List.filter (\ (_,j,_) -> j == my_i)
		                     $ concat chans
		         third (_,_,x) = x
		         thirds = List.map third
		     in	threadloop world (mirrorWorld world) 
		                   (thirds ch) (thirds myinbound) [] tags)
		$ zip chans
		--  zip (List.groupBy (\ (a,b,_) (x,_) -> a==x) chans)
		$ splitN _NUMTHREADS inittags
       -- Finally, return the quiescent world:
       --readIORef global_world
       return chans
 where 
       threadloop world bworld outchans inchans primed mytags = 
	 do 
            --worldref   <- newIORef world
	    --blockedref <- newIORef (mirrorWorld world)
	    --let mirror = mirrorWorld world

            -- Receive updates from other workers:
	    world2 <-
	     foldM (\ w c -> do b <- isEmptyChan c
	                        if b then return w
	                             else return w
		   )
	       world inchans

	    -- If the world is stale, we might block unnecessarily.
	    let B {blocked, bsteps, intags, outtags, items} = 
		  runSomeSteps graph world _GRAIN 
		     (B {blocked=[], bsteps=[], intags=mytags, outtags=[], items=[]})
		     primed
	        world2  = mergeUpdates outtags items 
		newb    = updateMirrorList bworld blocked bsteps 
	        bworld2 = newItemsAgainstBlocked items newb

            -- Send updates to other workers:
	    mapM_ (\_ -> return () ) outchans
{-

            -- Atomically read blocked and unblock steps as necessary,
            -- extending blocked and returning unblocked steps.


	    if Prelude.null intags && Prelude.null fresh && Prelude.null newprimed
	     then putStr "EMPTIED\n"
	     else threadloop worldref blockedref newprimed (fresh ++ intags)
-}
            return (error "CncPure distScheduler not complete yet")

--------------------------------------------------------------------------------
-- Run some steps, accumulate output, and then return to synchronize/schedule.
--------------------------------------------------------------------------------

-- The big question is how many actions we should run before we
-- stop and commit.  Perhaps we should determine some heuristic
-- that would serve here.  One example heuristic would be that
-- we could check realtime every time that we come back to
-- commit and dynamically adjust the number of actions so that
-- we don't come back to commit too often.

-- There are several additional choices in terms of how we
-- commit the output of a worker thread.  

-- First, if we batch up output items without committing them
-- to the local copy of the world, then subsequent steps we
-- perform within a thread (before committing) will not be able
-- to see those items and will block unnecessarily.  We can
-- live with this problem, but it will create trouble on
-- "depth-first" style problems---ones where the thread could
-- go ahead as far as it likes using only local data.  There
-- are a couple solutions to the problem:
				
--   (1) If we commit to the local world, then we would need to
-- do a full merge of the local & global worlds.  

--   (2) We could build up the new items as a Map (rather than
-- a list), and modify get so that it always checks the local
-- item collection before the (snapshot of) the global one.

-- For now, however, we just live with the problem:

runSomeSteps :: Graph -> Collections -> Int -> Bundle [NewTag] -> [PrimedStep] -> Bundle [NewTag]

-- If we run out of work we have to stop:
runSomeSteps _ _ n (rec @ B{intags=[]}) [] = rec
--trace ("Out of work.. stopping blocked: "++ show (length blocked)) $ 
--(blocked,bsteps,[],items)

-- If we're over our limit, we stop even if there's work left.
-- (But we make sure to finish the already primed steps.)
runSomeSteps _ _ n bundle [] | n <= 0 = bundle
					     
-- In this case we're out of primed steps, but we have more tags.
-- We prime a batch of new steps (corresponding to the next tag).
runSomeSteps graph w n (rec @ B{intags = hd:tl}) [] = 
	   case hd of 
	    NT id tag ->
	     runSomeSteps graph w n rec{intags=tl} (callSteps graph id tag)

-- Here's where we do the real work, execute the next primed step:
runSomeSteps g w n (rec @ B{..}) (pstep:tl) = 
           -- INVOKE THE STEP!
	   case pstep w of
	     -- Accumulate blocked tokens:
	     newb@(Block _ _) -> 
		 runSomeSteps g w (n-1) 
		    rec{blocked= newb:blocked, bsteps= pstep:bsteps} tl

             -- Alas, we don't know which of these newtags are really
             -- FRESH (seen for the first time) until we merge it back
             -- into the global world state.
	     Done newtags newitems ->
		 runSomeSteps g w (n-1) 
		    rec{outtags=newtags++outtags, items=newitems++items} tl


--------------------------------------------------------------------------------
-- Common interface for interoperability:

-- The StepCode monad carries forward a state (newtags, newitems) and
-- blocks on a failed get.
data StepCode a = CC (Collections -> [NewTag] -> [NewItem] -> (Maybe a, StepResult))
-- [2010.05.03] Could probably do this with a state and exception monad transformers.

-- Currently ONE GRAPH context implicit in the monad (could do many):
-- GraphCode threads through the Collections and Graph values:
data GraphCode a = GC (Collections -> Graph -> [NewTag] -> (Collections, Graph, [NewTag], a))



newTagCol      :: GraphCode (TagCol a)
newItemCol     :: GraphCode (ItemCol a b)

-- The monadic interface 
put  :: (Show a, Ord a) => ItemCol a b -> a -> b -> StepCode ()
get  :: (Show a, Ord a) => ItemCol a b -> a -> StepCode b
putt :: Ord a => TagCol  a   -> a -> StepCode ()

-- StepCode accumulates a list of new items/tags without committing them to the Collections.
instance Monad StepCode where 
    return x = CC$ \w nt ni -> (Just x, Done nt ni)
    -- Bind catches blocks and threads the state through:
    (CC ma) >>= f = CC$ \w nt ni -> 
		          case ma w nt ni of 
			   (_, Block ic t) -> (Nothing, Block ic t)
			   (Just a, Done nt' ni') -> let CC mb = f a 
						     in mb w nt' ni'
get col tag = CC $
    \ w tags items -> 
      case _get w col tag of 
       Nothing -> (Nothing, Block col tag)
       Just x  -> (Just x,  Done tags items)

put col tag val = CC $ 
   \ w tags items -> 
      (Just (), Done tags (_put col tag val : items))

putt col tag = CC $ 
   \ w tags items -> 
      (Just (), Done (_putt col tag : tags) items)


-- The graph monad captures code that builds graphs:
instance Monad GraphCode where 
    return x = GC$ \ w g it -> (w,g,it, x)
    (GC ma) >>= f = 
      GC $ \w g itags -> 
	let (w',g',it',a) = ma w g itags
	    GC mb = f a
	in mb w' g' it'
	
newTagCol = 
    GC$ \(cnt, MT tags, items) graph inittags -> 
      let newtags = IntMap.insert cnt Set.empty tags in
       ((cnt+1, MT newtags, items), 
        graph, inittags,TCID cnt)

newItemCol = 
    GC$ \(cnt, tags, MI items) graph inittags -> 
      let newitems = IntMap.insert cnt Map.empty items in
       ((cnt+1, tags, MI newitems), 
        graph, inittags, ICID cnt)

prescribe :: Ord a => TagCol a -> (a -> StepCode ()) -> GraphCode ()
--prescribe tc step = 
prescribe tc stepcode = 
    GC$ \ cols graph inittags -> 
	(cols,
	 _prescribe tc 
	     (\a w -> 
	      let CC fn = stepcode a 
	          (_,result) = fn w [] []
	      in result)
	     graph,
	 inittags, ())    

-- Initialize runs StepCode but does not invoke the scheduler.
-- You should not do any 'gets' inside this StepCode.
-- New tags introduced are accumulated as "inittags":
initialize :: StepCode a -> GraphCode a
initialize (CC fn) = 
    GC$ \w graph inittags -> 
     case fn w inittags [] of 
       -- This commits the new tags/items to the Collections
       (Just x, Done nt ni) ->  
	   --seq (unsafePerformIO $ putStrLn $ show ("  initializing", length nt, length ni)) $
	   let (w2,[]) = mergeUpdates [] ni w
           in (w2, graph, nt, x)
       (Nothing, Block itemcol tag) ->
	   error ("Tried to run initialization StepCode within the GraphCode monad but it blocked!: "
		 ++ show (itemcol, tag))

-- Execute is like init except that it invokes the scheduler.
-- Any tags already in the collection are taken to be "unexecuted"
-- and make up the inittags argument to the scheduler.
-- 
-- NOTE: The current philosophy is that the scheduler runs until
-- nothing is blocking.  Thus the finalize action won't need to block.
-- A different method would be to only run the scheduler just enough
-- to satisfy the finalize action.  That would be nice.
finalize :: StepCode a -> GraphCode a
finalize (CC fn) = 
    GC$ \w graph inittags -> 	
	case w of  
	 (_, MT tmap, _) -> 
	  let finalworld = scheduler graph inittags w in
	  -- After the scheduler is done executing, then when run the final action:
          case fn finalworld [] [] of 
	   (Just x, Done [] []) -> (finalworld, graph, [], x)
	   (Just _, Done _ _)   -> error "It isn't proper for a finalize action to produce new tags/items!"
	   (Nothing, Block itemcol tag) ->
	    error ("Tried to run finalization StepCode but it blocked!: "
		 ++ show (itemcol, tag))

-- Run a complete CnC graph and get a final result.
runGraph :: GraphCode a -> a
runGraph (GC fn) = x
    where (_,_,_,x) = fn (_newWorld 0) emptyGraph []


gcPrintWorld :: String -> GraphCode ()
gcPrintWorld str =
  GC$ \w g it -> 
   case w of 
    (n, MT tmap, MI imap) ->
     seq (unsafePerformIO $
	  do putStr "GraphCode - Printing world: "
	     putStrLn str
	     putStrLn ("  "++ show (IntMap.size tmap) ++" tag collections "++
		              show (IntMap.size imap) ++" item collections")
	     mapM (\key -> 		    
		    let m = IntMap.findWithDefault (error "shouldn't happen") key tmap in
		    putStrLn ("    Tag col "++ show key ++" size "++ show (Set.size m)))
	       (IntMap.keys tmap)

	     mapM (\key -> 		    
		    let m = IntMap.findWithDefault (error "shouldn't happen") key imap in
		    putStrLn ("    Item col "++ show key ++" size "++ show (Map.size m)))
	       (IntMap.keys imap)
	 )
     (w,g,it,())
       
--   show (n, IntMap.size tmap, IntMap.keys imap, 
-- 	Map.keys foo, 
-- 	Map.elems foo)


-- cncPutStr :: String -> GraphCode ()
-- cncPutStr str = 
--   GC$ \w g it -> 
--      seq (unsafePerformIO (putStr str))
-- 	 (w,g,it,())

-- stepPutStr :: String -> StepCode ()
-- stepPutStr str =
--   CC$ \w nt ni -> 
--      seq (unsafePerformIO (putStr str))
-- 	 (Just (), Done nt ni)

-- -- For debugging we have print messages lifted into the CnC monads.
-- stepPutStr' :: String -> StepCode ()
-- stepPutStr' msg = 
--    CC$ \w nt ni -> trace msg (Just (), Done nt ni)

cncUnsafeIO :: IO a -> GraphCode a
cncUnsafeIO action = 
  GC$ \w g it -> 
     let v = unsafePerformIO action
     in seq v (w,g,it,v)

stepUnsafeIO :: IO a -> StepCode a
stepUnsafeIO action = 
  CC$ \w nt ni -> 
     let v = unsafePerformIO action
     in seq v (Just v, Done nt ni)

stepPutStr str = stepUnsafeIO (putStr str)
cncPutStr  str = cncUnsafeIO  (putStr str)


finalmagic :: ItemCol a b -> [(c,d)] -> [(a,b)]
finalmagic id ls = unsafeCoerce ls

itemsToList :: ItemCol a b -> StepCode [(a,b)]
itemsToList id = 
 CC $ \w tags items -> 
    case w of 
     (_, _, MI imap) ->
      let ICID num = id 
	  it = (IntMap.!) imap num
      in (Just (finalmagic id (Map.toList it)),
	  Done tags items)

-- This is just a serial loop for now:
cncFor :: Int -> Int -> (Int -> StepCode ()) -> StepCode ()
cncFor start end body = for_ start (end+1) body

cncFor2D :: (Int,Int) -> (Int,Int) -> (Int -> Int -> StepCode ()) -> StepCode ()
cncFor2D (s1,s2) (e1,e2) body =
  cncFor s1 e1 $ \ i ->  
   cncFor s2 e2 (body i)



--------------------------------------------------------------------------------
-- Testing:
--------------------------------------------------------------------------------


type TI = TagCol  Char
type II = ItemCol Char Int
incrStep :: II -> (TI, II) -> Step Char
incrStep d1 (t2,d2) tag c = 
    case _get c d1 tag of 
      Nothing -> Block d1 tag
      Just n ->  Done [_putt t2 tag]
		      [_put  d2 tag (n+1)]

-- Test using the function interface directly:
test1 = TestCase $ 
    -- Allocate collections:
    let w0      = _newWorld 0
        (t1,w2) = _newTagCol  w0
        (t2,w3) = _newTagCol  w2
        (t3,w4) = _newTagCol  w3
        (d1,w5) = _newItemCol w4
        (d2,w6) = _newItemCol w5
        (d3,w7) = _newItemCol w6
		  
        -- Initialize:
        (w8,[]) = mergeUpdates [] [_put d1 'a' 33, 
 				   _put d1 'b' 100] w7

        graph = _prescribe t1 (incrStep d1 (t2,d2)) $
 		_prescribe t2 (incrStep d2 (t3,d3)) $
 		emptyGraph

        inittags = [_putt t1 'b', _putt t1 'a']

        w9 = scheduler graph inittags w8

    in 
     do putStrLn $ ""
	putStrLn $ showcol w9
        putStrLn $ "  d1: " ++ show (_get w9 d1 'a', _get w9 d1 'b') 
        putStrLn $ "  d2: " ++ show (_get w9 d2 'a', _get w9 d2 'b') 
        putStrLn $ "  d3: " ++ show (_get w9 d3 'a', _get w9 d3 'b') 
        return ()

-- Same test using wrappers:	 
test2 = TestCase $ 
  let v = runGraph $ do
        t1 <- newTagCol
        t2 <- newTagCol
        t3 <- newTagCol
        d1 <- newItemCol
        d2 <- newItemCol
        d3 <- newItemCol
		  
	initialize $ do stepPutStr "\n"
			put d1 'a' 33
 			put d1 'b' 100
			putt t1 'b'
			putt t1 'a'

        let incrStep d1 (t2,d2) tag = 
	     do n <- get d1 tag
	        put d2 tag (n+1)
	        putt t2 tag

        prescribe t1 (incrStep d1 (t2,d2))
 	prescribe t2 (incrStep d2 (t3,d3))

        gcPrintWorld "Initialization finished"

        -- Get some of the results:
	finalize $ 
	  do a <- itemsToList d1
	     b <- itemsToList d2
	     c <- itemsToList d3
	     return (a,b,c)
		      
  in putStrLn ("Final: "++ show v)

showcol (n, MT tmap, MI imap) =
  show (n, IntMap.size tmap, IntMap.keys imap, 
	Map.keys foo, 
	Map.elems foo)
 where 
    -- Hack -- pull out the first item collection:
    foo = (unsafeCoerce $ (IntMap.!) imap 3) :: ItemColInternal Char Int

--------------------------------------------------------------------------------

tests = TestList [test1, test2]
