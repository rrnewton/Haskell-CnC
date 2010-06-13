{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , TypeFamilies 
  , UndecidableInstances
  , OverlappingInstances
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , NamedFieldPuns
  #-}
-- , ImpredicativeTypes   
--   , Rank2Types
-- State monad transformer is needed for both step & graph:
#ifndef MODNAME
#define MODNAME Intel.Cnc10
#endif
#define CNC_SCHEDULER 10

#define STEPLIFT  S.lift$ S.lift$
#define GRAPHLIFT S.lift$

#define SUPPRESS_runGraph

#define SUPPRESS_put
#define SUPPRESS_newItemCol
#define SUPPRESS_itemsToList
#define SUPPRESS_initialize
#define SUPPRESS_graphInStep

-- #define DEFINED_free_items

-- USE_GMAP is just to trigger the right type class prereqs:
#define USE_GMAP
#include "Cnc.Header.hs"

-- import Control.Monad.Cont
-- import Control.Monad.Cont
-- import Data.IORef
-- import Data.Set as Set
-- import Data.Map as Map
-- import System.IO.Unsafe

-- import qualified  Control.Monad.State.Strict as S 


------------------------------------------------------------
-- Version 10: A merger of the manual-get-syncing in versions 8 & 9
-- with the global work queue in version 4-7.

-- The idea here is to verify that the attempt to use sparks in
-- version 9 isn't shooting us in the foot.

--------------------------------------------------------------------------------

type StateOnly a = (S.StateT (HiddenState5) IO a)

type GraphCode a = StateOnly a

type StepCode a = ContT ContResult (S.StateT (HiddenState5) IO) a

data ContResult = ContResult

-- Contains the escape continuation to jump out of the current step.
data EscapeCont = EC (() -> StepCode ())

data HiddenState5 = 
    HiddenState5 { stack :: HotVar [StateOnly ()],
		   numworkers :: HotVar Int, 
		   makeworker :: Int -> IO (), 
		   myid :: Int,
		   escapeCont :: EscapeCont
		 }
  deriving Show

type TagCol a   = (IORef (Set.Set a), IORef [Step a])

-- In this version we don't use MVars in the RHS because gets don't block:
--newtype ItemCol a b = ItemCol (IORef (GMap a ((Maybe b), WaitingSteps b)))
--newtype ItemCol a b = ItemCol (IORef (Map.Map a ((Maybe b), WaitingSteps b)))
newtype ItemCol a b = ItemCol (HotVar (Map.Map a ((Maybe b), WaitingSteps b)))

type WaitingSteps b = [b -> StepCode ()]

--------------------------------------------------------------------------------

instance Show (ItemCol a b) where
  show _ = "<itemcol>"
instance Show EscapeCont where
  show _ = "<escapeCont>"
instance Show (Int -> IO ()) where
  show _ = "<int to IO unit function>"

atomicIncr x = modifyHotVar_ x (+ 1)
atomicDecr x = modifyHotVar_ x (\n -> n-1)
atomicModifyIORef_ ref fn = atomicModifyIORef ref (\x -> (fn x, ()))

-- A simple stack interface:
----------------------------------------
push   :: HotVar [a] -> a -> IO ()
tryPop :: HotVar [a] -> IO (Maybe a)
push stack val = modifyHotVar_ stack (val:)
tryPop stack   = modifyHotVar stack tryfirst
  where 
    tryfirst []    = ([], Nothing)
    tryfirst (a:b) = (b,  Just a)
----------------------------------------

defaultState = 
  do hv  <- newHotVar []
     hv2 <- newHotVar 0
     hv3 <- newHotVar Set.empty
     let msg = "Intel.Cnc"++ show CNC_SCHEDULER ++" internal error: makeworker thunk used before initalized"
     return$ HiddenState5 { stack = hv, numworkers = hv2, makeworker= error msg, myid = -1,
			    escapeCont = EC$ error "unitialized escape continuation" }

--------------------------------------------------------------------------------

#warning " LOADING  SCHEDULER 10... "

newItemCol = do ref <- GRAPHLIFT newHotVar Map.empty
 		return (ItemCol ref)

get (ItemCol icol) tag = 
   callCC $ \ k -> 
    do (HiddenState5 { escapeCont }) <- S.lift S.get
       let (EC escape) = escapeCont

       let addquit ls = 
	    do -- If it's not there, store k for later and escape:
	       STEPLIFT modifyHotVar_ icol (Map.insert tag (Nothing, k:ls))
   	       -- After adding ourself to the wait list, jump out of this step:
	       escape ()
	       error "Should never reach this"

       map <- STEPLIFT readHotVar icol 
       case Map.lookup tag map of
   	 Nothing                 -> addquit [] 
   	 Just (Nothing, waiting) -> addquit waiting
   	 Just (Just v,  [])      -> return v
   	 Just (Just v, a:b)      -> error "CnC: internal invariant violated"


foo :: Int -> Int
foo x = x 


put (ItemCol icol) tag (!item) = 
    do waiting <- STEPLIFT modifyHotVar icol mod 

       -- Wake up waiting steps:
       (HiddenState5 {stack}) <- lift S.get
       STEPLIFT modifyHotVar_ stack ((map (\f -> runStep (f item)) waiting) ++)

   where 
       mod map = 
	 let new = (Just item, [])
	     f key _ (Nothing, _) = new
#ifdef REPEAT_PUT_ALLOWED
	     f key _ old@(Just v, ls) = old
#else
	     f key _ (Just v, _)  = error ("Single assignment violated at tag: "++ show tag)
#endif
	     (old, map') = Map.insertLookupWithKey f tag new map
	 in case old of
	      Nothing                 -> (map', [])
	      Just (Nothing, waiting) -> (map', waiting)
#ifdef REPEAT_PUT_ALLOWED
	      Just (Just _, waiting)  -> (map , waiting)
#else
	      Just (Just _, _)        ->  error ("Single assignment violated at tag: "++ show tag)
#endif



executeStep :: StepCode () -> StepCode ()
executeStep step = 
  callCC $ \ escape -> 
    do S.lift$ S.modify (\r -> r { escapeCont = EC escape })
--    do S.lift$ S.put (HiddenState5 undefined)
       step


-- Remove the continuation monad
runStep :: StepCode () -> StateOnly ()
runStep m = do runContT m (\() -> return ContResult); return ()

--executeStep :: StepCode a -> StepCode a

putt = proto_putt
	(\ steps tag -> 
	   do (HiddenState5 { stack }) <- C.lift S.get
              foldM (\ () step -> 
		     STEPLIFT push stack (runStep $ step tag))
                       () steps)


itemsToList = error "itemsolist unimplemented"
graphInStep = C.lift

initialize = runStep

runGraph x = unsafePerformIO (runState x)
runState x =
    do state <- defaultState 
       (a,_) <- S.runStateT x state
       return a

finalize finalAction = 
    do joiner <- GRAPHLIFT newChan 
       (state1 @ HiddenState5 { stack, numworkers, myid }) <- S.get

       -- At this level we don't need the ContT transformer, hence "StateOnly":
       let worker :: Int -> StateOnly () = \id ->
	       do x <- GRAPHLIFT tryPop stack
		  case x of 
		    Nothing -> GRAPHLIFT  writeChan joiner id
		    Just action -> 
			do action 
			   myId <- GRAPHLIFT myThreadId
			   worker id -- keep going
			   
       -- Here we install the makeworker funciton in the monad state:
       let mkwrkr id = do S.runStateT (worker id) (state2 id); return ()
           state2 id = state1 { makeworker = mkwrkr, myid = id }
       -- Write it back for the "finalAction" below:
       S.put (state2 myid)
       GRAPHLIFT modifyHotVar_ numworkers (+ numCapabilities)

       -- Fork one worker per thread:
       GRAPHLIFT forM_ [0..numCapabilities-1] (\n -> forkIO (mkwrkr n)) 
       --GRAPHLIFT forM_ [0..numCapabilities-1] (\n -> forkOnIO n (mkwrkr n)) 

       -- This waits for quiescense BEFORE doing the final action
       let waitloop = do num <- readHotVar numworkers
	                 if num == 0
			  then return () 
			  else do readChan joiner -- A return message.
				  atomicDecr numworkers
				  waitloop
       GRAPHLIFT waitloop

       -- Here's a bit of a hack... we use IO to get the result out of
       -- the continuation:
       var <- GRAPHLIFT newEmptyMVar
       let --k :: a -> StateOnly ContResult 
	   k x = do GRAPHLIFT putMVar var x
		    return ContResult
       runContT finalAction k
       result <- GRAPHLIFT readMVar var
       return result




ref = unsafePerformIO (newIORef undefined)

--test step =

test step =
  callCC $ \ escape -> 
    do S.lift$ writeIORef ref escape
       fn <- S.lift$ readIORef ref
       fn 33



        
myitems = do i :: ItemCol Int Int <- newItemCol
             return i


--------------------------------------------------------------------------------

quiescence_support = True



