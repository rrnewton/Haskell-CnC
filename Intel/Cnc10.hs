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

-- newtype GraphCode a = GC (S.StateT (HiddenState5) IO a)

-- -- Annoying.  Almost wish we could "derive" monad here.
-- instance Monad GraphCode where
--   (GC m) >>= f = GC (m >>= (\x -> case f x of GC v -> v))
--   return v = GC (return v)

-- type StepCode a = ContT a GraphCode a


-- #define SUPPRESS_HiddenState5
-- #define SUPPRESS_AllTypes

-- #include "shared_5_6.hs"

atomicIncr x = modifyHotVar_ x (+ 1)
atomicDecr x = modifyHotVar_ x (\n -> n-1)
atomicModifyIORef_ ref fn = atomicModifyIORef ref (\x -> (fn x, ()))

--newItemCol :: (Ord tag, Show tag, Eq tag) => GraphCode (ItemCol tag val)

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


-- -================================================================================

type StateOnly a = (S.StateT (HiddenState5) IO a)

type GraphCode a = StateOnly a

data ContResult = ContResult

type StepCode a = ContT ContResult (S.StateT (HiddenState5) IO) a
--type StepCode a = ContT a (S.StateT (HiddenState5) IO) a
--type StepCode a = ContT a (S.StateT (HiddenState5) IO) ()

--type StepCode b = forall a . ContT b (S.StateT (HiddenState5) IO) a

type TagCol a   = (IORef (Set.Set a), IORef [Step a])
--type ItemCol a b = MutableMap a b

-- Contains the escape continuation to jump out of the current step.
--newtype HiddenState5 a = HiddenState5 (() -> StepCode a)
--newtype HiddenState5 = HiddenState5 (EscapeCont)
--data EscapeCont = forall a . EC (() -> a)
--type EscapeCont = forall a . (() -> a)
data EscapeCont = EC (() -> StepCode ())

instance Show EscapeCont where
  show _ = "<escapeCont>"

instance Show (Int -> IO ()) where
  show _ = "<int to IO unit function>"

data HiddenState5 = 
--    HiddenState5 { stack :: HotVar [StepCode ()], 
    HiddenState5 { stack :: HotVar [StateOnly ()],
		   numworkers :: HotVar Int, 
		   makeworker :: Int -> IO (), 
		   myid :: Int,
		   escapeCont :: EscapeCont
		 }
  deriving Show

defaultState = 
  do hv  <- newHotVar []
     hv2 <- newHotVar 0
     hv3 <- newHotVar Set.empty
     let msg = "Intel.Cnc"++ show CNC_SCHEDULER ++" internal error: makeworker thunk used before initalized"
     return$ HiddenState5 { stack = hv, numworkers = hv2, makeworker= error msg, myid = -1,
			    escapeCont = EC$ error "unitialized escape continuation" }



-- In this version we don't use MVars because gets don't block:
newtype ItemCol a b = ItemCol (IORef (Map.Map a ((Maybe b), WaitingSteps b)))
--newtype ItemCol a b = ItemCol (MutableMap a ((Maybe b), WaitingSteps))
type WaitingSteps b = [b -> StepCode ()]

--data EscapeStep = EscapeStep  deriving (Show, Typeable)
--instance Exception EscapeStep

instance Show (ItemCol a b) where
  show _ = "<itemcol>"



newItemCol = do ref <- GRAPHLIFT newIORef Map.empty
 		return (ItemCol ref)

get (ItemCol icol) tag = 
   callCC $ \ k -> 
    do (HiddenState5 { escapeCont }) <- S.lift S.get
       let (EC escape) = escapeCont

       let addquit ls = 
	    do -- If it's not there, store k for later and escape:
   	       S.lift$ S.lift$ atomicModifyIORef_ icol (Map.insert tag (Nothing, k:ls))
	       
   	       -- After adding ourself to the wait list, jump out of this step:
	       escape ()
	       error "Should never reach this"

       map <- S.lift$ S.lift$ readIORef icol 
       case Map.lookup tag map of
   	 Nothing                 -> addquit [] 
   	 Just (Nothing, waiting) -> addquit waiting
   	 Just (Just v,  [])      -> return v
   	 Just (Just v, a:b)      -> error "CnC: internal invariant violated"


put = undefined
itemsToList = undefined
initialize = undefined
graphInStep = undefined
putt = undefined
runGraph = undefined

-- initfin :: String -> StepCode a -> GraphCode a
-- initfin str m = do let err = error str
-- 	           x <- try_stepcode err m
-- 	           case x of Nothing -> err
-- 		  	     Just v  -> return v

-- initialize = initfin "Get failed within initialize action!"
-- finalize   = initfin "Get failed within finalize action!"



--executeStep :: StepCode a -> StepCode a
executeStep :: StepCode () -> StepCode ()
executeStep step = 
  callCC $ \ escape -> 
    do S.lift$ S.modify (\r -> r { escapeCont = EC escape })
--    do S.lift$ S.put (HiddenState5 undefined)
       step


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



