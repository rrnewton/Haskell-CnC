{-# LANGUAGE FlexibleInstances
  , BangPatterns
  , MagicHash 
  , ScopedTypeVariables
  , DeriveDataTypeable
  , MultiParamTypeClasses
  , RankNTypes
  #-}
-- We don't need to lift through a monad transformer for the step or
-- graph monads in this implementation:
#ifndef MODNAME
#define MODNAME Intel.CncSM
#endif
#define CNC_SCHEDULER 100
#define STEPLIFT  C.liftIO$
#define GRAPHLIFT C.liftIO$

#define SUPPRESS_put
#define SUPPRESS_newItemCol
#define SUPPRESS_newTagCol
#define SUPPRESS_itemsToList
#define SUPPRESS_runGraph

#include "Cnc.Header.hs"

#warning "Loading Scheduler 100"

-- XXX: make TagCol/ItemCol proper data types
type TagCol  a   = (IORef (Set.Set a), IORef [Step a])
type ItemCol a b = TVar (Map.Map a (Either b [Step b]))

newItemCol = C.liftIO $ newTVarIO Map.empty
newTagCol  = C.liftIO $ do
  tags <- newIORef Set.empty
  steps <- newIORef []
  return (tags,steps)

data Sched = Sched (Var [StepCode ()]) (Var (Set.Set ThreadId))
--  deriving Show

type StepCode  a = C.ContT () (R.ReaderT Sched IO) a
type GraphCode a = StepCode a

runGraph x = unsafePerformIO $ do
  stack <- newTVarIO []
  threads <- newTVarIO Set.empty
  r <- newIORef undefined
  R.runReaderT (C.runContT x (C.liftIO . writeIORef r)) (Sched stack threads)
  readIORef r

type M r a = C.ContT r (R.ReaderT Sched IO) a

mycallCC :: ((forall b . a -> M r b) -> M r a) -> M r a
mycallCC f = C.ContT $ \c -> C.runContT (f (\a -> C.ContT $ \_ -> c a)) c

-----------------------------------------------------------------------------
type Var a = TVar a

push :: Var [a] -> a -> IO ()
push v a = atomically $ do xs <- readTVar v; writeTVar v (a:xs)

pop :: Var [a] -> IO (Maybe a)
pop v = atomically $ do 
  xs <- readTVar v
  case xs of
    [] -> return Nothing
    x:xs' -> do
      writeTVar v xs'
      return (Just x)
-----------------------------------------------------------------------------

-- putt :: TagCol  tag -> tag -> StepCode ()
putt = proto_putt $ \ steps tag -> pushSteps steps tag

pushSteps :: [Step tag] -> tag -> StepCode ()
pushSteps steps tag = do
  Sched stack _ <- R.ask
  C.liftIO $ sequence_ [ push stack (step tag) | step <- steps ]

-- get :: ItemCol k v -> k -> StepCode v
get col tag =
  mycallCC $ \cont -> do
    r <- C.liftIO $ atomically $ do 
      m <- readTVar col
      case Map.lookup tag m of
        Nothing -> do
           writeTVar col $! Map.insert tag (Right [cont]) m
           return reschedule
        Just (Left v) -> return (cont v)
        Just (Right steps) -> do
           writeTVar col $! Map.insert tag (Right (cont:steps)) m
           return reschedule
    r

-- finalize :: StepCode a -> GraphCode a
finalize x = x -- XXX

{-
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

-}


reschedule :: StepCode a
reschedule = C.ContT rescheduleR

rescheduleR :: a -> R.ReaderT Sched IO ()
rescheduleR _ = do
  Sched stack _ <- R.ask
  m <- C.liftIO $ pop stack
  case m of
    Nothing -> return ()
    Just (C.ContT f)  -> f rescheduleR

-- put  :: ItemCol k v -> k -> v  -> StepCode ()
put col tag (!item) = do
  steps <- C.liftIO $ atomically $ do
    m <- readTVar col
    let !(mb_old, new) = Map.insertLookupWithKey 
                            (\_ new _ -> new) tag (Left item) m
    writeTVar col $! new
    case mb_old of
      Nothing -> return []
      Just (Left v) -> error "multiple put"
      Just (Right steps) -> return steps
  pushSteps steps item

quiescence_support=False

itemsToList = undefined -- XXX
