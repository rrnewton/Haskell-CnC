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
