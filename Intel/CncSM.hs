
--------------------------------------------------------------------------------------------
-- Currently this file is #included to mix it with different data structure implementations:
--------------------------------------------------------------------------------------------

-- Author: Simon Marlow
-- Modified and extended by Ryan Newton

-- XXX: make TagCol/ItemCol proper data types
type TagCol  a   = (IORef (Set.Set a), IORef [Step a])
-- Taking specific advantage of TVars in this implementation:
type ItemCol a b = HotVar (Map.Map a (Either b [Step b]))

newItemCol = C.liftIO $ newHotVar Map.empty
newTagCol  = C.liftIO $ do
  tags <- newIORef Set.empty
  steps <- newIORef []
  return (tags,steps)

data Sched = Sched (HotVar [StepCode ()]) (HotVar (Set.Set ThreadId))
--  deriving Show

type ReaderOnly = R.ReaderT Sched IO

type StepCode  a = C.ContT () ReaderOnly a
type GraphCode a = StepCode a

runGraph x = unsafePerformIO $ do
  stack <- newHotVar []
  threads <- newHotVar Set.empty
  r <- newIORef undefined
  R.runReaderT (C.runContT x (C.liftIO . writeIORef r)) (Sched stack threads)
  readIORef r

type M r a = C.ContT r (R.ReaderT Sched IO) a

mycallCC :: ((forall b . a -> M r b) -> M r a) -> M r a
mycallCC f = C.ContT $ \c -> C.runContT (f (\a -> C.ContT $ \_ -> c a)) c

-- putt :: TagCol  tag -> tag -> StepCode ()
putt = proto_putt $ \ steps tag -> pushSteps steps tag

pushSteps :: [Step tag] -> tag -> StepCode ()
pushSteps steps tag = do
  Sched stack _ <- R.ask
  C.liftIO $ sequence_ [ push stack (step tag) | step <- steps ]

-- get :: ItemCol k v -> k -> StepCode v
get col tag =
  mycallCC $ \cont -> do

-- Breaking the abstraction here so that we can take special advantage of TVars:
#if HOTVAR == 3
    r <- C.liftIO $ atomically $ do 
      m <- readTVar col
#define TMPWRITEVAR writeTVar
#else
    r <- C.liftIO $ do 
      m <- readHotVar col
#define TMPWRITEVAR writeHotVar
#endif
      case Map.lookup tag m of
        Nothing -> do
           TMPWRITEVAR col $! Map.insert tag (Right [cont]) m
           return reschedule
        Just (Left v) -> return (cont v)
        Just (Right steps) -> do
           TMPWRITEVAR col $! Map.insert tag (Right (cont:steps)) m
           return reschedule
    r

-- finalize :: StepCode a -> GraphCode a
finalize finalAction = 
    do joiner <- GRAPHLIFT newChan 
       --(state1 @ HiddenState5 { stack, numworkers, myid }) <- S.get							      
       Sched stack threads <- R.ask

       -- This is a little redundant... the reschedule loop will keep track of terminating when the queue goes empty.
       let worker :: Int -> GraphCode () = \id ->
       	       do x <- GRAPHLIFT pop stack
		  tid <- GRAPHLIFT myThreadId
		  ls <- GRAPHLIFT readHotVar stack
		  --cncPutStr$ "\n *** WORKER LOOP stack len "++ show (length ls) ++ " threadid " ++ show tid ++"\n"
       		  case x of 
		    -- Termination on first empty queue observation:
       		    Nothing -> do --cncPutStr$ "\n *** WORKER " ++ show tid ++" terminating...\n"
			          GRAPHLIFT writeChan joiner id
       		    Just stepcode -> 
       			do stepcode 
       	                   worker id -- keep going

       let worker_io n = R.runReaderT (C.runContT (worker n) (return)) (Sched stack threads)

       -- Having a problem with the below version... Not gettign to the terminate continuation & blocking indefiinetyl.
       {-
       let worker_io n = R.runReaderT (C.runContT (worker n) 
				       (\x -> do GRAPHLIFT putStrLn $ "\n *** WORKER " ++ show n ++" terminating..."
					         GRAPHLIFT writeChan joiner n
					         return x))
			              (Sched stack threads)
       -}

       --cncPutStr$ " *** Forking workers.\n"

       -- Fork one worker per thread:
       -- For this version there's no way PIN_THREADS could be bad:
       -- #ifdef PIN_THREADS
#if PIN
       GRAPHLIFT forM_ [0..numCapabilities-1] (\n -> forkOnIO n (worker_io n)) 
#else
       GRAPHLIFT forM_ [0..numCapabilities-1] (\n -> forkIO (worker_io n)) 
#endif

       --cncPutStr$ " *** Forked, now block on workers.\n"

       -- ================================================================================
       -- FIXME: NO WORKER RESTART ON PREMATURE TERMINATION RIGHT NOW!!
       -- ================================================================================

       -- This waits for quiescense BEFORE doing the final action
       -- let waitloop = do num <- readHotVar numworkers
       -- 	                 if num == 0
       -- 			  then return () 
       -- 			  else do readChan joiner -- A return message.
       -- 				  atomicDecr numworkers
       -- 				  waitloop
       -- GRAPHLIFT waitloop

       -- Wait till all workers complete.
       GRAPHLIFT forM_ [1.. numCapabilities] $ \_ -> do readChan joiner
							--putStrLn "    *** Got return token!"

       --cncPutStr$ " *** Workers returned, now finalize action:\n"
       
       finalAction			   


-- RRN: We need to suppress attempts the scheduling loop from starting
-- right away inside the initialize action.  We don't want to start it
-- till we've forker workers.  This *shouldnt* happen because gets are
-- not supposed to be allowed in initialize... (need to enforce it
-- though).
initialize x = x


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
#if HOTVAR == 3
  steps <- C.liftIO $ atomically $ do
    m <- readTVar col
#else
  steps <- C.liftIO $ do
    m <- readHotVar col
#endif
    let !(mb_old, new) = Map.insertLookupWithKey 
                            (\_ new _ -> new) tag (Left item) m
#if HOTVAR == 3
    writeTVar col $! new
#else
    writeHotVar col $! new
#endif
    case mb_old of
      Nothing -> return []
      Just (Left v) -> error "multiple put"
      Just (Right steps) -> return steps
  pushSteps steps item

quiescence_support=False

itemsToList = undefined -- XXX
