
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



type ReaderOnly = R.ReaderT Sched IO

type StepCode  a = C.ContT () ReaderOnly a
type GraphCode a = StepCode a

runGraph x = unsafePerformIO $ do
  state <- defaultState
  r <- newIORef (error "Uninitialized graph result read prematurely")
  R.runReaderT (C.runContT x (C.liftIO . writeIORef r)) state
  readIORef r

type M r a = C.ContT r (R.ReaderT Sched IO) a

mycallCC :: ((forall b . a -> M r b) -> M r a) -> M r a
mycallCC f = C.ContT $ \c -> C.runContT (f (\a -> C.ContT $ \_ -> c a)) c

-- putt :: TagCol  tag -> tag -> StepCode ()
putt = proto_putt $ \ steps tag -> pushSteps steps tag

pushSteps :: [Step tag] -> tag -> StepCode ()
pushSteps steps tag = do
--  C.liftIO $ sequence_ [ pushWork stack (step tag) | step <- steps ]
-- Make sure THIS doesn't cause any performance loss:
  sequence_ [ pushWork (step tag) | step <- steps ]

-- get :: ItemCol k v -> k -> StepCode v
get col tag =
  mycallCC $ \cont -> do
    r <- C.liftIO $ hotVarTransaction $ do 
      m <- readHotVarRaw col
      case Map.lookup tag m of
        Nothing -> do
           writeHotVarRaw col $! Map.insert tag (Right [cont]) m
           return reschedule
        Just (Left v) -> return (cont v)
        Just (Right steps) -> do
           writeHotVarRaw col $! Map.insert tag (Right (cont:steps)) m
           return reschedule
    -- Out of the transaction, bounce on that trampoline:
    r

-- finalize :: StepCode a -> GraphCode a
proto_finalize finalAction = 
    do joiner <- GRAPHLIFT newChan 
       --(state1 @ HiddenState5 { stack, numworkers, myid }) <- S.get						      
       (state) <- R.ask

       -- This is a little redundant... the reschedule loop will keep track of terminating when the queue goes empty.
       let worker :: Int -> GraphCode () = \id ->
       	       do x <- C.lift$ popWork
		  -- tid <- GRAPHLIFT myThreadId
       		  case x of 
		    -- Termination on first empty queue observation:
       		    Nothing -> do --cncPutStr$ "\n *** WORKER " ++ show tid ++" terminating...\n"
			          GRAPHLIFT writeChan joiner id
       		    Just stepcode -> 
       			do stepcode 
       	                   worker id -- keep going

	   mystate n = state { myid = n }
           worker_io n = R.runReaderT (C.runContT (worker n) (return)) (mystate n)

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
       -- FIXING it for now:
#if 1
       -- #ifdef PIN_THREADS
       GRAPHLIFT forM_ [1..numCapabilities] (\n -> forkOnIO n (worker_io n)) 
#else
       GRAPHLIFT forM_ [1..numCapabilities] (\n -> forkIO (worker_io n)) 
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


       finalAction joiner


-- RRN: We need to suppress attempts the scheduling loop from starting
-- right away inside the initialize action.  We don't want to start it
-- till we've forker workers.  This *shouldnt* happen because gets are
-- not supposed to be allowed in initialize... (need to enforce it
-- though).
-- initialize x = x


reschedule :: StepCode a
reschedule = C.ContT rescheduleR

rescheduleR :: a -> R.ReaderT Sched IO ()
rescheduleR _ = do
  m <- popWork 
  case m of
    Nothing -> return ()
    Just (C.ContT f)  -> f rescheduleR

-- put  :: ItemCol k v -> k -> v  -> StepCode ()
put col tag (!item) = do
  steps <- C.liftIO $ hotVarTransaction $ do
    m <- readHotVarRaw col
    let !(mb_old, new) = Map.insertLookupWithKey 
                            (\_ new _ -> new) tag (Left item) m
    writeHotVarRaw col $! new
    case mb_old of
      Nothing -> return []
      Just (Left v) -> error$ "multiple put at tag "++ show tag
      Just (Right steps) -> return steps
  pushSteps steps item

itemsToList = error "itemstolist not implemented yet for this scheduler" -- XXX
