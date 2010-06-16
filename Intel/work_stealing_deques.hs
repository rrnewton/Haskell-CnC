-----------------------------------------------------------------------------
-- Work stealing DEQUES 
-----------------------
-- To be included in other files:
-----------------------------------------------------------------------------

data Sched = Sched 
    { workpool :: Array.Array Int (HotVar (Seq.Seq (StepCode ()))),
      randoms :: Array.Array Int (IORef Random.StdGen),
      myid :: Int
     }
--  deriving Show

defaultState = do
  dvars <- forM [0..numCapabilities] $ \_ -> newHotVar Seq.empty
  let randoms = Array.listArray (0,numCapabilities) [ Random.mkStdGen id | id <- [0..numCapabilities ]]
  rands <- sequence [ newIORef (Random.mkStdGen id) | id <- [0..numCapabilities ]]
  let randoms = Array.listArray (0,numCapabilities) rands
  let deques  = Array.listArray (0,numCapabilities) dvars
  return$ Sched { workpool= deques, 
		  randoms = randoms,
		  myid = 0 -- The scheduler thread.
		}

pushWork :: StepCode () -> StepCode ()
popWork  :: R.ReaderT Sched IO (Maybe (StepCode ()))

-- One option here is that at the end of the initialize action, stripe
-- the work across dequeus.  Another option to accomplish the same
-- work-sharing effect is to round-robin distribute the work when a
-- push is done from the scheduler thread running "initialize".
initialize initAction = 
  do initAction
     Sched { workpool }  <- R.ask
     -- Read the deque of the scheduler thread.  No parallelism yet so don't worry about atomicity:
     initdeque <- C.liftIO$ readHotVar (workpool Array.! 0) 
     let segs = splitSeq numCapabilities initdeque
     C.liftIO$ forM_ (zip [1..] $ segs) $ \ (id,seq) -> 
       modifyHotVar_ (workpool Array.! id) $ \ olddeq -> 
         if not$ Seq.null olddeq
	 then error$ "Worker's deque ("++ show id ++") was not empty at initialization, size: "++ show (Seq.length olddeq)
	 else seq
     stepPutStr$ " +++ FINISHED DISSEMINATING " ++ show (Seq.length initdeque) ++ " initial units of work!: "
		 ++ show (map Seq.length segs) ++ "\n"


splitSeq :: Int -> Seq.Seq a -> [Seq.Seq a]
splitSeq pieces seq = all
  where 
   (all, _)     = loop portion (pieces-remain) seq' bigs
   (bigs, seq') = loop (portion+1) remain seq []

   len = Seq.length seq
   (portion, remain) = len `quotRem` pieces
   loop block 0 seq acc = ( acc, seq )
   loop block n seq acc = 
      let (hd,tl) = Seq.splitAt block seq 
      in loop block (n-1) tl (hd:acc)

	 
-- Or... we could just make stealing do everything.  But work-sharing
-- is probably worthwhile here, because so many of our benchmarks dump a load of tags at the beginning.

pushWork something =
  -- First get the state, including array of deques
  do Sched { workpool, myid }  <- R.ask
     let mydeque = workpool Array.! myid
     stepPutStr$ " +++ Putting into deque of id "++ show myid ++ "\n"
     -- Add to my dequeue, on the right:
     C.liftIO$ hotVarTransaction $ 
      do old <- readHotVarRaw mydeque; 
     	 writeHotVarRaw mydeque (old Seq.|> something)
     return ()

popWork = do 
  -- First get the state, including array of deques
  Sched { workpool, randoms, myid }  <- R.ask

  let mydeque = workpool Array.! myid  
      myrandom = randoms Array.! myid  
      -- takeleft deq = C.liftIO$ hotVarTransaction $ do 
      -- 		       seq <- readHotVarRaw deq
      -- 		       case Seq.viewl seq of
      -- 		         Seq.EmptyL -> return Nothing
      -- 		         (x  Seq.:<  xs') -> do
      -- 		            writeHotVarRaw deq xs'
      -- 			    return (Just x)
--  mine <- takeit mydeque

      stealLoop = do 
	-- Our RNG state is private, this doesn't need to be atomic:
        rng <- readIORef myrandom
        let (victim, rng') = Random.randomR (1,numCapabilities-1) rng 
	    -- Bounce steals from ourself to the one omitted:
	    -- (This simply won't happen if *we* are numCapabilities.)
	    victim' = if victim == myid then numCapabilities else victim
        writeIORef myrandom rng'
	putStrLn$ " +++  Worker "++ show myid ++" trying to steal from "++ show victim'	
	-- Try to steal; steal from the left:
	let theirs = workpool Array.! victim'
	stolen <- C.liftIO$ hotVarTransaction $ do 
	     seq <- readHotVarRaw theirs
	     let view = Seq.viewl seq
	     case view of
	       (x  Seq.:<  xs') -> writeHotVarRaw mydeque xs'
	       Seq.EmptyL -> return ()
	     return view
	case stolen of 
	  Seq.EmptyL -> 
	     -- After every failed steal, check to see if we should terminate.
	     do if False 
		 then undefined 
		 else do putStrLn$ " +++    -> Steal failed."
		         return Nothing
--		 else stealLoop
	  (x  Seq.:<  _) -> return (Just x)

  -- Try to get work from our local dequeue, read from the right:
  mine <- C.liftIO$ hotVarTransaction $ do 
	     seq <- readHotVarRaw mydeque
	     let view = Seq.viewr seq
	     case view of
	       (xs'  Seq.:> _) -> writeHotVarRaw mydeque xs'
	       Seq.EmptyR -> return ()
	     return view

  -- Out of transaction, 
  case mine of 
    Seq.EmptyR -> C.liftIO$ stealLoop
    (_  Seq.:> x) -> return (Just x)

  -- -- If not, then pick a victim and steal.
  --  hotVarTransaction $ do 
  -- xs <- readHotVarRaw v
  -- case xs of
  --   [] -> return Nothing
  --   x:xs' -> do
  --     writeHotVarRaw v xs'
  --     return (Just x)



-----------------------------------------------------------------------------
