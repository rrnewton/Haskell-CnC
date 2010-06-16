-----------------------------------------------------------------------------
-- Work stealing DEQUES 
-----------------------

-- To be included in other files:
-----------------------------------------------------------------------------

pushWork :: StepCode () -> StepCode ()
popWork  :: R.ReaderT Sched IO (Maybe (StepCode ()))
	 
-- Or... we could just make stealing do everything.  But work-sharing
-- is probably worthwhile here, because so many of our benchmarks dump a load of tags at the beginning.

pushWork something =
  -- First get the state, including array of deques
  do Sched { workpool, myid }  <- R.ask
     let mydeque = workpool Array.! myid
     -- Add to my dequeue, on the right:
     old <- C.liftIO$ hotVarTransaction $ 
      do old <- readHotVarRaw mydeque; 
     	 writeHotVarRaw mydeque (old Seq.|> something)      
	 return old
     stepPutStr$ " +++ Pushed into deque of id "++ show myid ++ " previous length: "++ show (Seq.length old) ++"\n"
     return ()

popWork = do 
  -- First get the state, including array of deques
  Sched { workpool, randoms, killflag, myid }  <- R.ask

  let mydeque = workpool Array.! myid  
      myrandom = randoms Array.! myid  

      stealLoop = do 
	-- Our RNG state is private, this doesn't need to be atomic:
        rng <- readIORef myrandom
        let --(victim, rng') = Random.randomR (1,numCapabilities-1) rng 
	    -- Bounce steals from ourself to the one omitted:
	    -- (This simply won't happen if *we* are numCapabilities.)
	    --victim' = if victim == myid then numCapabilities else victim

	    (victim', rng') = Random.randomR (0,numCapabilities-1) rng 

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
	     do killed <- readIORef killflag
		if killed
		 then return Nothing
		 else do putStrLn$ " +++    -> Steal failed, thief = " ++ show myid
                         stealLoop
	  (x  Seq.:<  rest) -> 
	      do putStrLn$ " +++    STOLEN successfully, victim "++ show victim'++ " thief " 
			   ++ show myid ++ " victim had left: "++ show (Seq.length rest)
		 return (Just x)

  -- Try to get work from our local dequeue, read from the right:
  mine <- C.liftIO$ hotVarTransaction $ do 
	     seq <- readHotVarRaw mydeque
	     let view = Seq.viewr seq
	     case view of
	       (xs'  Seq.:> _) -> writeHotVarRaw mydeque xs'
	       Seq.EmptyR -> return ()
	     return view
  -- Out of transaction:
  x <- case mine of 
        Seq.EmptyR -> C.liftIO$ stealLoop
        (_  Seq.:> x) -> return (Just x)

  C.liftIO$ putStrLn$ " ---> Popped off work for "++ show myid
  return x


-----------------------------------------------------------------------------
