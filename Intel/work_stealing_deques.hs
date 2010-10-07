-----------------------------------------------------------------------------
-- Work stealing DEQUES implemented in Haskell
-----------------------------------------------------------------------------

-- These are surely not as efficient as they could be.  Currently we
-- use atomic variables containing Data.Seq.  

-- Perhaps another option would be a IOArray circular buffer?

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

#if 0
     old <- C.liftIO$ hotVarTransaction $ 
      do old <- readHotVarRaw mydeque; 
     	 writeHotVarRaw mydeque (old Seq.|> something)      
	 return old
#else
     --old <- C.liftIO$ modifyHotVar mydeque $ \old -> (old Seq.|> something,  old)
     C.liftIO$ modifyHotVar_ mydeque (Seq.|> something)
#endif
     --if debugme then stepPutStr$ " +++ Pushed into deque of id "++ show myid ++ " previous length: "++ show (Seq.length old) ++"\n" else return ()
     return ()

popWork = do 
  -- First get the state, including array of deques
  Sched { workpool, randoms, killflag, myid }  <- R.ask

  let mydeque = workpool Array.! myid  
      myrandom = randoms Array.! myid  

      stealLoop = do 
	-- Our RNG state is private, this doesn't need to be atomic:
        rng <- readIORef myrandom
        let (victim, rng') = Random.randomR (0,numCapabilities-2) rng 
	    -- Bounce steals from ourself to the one omitted:
	    -- (This simply won't happen if *we* are numCapabilities.)
	    victim' = if victim == myid then numCapabilities-1 else victim

	    --(victim', rng') = Random.randomR (0,numCapabilities-1) rng 

        writeIORef myrandom rng'
	if debugme then putStrLn$ " +++  Worker "++ show myid ++" trying to steal from "++ show victim'	else return()
	-- Try to steal; steal from the left:
	let theirs = workpool Array.! victim'

#if 0
	stolen <- C.liftIO$ hotVarTransaction $ do 
	     seq <- readHotVarRaw theirs
	     let view = Seq.viewl seq
	     case view of
	       (_  Seq.:<  xs') -> writeHotVarRaw mydeque xs'
	       Seq.EmptyL -> return ()
	     return view
#else
	stolen <- C.liftIO$ modifyHotVar  theirs $ \ seq ->
	     let view = Seq.viewl seq in
	     (case view of
	        (_  Seq.:<  rest) -> rest
	        Seq.EmptyL        -> Seq.empty
	      , view)
#endif

	case stolen of 
	  Seq.EmptyL -> 
	     -- After every failed steal, check to see if we should terminate.
	     do killed <- readIORef killflag
		if killed
		 then return Nothing
		 else do if debugme then putStrLn$ " +++    -> Steal failed, thief = " ++ show myid else return()
                         stealLoop
	  (x  Seq.:<  rest) -> 
	      do if debugme
		   then putStrLn$ " +++    STOLEN successfully, thief <"++ show myid++ "> victim <" 
		                  ++ show victim' ++ "> victim had left: "++ show (Seq.length rest) 
		   else return () --putStr "!"
		 return (Just x)

  -- Try to get work from our local dequeue, read from the right:
#if 0
  mine <- C.liftIO$ hotVarTransaction $ do 
	     seq <- readHotVarRaw mydeque
	     let view = Seq.viewr seq
	     case view of
	       (xs' Seq.:> _) -> writeHotVarRaw mydeque xs'
	       Seq.EmptyR -> return ()
	     return view
#else
  mine <- C.liftIO$ modifyHotVar mydeque $ \seq ->
	     let view = Seq.viewr seq in
	     (case view of
	        (rest Seq.:> _) -> rest
	        Seq.EmptyR      -> Seq.empty
	      , view)
#endif
  -- Out of transaction:
  case mine of 
        Seq.EmptyR -> C.liftIO$ stealLoop
        (xs  Seq.:> x) -> 
	    do if debugme 
	        then C.liftIO$ putStrLn$ " ===> Popped off work for <"++ 
		                 show myid ++ "> remaining " ++ show (Seq.length xs)
	        else return()
	       return (Just x)

--  return x


-----------------------------------------------------------------------------
