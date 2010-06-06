
-- Pieces that are common to version 5 and 6
------------------------------------------------------------

type TagCol a   = (IORef (Set.Set a), IORef [Step a])
type ItemCol a b = MutableMap a b

-- Here the hidden state keeps track of a pointer to the work-sharing
-- stack used for this graph.
type StepCode a = (S.StateT (HiddenState5) IO a)

-- In this version we need to thread the state through the graph code as well:
type GraphCode a = StepCode a

-- Individual threads have numeric IDs.
-- Here the hidden state keeps four things:
--   (1) the stack used for this graph
--   (2) the number of workers for this graph
--   (3) the "make worker" function to spawn new threads (given ID as input)
--   (4) the set of "mortal threads"
--   (5) the ID of the current thread
newtype HiddenState5 = 
    HiddenState5 (HotVar [StepCode ()], 
		  HotVar Int, 
		  Int -> IO (), 
		  HotVar (Set.Set ThreadId),
		  Int 
		 )
  deriving Show

instance Show (Int -> IO ()) where
  show _ = "<int to IO unit function>"

atomicIncr :: Num n => HotVar n -> IO ()
atomicDecr :: Num n => HotVar n -> IO ()
atomicIncr x = modifyHotVar_ x (+ 1)
atomicDecr x = modifyHotVar_ x (\n -> n-1)


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

issueReplacement = 
  do (HiddenState5 (stack, numworkers, makeworker, _, id)) <- S.get
     STEPLIFT atomicIncr numworkers
     -- If this were CPS then we would just give our
     -- continuation to the forked thread.  Alas, no.
     STEPLIFT forkIO (makeworker id)

-- FIXME: [2010.05.05] I believe this has a problem.
-- tryTakeMVar can fail spuriously if there's a collision with another
-- thread reading the mvar.  This is a sense in which mvars CANNOT
-- mimick IVars (at least ivars with the ability to test for presence
-- -- a monotonic test!)

-- This should only be a performance bug (forks an extra task for no
-- good reason).  When the code below falls back to readMVar that
-- should succeed.

-- Grab an mvar, but bring in reinforcements if we need to go down:
grabWithBackup hook mvar =
    do hopeful <- STEPLIFT tryTakeMVar mvar
       case hopeful of 
         Just v  -> do STEPLIFT putMVar mvar v -- put it back where we found it
		       return v
	 -- Otherwise, no data.  If we block our own thread, we need to issue a replacement.
         Nothing -> do issueReplacement
		       
		       STEPLIFT hook -- Any IO action can go here...
#ifdef DEBUG_HASKELL_CNC
		       STEPLIFT putStrLn $ " >>> Blocked on "++ show tag ++"||| "
#endif
		       STEPLIFT readMVar mvar


ver5_6_core_get hook (col) tag = 
    do (HiddenState5 (stack, numworkers, makeworker, _, _)) <- S.get
       mvar    <- STEPLIFT assureMvar col tag 
       grabWithBackup hook mvar


ver5_6_core_finalize :: Chan Int -> StepCode b -> (Int -> StepCode ()) -> Bool -> GraphCode b
ver5_6_core_finalize joiner finalAction worker shouldWait = 
    do (HiddenState5 (stack, numworkers, _, mortal, id)) <- S.get

       -- Here we install the makeworker funciton in the monad state:
       let mkwrkr id = do S.runStateT (worker id) (state2 id); return ()
           state2 id = HiddenState5 (stack, numworkers, mkwrkr, mortal, id)
       -- Write it back for the "finalAction" below:
       S.put (state2 id)

       GRAPHLIFT modifyHotVar_ numworkers (+ numCapabilities)
       -- Fork one worker per thread:
#ifdef DEBUG_HASKELL_CNC
       GRAPHLIFT putStrLn$ "Forking "++ show numCapabilities ++" threads"
#endif 
       GRAPHLIFT forM_ [0..numCapabilities-1] (\n -> forkIO (mkwrkr n)) 
       --GRAPHLIFT forM_ [0..numCapabilities-1] (\n -> forkOnIO n (mkwrkr n)) 

       -- This waits for quiescense:
       let waitloop = do num <- readHotVar numworkers
	                 if num == 0
			  then return () 
			  else do 
#ifdef DEBUG_HASKELL_CNC
			          putStrLn ("=== Waiting on workers: "++ show num ++" left")
#endif
				  readChan joiner
				  atomicDecr numworkers
				  waitloop
       if shouldWait then GRAPHLIFT waitloop else return ()
       finalAction


putt = proto_putt
	(\ steps tag -> 
	   do (HiddenState5 (stack, _, _, _, _)) <- S.get
              foldM (\ () step -> STEPLIFT push stack (step tag))
                       () steps)

runGraph x = unsafePerformIO (runState x)
runState x =
    do hv  <- newHotVar []
       hv2 <- newHotVar 0
       hv3 <- newHotVar Set.empty
       let msg = "Intel.Cnc"++ show CNC_SCHEDULER ++" internal error: makeworker thunk used before initalized"
       (a,_) <- S.runStateT x (HiddenState5 (hv,hv2, error msg, hv3, -1))
       return a

------------------------------------------------------------
-- Experimental:  Free floating items:

type Item = MVar
newItem  = STEPLIFT newEmptyMVar
readItem = grabWithBackup (return ())
putItem mv x = 
  do b <- STEPLIFT tryPutMVar mv x
     if b then return ()
	  else error "Violation of single assignment rule; second put on Item!"
