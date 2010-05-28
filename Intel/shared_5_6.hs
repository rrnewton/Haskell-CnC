
-- Pieces that are common to version 5 and 6
------------------------------------------------------------

type TagCol a   = (IORef (Set a), IORef [Step a])
type ItemCol a b = MutableMap a b

-- Here the hidden state keeps track of a pointer to the work-sharing
-- stack used for this graph.
type StepCode a = (S.StateT (HiddenState5) IO a)

-- In this version we need to thread the state through the graph code as well:
type GraphCode a = StepCode a

-- Here the hidden state keeps four things:
--   (1) the stack used for this graph
--   (2) the number of workers for this graph
--   (3) the "make worker" function to spawn new threads
--   (4) the set of "mortal threads"
newtype HiddenState5 = HiddenState5 (HotVar [StepCode ()], HotVar Int, IO (), Set ThreadId)
  deriving Show

instance Show (IORef a) where 
  show ref = "<ioref>"
instance Show (IO a) where 
  show ref = "<io>"

atomicIncr x = atomicModifyIORef x (\n -> (n+1, ()))
atomicDecr x = atomicModifyIORef x (\n -> (n-1, ()))


-- This will be one hot IORef:
global_stack :: HotVar [StepCode ()]
global_stack = unsafePerformIO (newHotVar [])

global_numworkers :: IORef Int
global_numworkers = unsafePerformIO (newIORef 0)

-- A computation that forks a new worker thread:
global_makeworker :: IORef (IO ())
global_makeworker = unsafePerformIO$ newIORef (return ())


-- This is a bit silly, this emulates "thread local storage" to let
-- each worker thread know whether it is recursive (True) or "oneshot".
global_mortalthreads :: IORef (Set ThreadId)
global_mortalthreads = unsafePerformIO (newIORef Set.empty)


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



-- FIXME: [2010.05.05] I believe this has a problem.
-- tryTakeMVar can fail spuriously if there's a collision with another
-- thread reading the mvar.  This is a sense in which mvars CANNOT
-- mimick IVars (at least ivars with the ability to test for presence
-- -- a monotonic test!)

-- This should only be a performance bug (forks an extra task for no
-- good reason).  When the code below falls back to readMVar that
-- should succeed.

issueReplacement = 
  do STEPLIFT atomicIncr global_numworkers
     -- If this were CPS then we would just give our
     -- continuation to the forked thread.  Alas, no.
     makeworker <- STEPLIFT readIORef global_makeworker
     STEPLIFT forkIO makeworker

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
    do --(HiddenState5 (stack, numworkers, makeworker, _)) <- S.get
       mvar    <- STEPLIFT assureMvar col tag 
       grabWithBackup hook mvar

--ver5_6_core_finalize :: Chan a -> IO b -> IO () -> StepCode b
ver5_6_core_finalize :: Chan a -> StepCode b -> StepCode () -> GraphCode b
ver5_6_core_finalize joiner finalAction worker = 
    do --(HiddenState5 (stack, numworkers, makeworker, _)) <- S.get
       state <- S.get 
       let makeworker = do S.runStateT worker state; return ()
       S.lift$ writeIORef global_makeworker makeworker
       S.lift$ atomicModifyIORef global_numworkers (\n -> (n + numCapabilities, ()))
       -- Fork one worker per thread:
#ifdef DEBUG_HASKELL_CNC
       S.lift$ putStrLn$ "Forking "++ show numCapabilities ++" threads"
#endif
       S.lift$ mapM (\n -> forkIO makeworker) [0..numCapabilities-1]

       -- This waits for quiescense:
       let waitloop = do num <- readIORef global_numworkers
	                 if num == 0
			  then return () 
			  else do 
#ifdef DEBUG_HASKELL_CNC
			          putStrLn ("=== Waiting on workers: "++ show num ++" left")
#endif
				  readChan joiner
				  atomicDecr global_numworkers
				  waitloop
       S.lift$ waitloop
       finalAction


putt = proto_putt
	(\ steps tag -> 
	   do --(HiddenState5 (stack, numworkers, makeworker, _)) <- S.get
              foldM (\ () step -> S.lift$ push global_stack (step tag))
                       () steps)

runGraph x = unsafePerformIO (runState x)
runState x =
    do hv  <- newHotVar []
       hv2 <- newHotVar 0
       (a,_) <- S.runStateT x (HiddenState5 (hv,hv2, undefined, Set.empty))
       return a