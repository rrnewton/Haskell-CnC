-----------------------------------------------------------------------------
-- UNFINISHED: Put work stealing DEQUES here.
-----------------------------------------------------------------------------


data Sched = Sched 
    { workpool :: Array.Array Int (HotVar (Seq.Seq (StepCode ()))),
      myid :: Int
     }
  deriving Show


defaultState = do
  dvars <- forM [1..numCapabilities] $ \_ -> newHotVar Seq.empty
  let deques = Array.listArray (0,numCapabilities-1) dvars
  return$ Sched { workpool= deques, 
		  myid = -999
		}


#if 0
pushWork :: HotVar [a] -> a -> StepCode ()
popWork  :: HotVar [a] -> R.ReaderT Sched IO (Maybe a)

pushWork v a = 
   C.liftIO$  hotVarTransaction $ 
   do xs <- readHotVarRaw v
      writeHotVarRaw v (a:xs)

popWork v = 
  R.lift $ 
  hotVarTransaction $ 
  do xs <- readHotVarRaw v
     case xs of
       [] -> return Nothing
       x:xs' -> do
         writeHotVarRaw v xs'
         return (Just x)

#else

-----------------------------------------------------------------------------
-- To be included in other files:
-----------------------------------------------------------------------------

pushWork :: HotVar [a] -> a -> StepCode ()
popWork  :: HotVar [a] -> R.ReaderT Sched IO (Maybe a)

pushWork v a =
  -- First get the state, including array of deques
  do Sched { myid, deques }  <- R.ask
     --let mydeque = dequeues Array.! myid
     -- Add to my dequeue
     hotVarTransaction $ 
      do xs <- readHotVarRaw v; 
	 writeHotVarRaw v (a:xs)

popWork v = do 
  -- First get the state, including array of deques
  Sched _ _ _  <- R.ask
  -- Try to get work from our local dequeue.
  let mydeque = dequeues Array.! myid  

  -- If not, then pick a victim and steal.
   hotVarTransaction $ do 
  xs <- readHotVarRaw v
  case xs of
    [] -> return Nothing
    x:xs' -> do
      writeHotVarRaw v xs'
      return (Just x)

  -- After every failed steal, check to see if we should terminate.

#endif

-----------------------------------------------------------------------------
