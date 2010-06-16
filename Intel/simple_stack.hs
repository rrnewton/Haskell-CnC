

-- To be included in other files:
-----------------------------------------------------------------------------

data Sched = Sched 
    { workpool :: HotVar [StepCode ()],
      myid :: Int
    }
  deriving Show

defaultState = do
  pool <- newHotVar []
  return$ Sched { workpool=pool, myid = -999 }

-----------------------------------------------------------------------------

--pushWork :: HotVar [a] -> a -> StepCode ()
--popWork  :: HotVar [a] -> R.ReaderT Sched IO (Maybe a)

pushWork :: StepCode () -> StepCode ()
popWork  :: R.ReaderT Sched IO (Maybe (StepCode ()))

pushWork a =
 do Sched { workpool } <- R.ask 
    C.liftIO$  hotVarTransaction $ 
     do xs <- readHotVarRaw workpool
        writeHotVarRaw workpool (a:xs)

popWork  = 
 do Sched { workpool } <- R.ask 
    R.lift $ 
     hotVarTransaction $ 
     do xs <- readHotVarRaw workpool
        case xs of
         [] -> return Nothing
         x:xs' -> do
           writeHotVarRaw workpool xs'
           return (Just x)
-----------------------------------------------------------------------------
