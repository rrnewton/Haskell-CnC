

-- To be included in other files:
-----------------------------------------------------------------------------

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
