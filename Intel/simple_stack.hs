

-- To be included in other files:
-----------------------------------------------------------------------------

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
-----------------------------------------------------------------------------
