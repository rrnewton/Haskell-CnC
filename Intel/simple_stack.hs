

-- To be included in other files:
-----------------------------------------------------------------------------

pushWork :: HotVar [a] -> a -> IO ()
popWork :: HotVar [a] -> IO (Maybe a)

pushWork v a = hotVarTransaction $ do xs <- readHotVarRaw v; writeHotVarRaw v (a:xs)

popWork v = hotVarTransaction $ do 
  xs <- readHotVarRaw v
  case xs of
    [] -> return Nothing
    x:xs' -> do
      writeHotVarRaw v xs'
      return (Just x)
-----------------------------------------------------------------------------
