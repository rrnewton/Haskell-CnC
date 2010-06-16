

-- To be included in other files:
-----------------------------------------------------------------------------

push :: HotVar [a] -> a -> IO ()
pop :: HotVar [a] -> IO (Maybe a)

push v a = hotVarTransaction $ do xs <- readHotVarRaw v; writeHotVarRaw v (a:xs)

pop v = hotVarTransaction $ do 
  xs <- readHotVarRaw v
  case xs of
    [] -> return Nothing
    x:xs' -> do
      writeHotVarRaw v xs'
      return (Just x)
-----------------------------------------------------------------------------
