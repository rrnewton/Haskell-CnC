

-- To be included in other files:
-----------------------------------------------------------------------------

push :: HotVar [a] -> a -> IO ()
#if HOTVAR == 3 
push v a = atomically $ do xs <- readTVar v; writeTVar v (a:xs)
#else
push v a = do xs <- readHotVar v; writeHotVar v (a:xs)
#endif

pop :: HotVar [a] -> IO (Maybe a)
#if HOTVAR == 3 
pop v = atomically $ do 
  xs <- readTVar v
#else
pop v = do 
  xs <- readHotVar v
#endif
  case xs of
    [] -> return Nothing
    x:xs' -> do
#if HOTVAR == 3 
      writeTVar v xs'
#else
      writeHotVar v xs'
#endif
      return (Just x)
-----------------------------------------------------------------------------
