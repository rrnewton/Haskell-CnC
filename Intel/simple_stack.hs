

-- To be included in other files:
-----------------------------------------------------------------------------
type Var a = TVar a

push :: Var [a] -> a -> IO ()
push v a = atomically $ do xs <- readTVar v; writeTVar v (a:xs)

pop :: Var [a] -> IO (Maybe a)
pop v = atomically $ do 
  xs <- readTVar v
  case xs of
    [] -> return Nothing
    x:xs' -> do
      writeTVar v xs'
      return (Just x)
-----------------------------------------------------------------------------
