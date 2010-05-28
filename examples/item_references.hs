import Intel.Cnc5
import Control.Monad

-- This is an example of the mechanism proposed by Vivek & I for items independent of collections.

stepA out tag = 
  do i <- newItem
     putt out (tag, i)

stepB (tag, item) = 
  do stepPutStr (show tag ++ ": B writing item! \n")
     putItem item ("B wuz here "++show tag)
     
stepC (tag, item) = 
  do x <- readItem item
     stepPutStr (show tag ++ ": C read item! Contents: " ++ show x ++ "\n")

graph = 
  do tags1 <- newTagCol
     tags2 <- newTagCol

     prescribe tags1 (stepA tags2)
     prescribe tags2 stepB
     prescribe tags2 stepC

     initialize$ 
       do forM_ [1..10] (putt tags1)
          stepPutStr "  Initial tags put.\n"

     finalize$ 
       do stepPutStr "  Finalizing...\n"
 	  return "Done"
		  
main = 
  do putStrLn "Running graph..."
     putStrLn$ "Result: " ++ show (runGraph graph)
