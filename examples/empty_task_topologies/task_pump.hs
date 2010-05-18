

-- Pump out a window of tasks, and then sync on it.  Wash, rinse, repeat.

-- Requires a scheduler that supports immediate execution during initialize phase.

#include <haskell_cnc.h>

window = 32
--window = 4 * numCapabilities

task_pump n = runGraph $ 
   do items :: ItemCol Int () <- newItemCol
      tags <- newTagCol

      prescribe tags (\t -> put items t ())
      initialize $ 
         do for_ 0 (n `quot` window) $ \i -> 
	       do for_ 0 window $ \t -> putt tags (t + i*window)
		  for_ 0 window $ \t -> get items (t + i*window)
      finalize   $ 
         do get items (n-1)

main = do args <- System.getArgs 
	  putStrLn $ show $
	   case args of 
	     []  -> task_pump 1000 
	     [n] -> task_pump (read n)

