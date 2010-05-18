
-- Dump a bunch of empty tasks out at once.

#include <haskell_cnc.h>

task_dump n = runGraph $ 
   do items :: ItemCol Int () <- newItemCol
      tags <- newTagCol

      prescribe tags (\t -> return ())
      initialize $ do for_ 0 n $ putt tags
      -- itemsToList simply assures that we have a scheduler with
      -- quiescence_support:
      finalize   $ do itemsToList items

main = do args <- System.getArgs 
	  putStrLn $ show $
	   case args of 
	     []  -> task_dump 1000 
	     [n] -> task_dump (read n)

