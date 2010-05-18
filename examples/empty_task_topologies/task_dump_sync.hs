
-- Dump a bunch of empty tasks out at once.  Synchronize on the completion of each of them (via item gets).

#include <haskell_cnc.h>

task_dump_sync n = runGraph $ 
   do items :: ItemCol Int () <- newItemCol
      tags <- newTagCol

      prescribe tags (\t -> put items t ())
      initialize $ do for_ 0 n $ putt tags
      finalize   $ do for_ 0 n $ get items

main = do args <- System.getArgs 
	  putStrLn $ show $
	   case args of 
	     []  -> task_dump_sync 1000 
	     [n] -> task_dump_sync (read n)

