

import System.Environment
import Data.Int
import Intel.CncUtil

#include "haskell_cnc.h"


run n = runGraph $  
       do tags  :: TagCol  Int       <- newTagCol
	  items :: ItemCol Int Int64 <- newItemCol
	  prescribe tags $ \i -> 
	     do x <- get items (i-1)
		y <- get items (i-2)
		put items i (x+y)
	  initialize $ 
	     do put items 0 0 		
		put items 1 1 
		for_ 2 (n+1) (putt tags)
		--forM_ [2..n] (putt tags)
		--forM_ (reverse [2..n]) (putt tags)
 	  finalize $ 
	     do get items n


main = do args <- getArgs 
	  putStrLn $ show $ 
	   case args of 
	    []  -> run 10
	    [s] -> run (read s)

