-- Author: Ryan Newton 

-- sched_tree.hs
-- A simple scheduler test that creates a tree of exponentially
-- expanding numbers of step executions (as though it were a binary
-- tree).  

import System.Environment

#define MEMOIZE
#define REPEAT_PUT_ALLOWED
#include <haskell_cnc.h>


-- We use lists of booleans as "tree indices":
type Tag = [Bool]

run limit = putStrLn (show v)
  where 
   v = runGraph $  
       do tags  :: TagCol  Tag     <- newTagCol
	  items :: ItemCol Tag Int <- newItemCol
	  prescribe tags 
	    (\ls -> do -- bin tree path as input
	               if length ls == limit
	                -- Trivial output: count the "right" steps in the tree path:
	                then put items ls (length $ Prelude.filter id ls)
	                else do putt tags (True:ls)
	                        putt tags (False:ls)
	    )
	  initialize $ 
	     do putt tags []

          -- Grab all the leaves of the binary tree:
	  let grabloop ls =
	       if length ls == limit
	       then get items ls
	       else do x <- grabloop (True:ls)
		       y <- grabloop (False:ls)
		       return (x+y)		 

 	  finalize $ grabloop []	 

main = do args <- getArgs 
	  case args of 
	    []  -> run 10
	    [s] -> run (read s)
