-- Author: Ryan Newton 

-- #include <haskell_cnc.h>

-- This demonstrates the normal (NOT #include) method of loading CnC:
import Intel.Cnc


-- Here's an odd little hello world where we communicate the two words
-- to a computational step which puts them together.

myStep items tag =
  do word1 <- get items "left"
     word2 <- get items "right"
     put items "result" (word1 ++ word2 ++ show tag)

cncGraph = 
  do tags  <- newTagCol
     items <- newItemCol
     prescribe tags (myStep items)
     initialize$ 
        do put items "left"  "Hello "
	   put items "right" "World "
	   putt tags 99
     finalize$ 
        do get items "result"

main = putStrLn (runGraph cncGraph)
