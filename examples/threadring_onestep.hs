-- Author: Ryan Newton 

-- #define INCLUDEMETHOD
-- -- #define MEMOIZE
-- #include "CncPure.hs"
-- -- #include "Cnc.hs"

import System.Environment

#include <haskell_cnc.h>


-- This simple microbenchmark is drawn from the "Great Language Shootout".
-- It passes token(s) around a ring.

-- This implementation is for comparison with the C++ CnC implementation.a

threadring hops agents 1 =
       do tags   :: TagCol Int      <- newTagCol
	  items  :: ItemCol Int Int <- newItemCol
	  answer :: ItemCol Int Int <- newItemCol

	  prescribe tags 
	    (\n -> do let next = n+1 
	              let myid = n `mod` agents
	              --putStr$ "Actor executing, id "++ show myid ++"\n"
	              token <- get items n
	              --putStr$ " token "++ show token ++"\n"
	              if token == 0 
	               then put answer 0 myid
	               else do put items next (token-1)
	                       putt tags next )

	  initialize $ 
	     do put items 0 hops; putt tags 0
-- MAKE SURE THIS WORKS ALSO:
-- ([2009.08.12] It currently does with CncPure but not Cnc)
--	     do putt tags 0; put items 0 hops

	  finalize $ get answer 0
	  --finalize $ return () 


main =   
  do ls <- getArgs 
     let v = runGraph $ 
              case Prelude.map read ls of 
	       []      -> threadring 17 503 1
	       [h]     -> threadring h  503 1
	       [h,a]   -> threadring h  a   1
	       [h,a,t] -> threadring h  a   t
     putStrLn (show v)

{- 
NOTES:

[2009.08.12]

  With CncPure we get a horrible score: 24.17s for only 5M rounds
  (plus 1.1gb of mem)!  (I didn't even test 50M.)  Well we have the
  same problem CnC/C++ does, of course, we leak memory like crazy.

  Turning MEMOIZE off, it peaks out at 560mb of mem and takes 12.15s
  (for 5M).

  How much do we spend in GC? Running with "+RTS -sstderr":

    ____

  Switching to Cnc.hs ... hah, well first that gives me a stack space overflow.
  That should NOT happen.
  That's a good hint though... Need to strictify some folds probably.
  (Btw, if I just force the stack size up to 100mb, it runs... but
   very slowly.  It takes >23.5 minutes... and, well it stack
   overflowed the 100mb after using 1.3gb heap.  Egad.)


 -}
