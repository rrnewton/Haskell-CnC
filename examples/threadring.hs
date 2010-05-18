-- Author: Ryan Newton 

import Control.Monad
import Data.Complex
import System.Environment

#include <haskell_cnc.h>

-- This simple microbenchmark is drawn from the "Great Language Shootout".
-- It passes token(s) around a ring.

-- This version uses a separate tag collection to represent each actor.

threadring hops agents 1 =
       do 
	  answer :: ItemCol Int Int <- newItemCol

          first:resttags <- mapM (\i -> do x <- newTagCol; return (i,x))
			         [1..agents]

          foldM (\ (i,last) (j,next) ->
		 do prescribe last
		     (\n -> if n == 0 
		            then put answer 0 i
		            else putt next (n-1))
		    return (j,next))
		first (resttags++[first])

	  initialize $ 
	     do putt (snd$ first) hops

 	  finalize $ get answer 0

-- Takes #hops #agents and #tokens in flight.
-- However tokens in flight > 1 is not yet implemented.
main =   
  do ls <- getArgs 
     let v = runGraph $ 
	      case Prelude.map read ls of 
	       []      -> threadring 17 503 1
       	       [h]     -> threadring h  503 1
       	       [h,a]   -> threadring h  a   1
       	       [h,a,t] -> threadring h  a   t	  
     putStrLn (show v)
