-- Author: Ryan Newton 

-- Embarassingly parallel.
-- If this doesn't get a speedup nothing will!

-- Note: This program is an example of a CnC Haskell program that
-- depends on "put" being strict.  If it were not the real work would
-- be deferred until after the parallel computation is finished!

import GHC.Conc
import Debug.Trace
import Control.Monad
import System.Environment
import Intel.CncUtil

import qualified  Control.Monad.State.Strict as S 

#include <haskell_cnc.h> 

-- Compute sum_n(1/n)
work :: Int -> Int -> Double -> Double
work offset 0 n = n
work offset (!i) (!n) = work offset (i-1) (n + 1 / fromIntegral (i+offset))

runit total = runGraph graph `pseq` return ()
 where
  oneshare = total `quot` numCapabilities
  mystep items jid =
     do 
#if CNC_VARIANT == 1
        let tid = -99
#elif CNC_SCHEDULER == 8
        tid <- S.lift$ myThreadId 
#else
        tid <- myThreadId 
#endif
	stepPutStr (show tid++" job "++show jid++":  About to do big work ("++ show oneshare ++" iterations)...\n")
        let res = work (oneshare * jid) oneshare 0.0
	--tid2 <- S.lift$ myThreadId 
	stepPutStr (show tid++"   job "++show jid++":  done with work (result "++ show res ++"), putting item...\n")
        put items jid res
  graph = 
   do items <- newItemCol
      tags  <- newTagCol
      cncPutStr$  "Running embarassingly parallel benchmark.  CnC Variant: "++ show cncVariant ++"\n"
      prescribe tags (mystep items)
      initialize $ 
	do stepPutStr$ "Begin initialize.  Splitting work into "++show numCapabilities++" pieces\n"
	   forM_ [0 .. numCapabilities-1] (putt tags) 
	   stepPutStr "Done initializing.\n"
      finalize $ 
        do stepPutStr "About to block on output:\n"
	   final <- 
	    foldM (\ acc i -> 
		    do stepPutStr$ "  Retrieving output "++ show i ++": "
		       n <- get items i
		       stepPutStr$ show n ++ "\n"
		       return (acc + n)) 
		  0.0 [0 .. numCapabilities-1]
	   stepPutStr$ "Final Output: " ++ show final ++"\n"


main = do args <- getArgs 
	  loop args
  where 
    loop args = 
       case args of 
	   []  -> runit $ 50*1000*1000
	   [n] -> runit $ round (10 ** read n)
	   [trials, n] -> doTrials (read trials) (loop [n])
