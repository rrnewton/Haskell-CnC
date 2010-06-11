{-
 - Intel Concurrent Collections for Haskell
 - Copyright (c) 2010, Intel Corporation.
 -
 - This program is free software; you can redistribute it and/or modify it
 - under the terms and conditions of the GNU Lesser General Public License,
 - version 2.1, as published by the Free Software Foundation.
 -
 - This program is distributed in the hope it will be useful, but WITHOUT
 - ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 - FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 - more details.
 -
 - You should have received a copy of the GNU Lesser General Public License along with
 - this program; if not, write to the Free Software Foundation, Inc., 
 - 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
 -
 -}

-- Author: Ryan Newton 

-- This benchmark models a workload that has alternating parallel and serial phases.
-- It is similar to embarassingly_par.hs

-- Incidentally, this example also demonstrates how to
-- /programattically/ construct a CnC graph rather than describing it
-- directly.

import GHC.Conc
import Debug.Trace
import Control.Monad
import System.Environment

import qualified  Control.Monad.State.Strict as S 

#include <haskell_cnc.h> 



-- Compute sum_n(1/n)
work :: Int -> Int -> Double -> Double
work offset 0 n = n
work offset (!i) (!n) = work offset (i-1) (n + 1 / fromIntegral (i+offset))


runit total = runGraph graph `pseq` return ()
 where
  oneshare = total `quot` numCapabilities
  workstep items jid =
     do tid <- stepUnsafeIO myThreadId 
	stepPutStr ("  "++show tid++" job "++show jid++":  About to do big work ("++ show oneshare ++" iterations)...\n")
        let res = work (oneshare * jid) oneshare 0.0
	stepPutStr ("  "++show tid++"   job "++show jid++":  done with work (result "++ show res ++"), putting item...\n")
        put items jid res

  barrier num items tags _ =
   do sum <- foldM (\acc i -> do x <- get items i; return (acc + x)) 0 [0..num-1]
      stepPutStr$ "     Sum  of  "++ show num ++" items at barrier: " ++ show sum ++"\n\n"
      putt tags 0 -- Put a single tag for the next stage of the pipeline.

  chain length kickoff =
   do wideitems <- newItemCol
      narritems <- newItemCol
      widetags <- newTagCol
      narrtags <- newTagCol      

      -- A single tag kicks off the chain:
      prescribe kickoff (\_ -> forM_ [0 .. numCapabilities-1] (putt widetags))

      -- The wide phase:
      prescribe widetags (workstep wideitems)

      -- Barrier:
      prescribe kickoff (barrier numCapabilities wideitems narrtags)

      -- The narrow phase:
      prescribe narrtags (workstep narritems)      
 
      if length > 1 
       then do nextchain <- newTagCol     
	       prescribe narrtags (barrier 1 narritems nextchain)
	       chain (length-1) nextchain
       else return narritems

  graph = 
   do initial <- newTagCol
      finalItems <- chain 4 initial
      
      initialize $ 
	do stepPutStr$ "Begin initialize.\n"
	   putt initial 0
	   stepPutStr "Done initializing.\n"
      finalize $ 
        do stepPutStr "About to block on final output:\n"
	   x <- get finalItems 0 
	   stepPutStr$ "Final Output: " ++ show x ++"\n"


main = do args <- getArgs 
	  loop args
  where 
    loop args = 
       case args of 
	   []  -> runit $ 20*1000*1000
	   [n] -> runit $ let num = read n in 
		          -- Here's a bit of a hack, if the input is inexact treat it as an exponent.  Otherwise as a plain scalar.
			  if num == fromIntegral (round num)
                          then round num
		          else round (10 ** read n)
