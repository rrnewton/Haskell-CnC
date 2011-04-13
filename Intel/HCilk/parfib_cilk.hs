
import Data.Int
import System.Environment
import GHC.Conc

import Intel.HCilk.HCilk_Sparks
--import Intel.HCilk.HCilk_Threads
-- import Intel.HCilk 

----------------------------------------------------------------------------------------------------
type FibType = Int64
--type FibType = Int

----------------------------------------------------------------------------------------------------
parfib0 :: FibType -> HCilk FibType
parfib0 n | n < 2 = return 1 
parfib0 n = 
  do --lift$ putStrLn ("parfib " ++ show n)
     xf <- spawn$ parfib0 (n-1)
     y  <- parfib0 (n-2)  
     return (get xf + y)
--     x  <- get xf
--     return (get x + y)

parfib1 :: FibType -> HCilk FibType
parfib1 n | n < 2 = return 1 
parfib1 n = 
  do --lift$ putStrLn ("parfib " ++ show n)
     xf <- spawn$ parfib1 (n-1)
     y  <- parfib1 (n-2)  
     syncAll
     return (get xf + y)


parfib2 :: FibType -> HCilk FibType
parfib2 n | n < 2 = return 1 
parfib2 n = 
  do --lift$ putStrLn ("parfib " ++ show n)
     xf <- spawnDupable$ parfib2 (n-1)
     y  <- parfib2 (n-2)  
--     syncAll
     return (get xf + y)


main = do [n] <- getArgs

          -- putStrLn "Running parfib without sync:"
	  -- x <- runCilk$ parfib0 (read n)

          -- putStrLn "Running parfib with sync:"
          -- x <- runCilk$ parfib1 (read n)

          putStrLn "Running parfib without sync, with spawnDupable:"
          x <- runCilk$ parfib2 (read n)

	  print x


----------------------------------------------------------------------------------------------------
-- Notes on running with HCilk_Threads
----------------------------------------------------------------------------------------------------
{-

Comparing this against a basic parfib.

Trying on a very small size.. fib(30)
It spends a lot of time in GC.

  real    0m7.681s
  user    0m11.050s
with 20% productivity, 1.8gb alloc.
Doing a heap residency profile..

As expected, with a run of fib(28) there are 185M thread state objects
resident.  Two orders of magnitude more than anything else in the heap.

     BLACKHOLE	          22768a
     FUN_2_0	          999192
     THUNK_0_1	          1035624
     TSO	          185218048
     FUN_1_0	          4511024
     MVAR_CLEAN	          9053088
     ghc-prim:GHC.Types.: 6764112
     THUNK_2_0	          5977536

 10% prudctivity..
  Parallel GC work balance: 2.94 (198979380 / 67725061, ideal 4)
  Alloc rate    644,868,466 bytes per MUT second
  153,031,920 bytes maximum residency (17 sample(s))


-}

----------------------------------------------------------------------------------------------------
-- Notes on running with HCilk_Sparks
----------------------------------------------------------------------------------------------------

{- 

Compared to HCilk_Threaded this versio is a success.  Rather than 7.6
seconds on fib(30) it takes 0.127 seconds.

Why don't I get a count of "sparks converted"?

     $ ./HCilk_Sparks 30 +RTS -N4 -s 
     1346269
	  414,948,984 bytes allocated in the heap
	   16,171,032 bytes copied during GC
	       66,328 bytes maximum residency (1 sample(s))
	       64,544 bytes maximum slop
		    3 MB total memory in use (0 MB lost due to fragmentation)

       Generation 0:   270 collections,     0 parallel,  0.04s,  0.05s elapsed
       Generation 1:     1 collections,     0 parallel,  0.00s,  0.00s elapsed

       Parallel GC work balance: nan (0 / 0, ideal 4)
       Alloc rate    2,305,272,133 bytes per MUT second
       Productivity  81.8% of total user, 144.6% of total elapsed

     real    0m0.127s
     user    0m0.220s
     sys     0m0.050s

  [The above must have been on wasp, a 4 core nehalem desktop.]

But it still takes 14.5 seconds for fib(40)... and looks like it's
using about 200% cpu.  So it's not in the ballpark of the native
parfib (6.6s for fib(42)), understandably.

If we do fib(42) it chugs along using 300% cpu.. .it takes 66 seconds.  Almost exactly a factor of ten slower.

Trying with unsafeDupable:
----------------------------------------
Well... that's interesting... on fib(42) it takes 16.5 seconds and 81% productivity.
That's 4X better than the non-dupable version.

-}



--------------------------------------------------------------------------------
-- Running again after changing sync/get and with GHC 7.0.1
--------------------------------------------------------------------------------
{- 
[2011.03]

  First, trying to reproduce the above... 
  On wasp it's using 400% cpu and negligable memory... 31% productivity and taking a long time.

  I'm using GHC 7.0.1.  I also changed the former "get" into a "sync" and made it non-monadic.


 wasp, real/user, WITHOUT syncAll, nonmonadic get, int64 type:
 -------------------------------------------------  

  1 thread:  fib(40)  28.629s 28.420s 94%   productivity, 54.9Gb allocated 
  2 thread:  fib(40)  14.992s 29.640s 48.3% productivity, 54.9Gb allocated 
  4 threads: fib(40)  7.922s  31.300s 24%   productivity, 54gb allocated

  4 threads: fib(42)  19.0s 15.6s  


 wasp, real/user, WITH syncAll, nonmonadic get, Int64 type:
 -------------------------------------------------  

  1 thread:  fib(40)  31.784s 31.530s 93.8% productivity 59GB
  2 thread:  fib(40)  16.484s 32.570s 
  4 threads: fib(40)  9.357s m36.820s 24%   productivity 59GB

     Running parfib with sync:
     165580141
       59,001,266,456 bytes allocated in the heap
	  123,249,872 bytes copied during GC
	      202,256 bytes maximum residency (20 sample(s))
	      322,680 bytes maximum slop
		    4 MB total memory in use (0 MB lost due to fragmentation)

       Generation 0: 31034 collections, 31033 parallel, 27.07s,  1.07s elapsed
       Generation 1:    20 collections,    20 parallel,  0.02s,  0.00s elapsed

       Parallel GC work balance: 3.42 (15219256 / 4452808, ideal 4)

			     MUT time (elapsed)       GC time  (elapsed)
       Task  0 (worker) :   23.62s    (  8.27s)      13.20s    (  0.54s)
       Task  1 (worker) :   36.82s    (  8.28s)       0.00s    (  0.00s)
       Task  2 (bound)  :   32.32s    (  8.28s)       4.50s    (  0.17s)
       Task  3 (worker) :   31.91s    (  8.28s)       4.91s    (  0.20s)
       Task  4 (worker) :   36.82s    (  8.28s)       0.00s    (  0.00s)
       Task  5 (worker) :   32.34s    (  8.28s)       4.48s    (  0.17s)

       SPARKS: 165580140 (373 converted, 165438162 pruned)

       INIT  time    0.00s  (  0.00s elapsed)
       MUT   time    9.73s  (  8.28s elapsed)
       GC    time   27.09s  (  1.08s elapsed)
       EXIT  time    0.00s  (  0.00s elapsed)
       Total time   36.82s  (  9.35s elapsed)

       %GC time      73.6%  (11.5% elapsed)

       Alloc rate    6,063,850,612 bytes per MUT second

       Productivity  26.4% of total user, 104.0% of total elapsed

     gc_alloc_block_sync: 25429
     whitehole_spin: 0
     gen[0].sync_large_objects: 89
     gen[1].sync_large_objects: 495


 wasp, real/user, without syncAll, with spawnDUPABLE, Int64 type:
 ----------------------------------------------------------------

  1 thread:  fib(40)  17.182s 17.060s
  2 thread:  fib(40)  9.623s  19.040s
  4 threads: fib(40)  5.216s  20.520s

SMT 8 threads: fib(40) 4.534s 33.370s

 NOTES: -qm helped a tiny bit... -qa didn't

  4 threads: fib(42)  12.8s 50.8s

And with Int type instead of Int64:

  4 threads: fib(42)  12.9s 51.2


 -}

