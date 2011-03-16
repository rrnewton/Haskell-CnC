
import Data.Int
import System.Environment
import GHC.Conc

--import Intel.HCilk.HCilk_Sparks
--import Intel.HCilk.HCilk_Threads
import Intel.HCilk

----------------------------------------------------------------------------------------------------
parfib :: Int64 -> HCilk Int64
parfib n | n < 2 = return 1 
parfib n = 
  do --lift$ putStrLn ("parfib " ++ show n)
     xf <- spawn$ parfib (n-1)
     y  <- parfib (n-2)  
     x  <- sync xf
     return (x+y)


main = do [n] <- getArgs
	  x <- runCilk$ parfib (read n)
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

