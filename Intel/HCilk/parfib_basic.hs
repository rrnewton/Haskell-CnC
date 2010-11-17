

import Data.Int
import System.Environment

--import Control.Concurrent
import GHC.Conc
--import Control.Parallel


parfib :: Int64 -> Int64
parfib n | n < 2 = 1 
parfib n = x `par` y `pseq` (x+y)
  where 
     x = parfib (n-1)
     y = parfib (n-2)  

main = do [n] <- getArgs
	  let x = parfib (read n)
	  print x

{- 

On wasp (3.33ghz core i7, 4 core, hyperthreading) -N4, fib(42):

  real    0m6.692s
  user    0m11.420s

With -N8 it uses a bunch more CPU, and highly variable CPU... it gets
pretty confused.... taking a long long time.  Ok, I killed it
after 3 minutes...
(Is idempotent work stealing biting here?)

But -N7 does better, but still not as well as -N4:

  real    0m8.763s
  user    0m18.120s
    
If I try the thresholded version... it takes 3.0 seconds serial.
It takes two seconds with a cutoff of 30, but STILL doesn't work the whole CPU.

   $ time ./parfib_threshold 42 30 +RTS -N4 -s
   real    0m2.073s
   user    0m6.430s
(And with threshold 20 it takes only one second... )


Well I take that back, if I run it for 44, it does work most of the CPU:
  user    0m10.600s

fib 44 -N7: 4.7s
fib 44 -N4: 2.87s
fib 44 -N3: 4.5s

Reducing cutoff to 10... same time.  Threshold of 20 does BETTER, 2.5s.
Pumping up to 35... 8.3s.



-}
