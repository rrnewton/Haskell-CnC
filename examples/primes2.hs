
-- This version uses a item collection and does an explicit 'get' to
-- check primality for each number it tests.  Thus it does not require
-- quiescence-support.  But the Maps grow much larger and there's much
-- more MVar traffic.

-- Author: Ryan Newton

import System.Environment

-- #define MEMOIZE
#include <haskell_cnc.h>
----------------------------------------

-- First a naive serial test for primality:

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = (prmlp 3 == n)
    where prmlp :: Int -> Int
  	  prmlp i = if (rem n i) == 0
 		    then i else prmlp (i + 2)

----------------------------------------

-- Next, a CnC program that calls the serial test in parallel.

primes n = 
   do primes :: ItemCol Int Bool <- newItemCol
      tags <- newTagCol
      prescribe tags (\t -> put primes t (isPrime t))

      let loop i | i >= n = return ()
  	  loop i = do putt tags i 
	              loop (i+2)

      let loop2 i acc | i >= n = return acc
  	  loop2 i acc = do b <- get primes i
	                   loop2 (i+2) (if b then acc+1 else acc)

      initialize $
	do put primes 2 True
           loop 3

      finalize $ loop2 3 1

-- For reference, here's a sieve :
primels :: [Integer]
primels = 2 : Prelude.filter isPrime [3,5..]
     where
     isPrime n   = all (not . divides n) $ takeWhile (\p -> p*p <= n) primels
     divides n p = n `mod` p == 0


main = do args <- getArgs 
	  let run n =
	        do x <- return $ runGraph $ primes n
		   putStrLn (show x)
	  case args of 
	   []  -> run 1000 -- Should output 168
	   [n] -> run (read n)
