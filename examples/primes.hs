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


-- This file contains a simple example that tests numbers for primality in parallel.
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
   do primes :: ItemCol Int Int <- newItemCol
      tags <- newTagCol
      prescribe tags (\t -> if isPrime (t) 
		            then put primes t t
		            else return ())

      let loop i | i >= n = return ()
  	  loop i = do putt tags i 
	              loop (i+2)
      initialize $
	do put primes 2 2
           loop 3      

      finalize $ 
        do result <- itemsToList primes
	   return (length result)	       

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
	   [trials, n] -> doTrials (read trials) (run (read n))
