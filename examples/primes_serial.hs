
import System

-- #include <haskell_cnc.h>

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = (prmlp 3 == n)
    where prmlp :: Int -> Int
  	  prmlp i = if (rem n i) == 0
 		    then i else prmlp (i + 2)

-- Test the serial function:
serial n = serlp 3 1
   where serlp :: Int -> Int -> Int
	 serlp i c | i >= n    = c
  	 serlp i c | isPrime i = serlp (i+2) (c+1)
	 serlp i c             = serlp (i+2) c

main = do args <- System.getArgs 
	  let run n =
	        do x <- return $ serial n
		   putStrLn (show x)
	  case args of 
	   []  -> run 1000 -- Should output 168
	   [n] -> run (read n)
