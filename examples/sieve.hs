{-# LANGUAGE ExistentialQuantification
   , ScopedTypeVariables
   , BangPatterns
   , NamedFieldPuns 
   , RecordWildCards
   , FlexibleInstances
   , DeriveDataTypeable
  #-}

-- Requires package data-ordlist
import Data.List.Ordered (minus,union)
import Data.List hiding (union)
import Data.Time.Clock -- Not in 6.10
import System

import qualified NumberTheory.Sieve.ONeill as ONeill

import qualified Data.Numbers.Primes as Primes

import Intel.Cnc3
import Control.Monad


--type PrimeNum = Integer
type PrimeNum = Int


-- For reference:  
--  10^5th prime: 1299721
--  10^6th prime: 15485867
--  10^7th prime: 179424691 


----------------------------------------------------------------------------------------------------

primes1 :: [PrimeNum]
primes1 = sieve [2..]
 where 
  sieve (p : xs) = p : sieve [ x | x <- xs, x `mod` p > 0]

-- This one is much slower, 3.23 seconds for 10^4'th prime
----------------------------------------------------------------------------------------------------


primes2 :: [PrimeNum]
primes2 = 2: 3: sieve [] (tail primes2) 3
   where
    sieve fs (p:ps) x = [x+2,x+4..q-2] `minus` foldl union [] mults
                        ++ sieve fs' ps q
     where
      q      = p*p
      mults  = [[y+s,y+2*s..q] | (s,y) <- fs]
      fs'    = (2*p,q) : zip (map fst fs) (map last mults)

-- This version on my laptop gives me the 10^5 prime in 0.6 seconds. 10^6 in 15.3s.
----------------------------------------------------------------------------------------------------

primes3 :: [PrimeNum]
primes3 = ONeill.primes

-- Package NumberSieves.  Gives 10^5 in 0.1 seconds.  10^6 in 1.57.
-- 10^7 in 25.9 seconds on my laptop with firefox eating cpu and
-- everything.  Jon Harrop reported 10^7 in 16 seconds with F# version below.
-- He says it was parallelizable.... but I don't think he posted the parallelized code.
-- http://fsharpnews.blogspot.com/2010/02/sieve-of-eratosthenes.html
--
-- He's also using ints... not integers.  
-- Ints on this version bring us to 10^6 in 1.25s... 10^7 in 18.125.
-- The versions not that complex but includes the whole priority queue implementation.
-- 
---------------------------------------------------------------------------------------------------

primes4 :: [Integer]
primes4 = Primes.primes

-- Package "primes".  This version only does arbitrary precision.
-- 10^5 0.28s,  10^6 7.73s -- not gonig to bother ith 10^7
----------------------------------------------------------------------------------------------------

-- The C version included in the "haskell-primes" zip takes 2.5
-- seconds for 10^7 (gcc -O3).  That's a big gap.  It's also SHORT.

-- Well what about just doing that imperative version in Haskell:

-- This version uses a bitvector...

--unsigned int MAXN, P1, P2, P3;
--unsigned int *sieve;

--get b = do n <- sieve[(b)>>5]
--	   return ((n >> (b & 31)) & 1)

    -- for (k = 1; k <= P3; k++)
    --   if (GET(k)==0)
    -- 	   for(j=2*k+1, i=2*k*(k+1); i<P2; i+=j)
    -- 	      sieve[i>>5] |= 1 << (i&31);


--int isprime(int p) { return p==2 || (p>2 && (p&1)==1 && (GET((p-1)>>1)==0)); }
isprime p = p==2 || False

-- int main(int argc, const char** argv)
-- {
--     int i, n;
--     MAXN = atoi(argv[1]);
--     P1 = (MAXN + 63)/64;
--     P2 = (MAXN + 1)/2;
--     P3 = (ceil(sqrt(MAXN)) + 1) / 2;
--     sieve = (unsigned int*) calloc(P1,sizeof(unsigned int));
--     make();
--     for (n = 0, i = 0; i < P2; i++)
--         if (GET(i)==0) n=i;
--     if (n == 0) n=2;
--     else n=2*n+1;
--     printf("The last prime before %d is %d.\n", MAXN, n);
--     return 0;
-- }


-- Btw, version *cheats* by having an upper bound on how high it will
-- look.  It doesn't give you the Nth prime, but the last prime before
-- a bound.  Different problem.


----------------------------------------------------------------------------------------------------


-- It would be nice if we could do some kind of effective 

--cncsieve :: Int -> GraphCode PrimeNum
--cncsieve n = 

-- loop last_items = 
--   do items :: ItemCol PrimeNum Bool <- newItemCol

--      initialize $
--        cncFor 2 n $ \i -> 
--          do 

--      return 99



--let tagsBatch = sequence $ take cpus $ repeat newTagCol
-- Generate a graph for a number of cores:
-- This actually generates an infinite graph.

     -- We could reduce these doubleton tags to singleton if we
     -- instead dynamically created levels of the graph, with each
     -- level specialized to a certain 'p'.  The confusing thing is that they would only be 

     -- Really, ranges should be anything SYMBOLICALLY DESCRIBABLE


     -- Here's a sketch, this version of reduce operates on a whole
     -- tag collection.  The whole tag collection must go empty for it
     -- to be done?  How do you know its empty?  Well, maybe this
     -- needs to be a tag collection that does not get added to after startup?

-- Reduce could work over a whole tag collection or over a single "tag range".
reduce :: (t -> a -> StepCode a) -> a -> TagCol t -> StepCode a
reduce = undefined

-- This "caps off" a tag collection, saying that nothing more will
-- come.  It only makes sense if there is a serial producer, OR if a
-- previous reduction guarantees production is finished.
putt_finished :: TagCol a -> StepCode ()
putt_finished = undefined
-- Could this be implemented by putting an item?


graphInStep :: GraphCode a -> StepCode a
graphInStep = undefined


cncsieve :: Int -> GraphCode PrimeNum
cncsieve n = 
  do initial_tags :: TagCol PrimeNum       <- newTagCol
     lvlfactors   :: ItemCol Int PrimeNum  <- newItemCol
     final        :: ItemCol Int PrimeNum  <- newItemCol
     -- final is a silly one-element collection for the final result.

     let do_round round tags = do 
	  -- Dynamically extend the graph for the next level of filtration:
          nexttags <- graphInStep newTagCol
	  -- Do a parallel reduce over all tags:
          next <- reduce_on (round+1) lvlfactors tags
		   n -- initial acc, effectively an upper bound
                   (\ p i acc -> 
                     if not (i `mod` p == 0)
		      then do putt nexttags i
		              return (min i acc)
		      else return acc)

          -- After we've finished our reduce for this round, we can
          -- set the item to unleash the next round:
	  put lvlfactors (round+1) next

	  if next*next < n -- Keep going?
	   then do_round (round+1) nexttags
	   -- Otherwise do one last reduce to get the final prime number:
	   else do lastprime <- reduce_on (round+1) lvlfactors nexttags n $ 
				 \ _ i acc -> return (max i acc)
		   put final 0 lastprime		  

     initialize$ do_round 2 initial_tags
     finalize  $ get final 0 


-- We can also just predicate the reduce being complete on an item.
-- It could use the item only for synchronization...  Or for
-- initializing the accumulator as follows:
reduce_on :: itag -> ItemCol itag val -> TagCol acc -> acc -> 
	     (val -> acc -> acc -> StepCode acc) -> StepCode acc
reduce_on = undefined


--cncsieve :: Int -> Int -> GraphCode PrimeNum
--cncsieve cpus n = 


----------------------------------------------------------------------------------------------------


--index = 1 * 1000 * 1000
--index = 100 * 1000
index = 10 * 1000
main = do args <- getArgs
	  let index = case args of [] -> 100 * 1000 
				   [n] -> 10 ^ (read n)
          putStrLn$ "Getting prime number " ++ show index
          strt <- getCurrentTime

	  putStrLn$ "  Got it! " ++ show (primes2 !! index)

          end  <- getCurrentTime
          let diff = (diffUTCTime end strt)
          putStrLn$ "  in time " ++ show diff



{-

let primes =
    let a = ResizeArray[2]

    let grow() =
      let p0 = a.[a.Count-1]+1
      let b = Array.create p0 true

      for di in a do
        let rec loop i =
          if i<b.Length then
            b.[i] <- false
            loop(i+di)

        let i0 = p0/di*di
        loop(if i0<p0 then i0+di-p0 else i0-p0)

      for i=0 to b.Length-1 do
        if b.[i] then a.Add(p0+i)

    fun n ->
      while n >= a.Count do
        grow()
      a.[n];;

val primes : (int -> int)

-}
