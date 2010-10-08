import System.Environment
import Control.Monad
-- import Control.DeepSeq

-- Ported by Simon Marlow

-- [2010.10.07] Ryan Newton: Changed it to use the newer, more dynamic API (forkStep)

#include <haskell_cnc.h>

dynAPI = True

nqueens nq = do

  results <- newItemCol
  let _ = results :: ItemCol [Int] ()

  tags <- newTagCol

  let
    threshold =3

    step :: (Int, [Int]) -> StepCode ()
    step (n,b)
       | n >= threshold = 
          sequence_ [ put results b () | b <- iterate gen [b] !! (nq - n) ]
       | otherwise =
          forM_ (gen [b]) $ \b -> 
	    if dynAPI then forkStep (step (n+1,b))
	              else putt tags (n+1, b)

  unless dynAPI $ prescribe tags step

  initialize $ 
    if dynAPI then forkStep$ step (0,[])
              else putt tags (0,[])
  finalize $ itemsToList results

 where
    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: [[Int]] -> [[Int]]
    gen bs = [ (q:b) | b <- bs, q <- [1..nq], safe q 1 b ]


main = do
  args <- fmap (fmap read) getArgs
  let n = case args of 
	    [n] -> n
	    []  -> 10 -- should yield 724
  print (length (runGraph (nqueens n)))
