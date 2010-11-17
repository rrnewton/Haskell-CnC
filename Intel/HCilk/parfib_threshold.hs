
module Main(main) where

import System.Environment (getArgs)
--import Control.Parallel
import GHC.Conc
import Data.Int

type FibType = Int64

main = do [arg1,arg2] <- getArgs
          let
            n = read arg1 :: FibType  -- input for nfib
            t = read arg2 :: FibType  -- threshold
            res = parfib n t
          putStrLn ("parfib " ++ show n ++ " = " ++ show res)

-- parallel version of the code with thresholding
parfib :: FibType -> FibType -> FibType
parfib n t | n <= t = nfib n
           | otherwise = n1 `par` (n2 `pseq` n1 + n2 + 1)
	                 where n1 = parfib (n-1) t
			       n2 = parfib (n-2) t

-- sequential version of the code
nfib :: FibType -> FibType
nfib 0 = 1
nfib 1 = 1
nfib x = nfib (x-2) + nfib (x-1) + 1

