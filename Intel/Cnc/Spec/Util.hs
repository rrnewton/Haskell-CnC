{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------------------------------------
-- A miscellaneous utility file
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

module Intel.Cnc.Spec.Util where

import Text.PrettyPrint.HughesPJClass
import Control.Monad.State
import System.IO


-- String Builder
----------------------------------------------------------------------------------------------------
-- We abstract the process of creating strings so that we can make our
-- code generation more efficient when we feel like it.
--
-- I couldn't find a nice simple version of this in hackage or the
-- standard libraries so here we roll our own (trivial) StringBuilder class.


class Monad m => StringBuilder m where 
  putS  :: String -> m ()
  putD  :: Doc -> m () 
  runSB :: m a -> (String, a)
  writeSB :: Handle -> m a -> IO a

  -- Default inefficient implementation:
  putS s = putD (text s)
  putD d = putS (show d)
  writeSB h m = let (s,a) = runSB m in 
		do hPutStr h s
		   return a

type SimpleBuilder a = State [String] a

-- Here's something simple, not yet bothering with compact strings or file Handles, just
-- accumulating a list of strings.
--instance StringBuilder SimpleBuilder where 
instance StringBuilder (State [String]) where 
  putS s = modify (s:)
  runSB m = let (res,ls) = runState m [] 
	    in (concat$ reverse ls, res)
----------------------------------------------------------------------------------------------------
