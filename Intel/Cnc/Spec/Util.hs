{-# LANGUAGE FlexibleInstances #-}

----------------------------------------------------------------------------------------------------
-- A miscellaneous utility file
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

module Intel.Cnc.Spec.Util where

import Text.PrettyPrint.HughesPJClass
import Control.Monad.State
import System.IO
import StringTable.Atom

-- Constant: indentation used across all code generators.
indent = 4

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

-- Simple pretty printing helpers:
vbraces d = lbrace $+$ d $+$ rbrace
textAtom = text . fromAtom
angles t = text "<" <+> t <+> text ">"
commspc = text ", "
pad t = space <> t <> space
hangbraces d1 n d2 = sep [d1, vbraces$ nest n d2]

struct   title body = (hangbraces (text "struct " <> title) indent body) <> semi
cppclass title body = (hangbraces (text "class "  <> title) indent body) <> semi

-- I am very lazy:
t = text

-- This seems useful:
collapseMaybe :: Maybe (Maybe t) -> Maybe t
collapseMaybe Nothing         = Nothing
collapseMaybe (Just Nothing)  = Nothing
collapseMaybe (Just (Just x)) = Just x


instance Pretty Atom where
  pPrint atom = text (show atom)


fst3 (a,_,_) = a
snd3 (_,b,_) = b
thd3 (_,_,c) = c


-- "Official" output from our process should be tagged in the following way:
--cnctag = ""
cnctag = "[CnC] "

hcnc_name = "hcnc"
--appname = "cnc"
