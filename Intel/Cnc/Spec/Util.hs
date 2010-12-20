{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

----------------------------------------------------------------------------------------------------
-- A miscellaneous utility file used by the CnC Spec tool.
--
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

module Intel.Cnc.Spec.Util where

import Text.PrettyPrint.HughesPJClass
import Control.Monad.State
import Control.Exception as CE
import System.IO.Unsafe
import System.IO
import StringTable.Atom

import Data.List
import Data.Char
import GHC.Exts -- For IsString

import qualified Test.HUnit as HU
import Debug.Trace

-- String Builder
----------------------------------------------------------------------------------------------------
-- We abstract the process of creating strings so that we can make our
-- code generation more efficient when we feel like it (later).
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
  writeSB h m = let (s,a) = runSB m in  -- This appends the whole string... it shouldn't.
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
-- Simple pretty printing helpers, and C/C++ codegen helpers.
----------------------------------------------------------------------------------------------------
-- These operate on and produce Doc types:

-- Export this friendly shortcut:
pp x = pPrint x -- Eta expand, due to monomorphism restriction.
commacat ls = hcat (intersperse (text ", ") $ map pPrint ls)
commasep ls = sep (intersperse (text ", ") $ map pPrint ls)

escapeString = foldr showLitChar ""

vbraces d = lbrace $+$ d $+$ rbrace
textAtom = text . fromAtom
angles t = text "<" <+> t <+> text ">"
commspc = text ", "
pad t = space <> t <> space

-- Braces-delimited as in C/C++/Java code.
hangbraces d1 n d2 = sep [d1, vbraces$ nest n d2]

-- Create C++ structs/classes
struct   title body = (hangbraces (text "struct " <> title) indent body) <> semi
cppclass title body = (hangbraces (text "class "  <> title) indent body) <> semi

-- Shorthand: I am very lazy.
t  = text

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

map_but_last fn [] = []
map_but_last fn [h] = [h]
map_but_last fn (h:t) = fn h : map_but_last fn t


-- The probability of any of the below stuff being reusable is pretty low.
-- (It's very C++ specific. This whole *strategy* of private context generation is probably pretty C++ specific.)
-- But I thought I would begin to at least BEGIN to abstract the syntax-construction operations.

-- A few decorations on C++ types:
mkPtr d = d <> t"*"                      -- Make a type into a pointer
mkRef tyD = tyD <> t"&"                  -- Make a type into a reference
mkConstRef tyD = t"const" <+> mkRef tyD  -- Make a type const

-- The dot operator:
deref x y = x <> t"." <> y


-- Create a line of code encompassing assignment commands:
assignCast ty x y = ty <+> x <+> t"=" <+> parens ty <> y <> semi
assign x y =  toDoc x <+> t"=" <+> toDoc y <> semi

-- Function application:
app fn ls = toDoc fn <> (parens$ hcat$ intersperse (t", ")$ map toDoc ls)
thunkapp fn = app fn ([] :: [Doc])

-- Create a C++ constructor with initializers:
--constructor :: Int -> Int -> Int -> Int -> Int

constructor :: (SynChunk a, SynChunk a1) =>
	       a -> [a1]  -> [(Doc, Doc)] -> Doc -> Doc
constructor name args inits body = 
    hangbraces (app name args <+> colon $$ 
		nest 10 (vcat$ map_but_last (<>t", ")$ map (\ (a,b) -> a <> parens b) inits)) 
                indent body

-- Parameters to a function... abstracting this for, say, untyped languages?
param ty name = ty <+> name

dubquotes :: SynChunk a => a -> Doc
dubquotes d = (t"\"") <> toDoc d <> (t"\"")

-- This overloading just supports my laziness:
class SynChunk a where 
  toDoc :: a -> Doc
instance SynChunk String where 
  toDoc = text
instance SynChunk Doc where 
  toDoc = id
instance SynChunk Atom where 
  toDoc = text . fromAtom


-- Also, overloading the string constants themselves is nice:
instance IsString Doc where
    fromString s = text s
instance IsString Atom where
    fromString s = toAtom s

instance ToAtom Doc where
   toAtom d = toAtom$ render d

-- Constant: indentation used across all code generators.
indent = 4

--------------------------------------------------------------------------------
-- Testing and Assertion Utilities
--------------------------------------------------------------------------------

-- The standard Control.Exception.assert is turned off in optimize mode and does not assume Show.
-- This is a variant that assumes Show and is always on.
alwaysAssertEq :: (Eq a, Show a) => String -> a -> a -> a1 -> a1
alwaysAssertEq msg expect got  final = 
  if (expect == got)
  then final
  else unsafePerformIO$
       do putStrLn$ "\n!!! Assertion failed."
	  putStrLn$ msg
	  putStrLn$ "Expected:  " ++ show expect
	  putStrLn$ "Received:  " ++ show got
	  putStrLn$ ""
	  -- For the source location:
	  CE.assert (expect == got) 
   		    (return final)

-- HUnit convenience function (used by other modules):
-- There's a problem with quickcheck where it doesn't
-- newline-terminate the "Cases: N" report message.
--testCase str io = HU.TestLabel str $ HU.TestCase$ do putStrLn$ "\n *** Running unit test: "++str; io; putStrLn ""

-- Tag a little bit more verbose output to the tests:
testCase prefix str tst = HU.TestLabel lab (trace (tag++ lab) tst)
 where lab = if prefix == ""
             then str
	     else prefix ++ ": " ++ str
--       tag = " *** "
       tag = " [test] "

-- Likewise, identify the per-module sub-groups of tests
testSet name ls = 
    trace ("\n"++header ++"\n"++ spacer) (HU.TestList ls)
 where header = "Running tests for module " ++ show name 
       spacer = (take (length header) $ repeat '=')


--------------------------------------------------------------------------------
-- These should be moved to a "globals" file:
--------------------------------------------------------------------------------

-- "Official" output from our process should be tagged in the following way:
--cnctag = ""
cnctag = "[CnC] "

--hcnc_name = "hcnc"
hcnc_name = "cnc"

special_environment_name = "env"
