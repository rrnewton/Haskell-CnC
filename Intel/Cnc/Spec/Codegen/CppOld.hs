{-# LANGUAGE QuasiQuotes #-}

----------------------------------------------------------------------------------------------------
-- This is the code generator for the original (CnC/C++ 0.1-0.5) "context"-based C++ API.
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

-- This should eventually be based on some intermediate representation of C code.  For a
-- first cut, however, it is easier to just generate syntax directly.  (Especially with
-- the assistance of the pretty printing libary.)

module Intel.Cnc.Spec.Codegen.CppOld where

import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.GatherGraph
import Intel.Cnc.Spec.Util 
import Control.Monad.State
import StringTable.Atom

import Text.PrettyPrint.HughesPJClass
--import Intel.Cnc.Spec.QuasiString
--import Text.InterpolatedString.Perl6
import Text.InterpolatedString.QQ

indent = 4

emitCppOld :: StringBuilder m => CncSpec -> m ()
emitCppOld graph = do 
 let name = "mymod" 
 -- First we produce the header (a quasiquoted multiline string):
 putS$ [$istr|    
#ifndef #{name}_H_ALREADY_INCLUDED
#define #{name}_H_ALREADY_INCLUDED

#include <cnc/cnc.h>
#include <cnc/debug.h>

// Forward declaration of the context class (also known as graph)
struct #{name}_context;

|] 
 emitStep  "foo" TInt
 return ()

emitStep name ty = putD$ 
  -- text "struct " <> text (fromAtom atom) <+> lbrace $+$
  -- (nest indent (text "execute();")) $+$
  -- rbrace
  hangbraces (text "struct " <> text name) indent 
	     (text "int execute(" <+> constRefType ty <+> text "tag," <+> 
	      text name <> text "_context & c) const;")


constRefType ty = text "const" <+> dType ty <+> text "&"

dType ty = case ty of 
  TInt   -> text "int"
  TFloat -> text "float"
  TSym s -> text $ fromAtom s

  -- Here is the convention for representing tuples in C++.
  TTuple [a,b] -> text "PAIR"
  

vbraces d = lbrace $+$ d $+$ rbrace

hangbraces d1 n d2 = sep [d1, vbraces$ nest n d2]
  

--      putS$ "#ifndef "++name++"_H_ALREADY_INCLUDED"
--      putS$ "#define "++name++"_H_ALREADY_INCLUDED"
