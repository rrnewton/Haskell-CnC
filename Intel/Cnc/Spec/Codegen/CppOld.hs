{-# LANGUAGE QuasiQuotes, RecordWildCards #-}

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

import Data.Maybe

import qualified StringTable.AtomMap as AM
import qualified StringTable.AtomSet as AS

indent = 4


--------------------------------------------------------------------------------


emitCppOld :: StringBuilder m => CncSpec -> m ()
emitCppOld (spec @ CncSpec{..}) = do 

   -- First we produce the header (a quasiquoted multiline string):
   --------------------------------------------------------------------------------
   putS$ [$istr|    
#ifndef #{appname}_H_ALREADY_INCLUDED
#define #{appname}_H_ALREADY_INCLUDED

#include <cnc/cnc.h>
#include <cnc/debug.h>

// Forward declaration of the context class (also known as graph)
struct #{appname}_context;

// Next this generated file contains prototypes for each step implementation:
|] 
   ------------------------------------------------------------
   -- Do the step prototypes:
   ------------------------------------------------------------
   -- Don't include builtins (e.g. "env")
   let stepls = filter (\ x -> not$ x `elem` builtinSteps) $
		AS.toList steps
       prescribers = map (getStepPrescriber spec) stepls
       tagtys = map (\ name -> fromJust $ tags AM.! name) prescribers  
		
   forM_ (zip stepls tagtys) $ \ (stp,ty) ->
     emitStep (fromAtom stp) ty   

   ------------------------------------------------------------
   -- Do the context class
   ------------------------------------------------------------   
   putS$ "\n\n// Finally, here is the definition for the context class:\n" 
   
   putD$ struct (text "context") $ vcat $ 
     text "// Member fields for the tag and item collections: " : 
     ((flip map) (AM.toList tags) $ \ (t,mty) -> 
       case mty of 
         Nothing -> error$ "CppOld Codegen: tag collection without type: "++ (fromAtom t)
         Just ty -> text "CnC::tag_collection" <> angles (dType ty) <+> textAtom t <> semi
     ) ++ 
     ((flip map) (AM.toList items) $ \ (t,mty) -> 
       case mty of 
         Nothing -> error$ "CppOld Codegen: item collection without type: "++ (fromAtom t)
         Just (ty1,ty2) -> text "CnC::item_collection" <> 
                           angles (dType ty1 <>commspc<> dType ty2) <+> textAtom t <> semi
     ) ++ 
     [space,text "// The context class constructor: "] ++
     ((flip map) (zip stepls prescribers) $ \ (stp,tg) ->
      text "prescribe" <> parens (pad$ textAtom tg <>commspc<> textAtom stp <> parens empty)
         <> semi
     )

   ------------------------------------------------------------
   -- Finish up
   ------------------------------------------------------------
   -- This is the "footer" for the document.
   putS$ "\n\n#endif // "++appname++"_H_ALREADY_INCLUDED\n"
   return ()


--------------------------------------------------------------------------------
-- Produce the prototype for a single step.
emitStep name ty = putD$ 
  struct (text name)
	 (text "int execute(" <+> constRefType ty <+> text "tag," <+> 
	  text name <> text "_context & c) const;")
  <> semi


constRefType ty = text "const" <+> dType ty <+> text "&"

dType ty = case ty of 
  TInt   -> text "int"
  TFloat -> text "float"
  TSym s -> text $ fromAtom s

  -- Here is the convention for representing tuples in C++.
  TTuple [a,b] -> text "Pair" <> angles (dType a <> commspc <> dType b)

-- Simple pretty printing helpers:
vbraces d = lbrace $+$ d $+$ rbrace
hangbraces d1 n d2 = sep [d1, vbraces$ nest n d2]
struct title body = hangbraces (text "struct " <> title) indent body
textAtom = text . fromAtom
angles t = text "<" <+> t <+> text ">"
commspc = text ", "
pad t = space <> t <> space