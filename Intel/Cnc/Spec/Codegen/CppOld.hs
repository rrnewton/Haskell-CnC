{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- QuasiQuotes

----------------------------------------------------------------------------------------------------
-- This is the code generator for the original (CnC/C++ 0.1-0.5) "context"-based C++ API.
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

-- This should eventually be based on some intermediate representation of C code.  For a
-- first cut, however, it is easier to just generate syntax directly.  (Especially with
-- the assistance of the pretty printing libary.)

module Intel.Cnc.Spec.Codegen.CppOld where

import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.CncGraph
--import Intel.Cnc.Spec.GatherGraph
import Intel.Cnc.Spec.Util 
import Control.Monad.State
import StringTable.Atom

import Text.PrettyPrint.HughesPJClass 
--import Intel.Cnc.Spec.QuasiString
--import Text.InterpolatedString.Perl6
--import Text.InterpolatedString.QQ

import Data.Maybe
import Data.Graph.Inductive hiding (empty)
import Debug.Trace

import qualified StringTable.AtomMap as AM
import qualified StringTable.AtomSet as AS


--------------------------------------------------------------------------------

emitCppOld :: StringBuilder m => CncSpec -> m ()
emitCppOld (spec @ CncSpec{..}) = do 

   -- First we produce the header (a quasiquoted multiline string):
   --------------------------------------------------------------------------------

   -- I love quasiquoting but for now [2010.07.23] the dependencies are complicated and it
   -- also makes the resulting binary 3X bigger (it shouldn't!?).
{-
   putS$ [$istr|    
#ifndef #{appname}_H_ALREADY_INCLUDED
#define #{appname}_H_ALREADY_INCLUDED

#include <cnc/cnc.h>
#include <cnc/debug.h>

// Forward declaration of the context class (also known as graph)
struct #{appname}_context;

// Next this generated file contains prototypes for each step implementation:
|] 
-}
   -- This almost looks nicer in emacs anyway:
   putS$ "\n// This code was GENERATED from a CnC specification, do not modify.\n\n"
   putS$ "#ifndef "++appname++"_H_ALREADY_INCLUDED\n"
   putS$ "#define "++appname++"_H_ALREADY_INCLUDED\n\n"

   putS  "#include <cnc/cnc.h>\n"
   putS  "#include <cnc/debug.h>\n"
   -- Ideally this would only be included IF tuple types are used... complicated right now.
   putS  "#include \"boost/tuple/tuple.hpp\"\n\n"

   putS  "// Forward declaration of the context class (also known as graph)\n"
   putS$ "struct "++appname++"_context;\n\n"

   putS  "// Next this generated file contains prototypes for each step implementation:\n"

   ------------------------------------------------------------
   -- Do the step prototypes:
   ------------------------------------------------------------
   -- Don't include builtins (e.g. "env")
   let stepls = filter (\ x -> not$ x `elem` builtinSteps) $
		AS.toList steps
       prescribers = map (getStepPrescriber spec) stepls
       tagtys = map (\ name -> case tags AM.! name of 
		                  Nothing -> error$ "Tag collection '"++ show name ++"' missing type, needed for C++ codegen!"
		                  Just ty -> ty)
		prescribers  
		
   forM_ (zip stepls tagtys) $ \ (stp,ty) ->
     do emitStep appname (fromAtom stp) ty 
        putS "\n\n"

   ------------------------------------------------------------
   -- Do the context class
   ------------------------------------------------------------   
   putS$ "\n\n// Finally, here is the definition for the context class:\n" 
   
   let contextname = t$ appname++"_context"
   putD$ struct (contextname <> t " : public CnC::context" <> angles (pad contextname)) $ 
     vcat $ 
     t "// Tag collections members:" : 
     ((flip map) (AM.toList tags) $ \ (tg,mty) -> 
       case mty of 
         Nothing -> error$ "CppOld Codegen: tag collection without type: "++ (fromAtom tg)
         Just ty -> text "CnC::tag_collection" <> angles (dType ty) <+> textAtom tg <> semi
     ) ++ 
     [space, t "// Item collections members:" ] ++
     ((flip map) (AM.toList items) $ \ (it,mty) -> 
       case mty of 
         Nothing -> error$ "CppOld Codegen: item collection without type: "++ (fromAtom it)
         Just (ty1,ty2) -> t "CnC::item_collection" <> 
                           angles (dType ty1 <>commspc<> dType ty2) <+> textAtom it <> semi
     ) ++ 
     [space, t "// The context class constructor: "] ++
     [hangbraces 
       (contextname <> parens empty <+> colon $$
	-- Initializer list:
	(nest 6 $ vcat $ 
	 t "// Initialize tag collections:" :
	 ((flip map) (AM.toList tags) $ \ (tg,Just ty) -> 
	   textAtom tg <> parens (t "this, false") <> commspc 

	 ) ++ 
	 t "// Initialize item collections:" :
	 (punctuate commspc $ 
	  (flip map) (AM.toList items) $ \ (it,Just (ty1,ty2)) -> 
  	   textAtom it <> parens (t "this")
	 )))
       indent 
       -- Body of the constructor
       (vcat $ 
         [-- Create objects for all step collections:
	  (vcat$ (flip map) stepls $ \ stp -> 
	   t "// " <> textAtom stp <> space <> t (step_obj$ fromAtom stp) <> t" = new " <> textAtom stp <> parens empty <> semi
	  ),
          -- Generate prescribe relations first [mandatory]:
	  (vcat $ (flip map) (zip stepls prescribers) $ \ (stp,tg) ->
          t"prescribe" <> parens (pad$ textAtom tg <>commspc<> textAtom stp <> parens empty)
          <> semi),
         -- Next, consume relations
	-- trace ("labEdges graph: "++ show (labEdges graph)) $
	(let egs = concat $ 
	           map (\ (a,b,_) -> 
			      case (lab graph a, lab graph b) of 
				 (Just (CGItems i), Just (CGSteps s)) -> [(i,s)]
				 _ -> []) $ 
		   labEdges graph in 
	 vcat $ (flip map) egs $ \ (a,b) ->
          t"// consume" <> parens (t (fromAtom b) <> commspc <> t (step_obj $ fromAtom a)) <> semi),

         -- Finally, produce relations:
	 t"// PRODUCE"
	     
        ])
       ]

   ------------------------------------------------------------
   -- Finish up
   ------------------------------------------------------------
   -- This is the "footer" for the document.
   putS$ "\n\n#endif // "++appname++"_H_ALREADY_INCLUDED\n"
   return ()


-- An annoying feature of our current API [2010.08.03] is that we
-- don't have explicit step_collection objects.  We separate object
-- creation from 

step_obj str = "obj_" ++ str 



--------------------------------------------------------------------------------
-- Produce the prototype for a single step.
emitStep appname name ty = putD$ 
  struct (t name)
	 (t "int execute(" <+> constRefType ty <+> t "tag," <+> 
	  t appname <> t "_context & c) const;")


constRefType ty = t "const" <+> dType ty <+> t "&"

dType ty = case ty of 
  TInt   -> t "int"
  TFloat -> t "float"
  TSym s -> textAtom s
  TPtr ty -> dType ty <> t "*"
  -- Here is the convention for representing tuples in C++.
  --TTuple [a,b]   -> t "Pair"   <> angles (hcat$ punctuate commspc (map dType [a,b]))
  --TTuple [a,b,c] -> t "Triple" <> angles (hcat$ punctuate commspc (map dType [a,b,c]))
  TTuple ls -> t "boost::tuple" <> angles (hcat$ punctuate commspc$ map dType ls)
  --TTuple ls -> error$ "CppOld codegen: Tuple types of length "++ show (length ls) ++" not standardized yet!"


