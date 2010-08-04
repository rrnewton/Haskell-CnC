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

----------------------------------------------------------------------------------------------------

-- An annoying feature of our current API [2010.08.03] is that we
-- don't have explicit step_collection objects.  We make sure to only
-- allocate one copy of the user's temporary Step() object and then
-- use its pointer as a key:
step_obj str = "m_" ++ str 
obj_ref a   = t"*" <> (t$ step_obj$ fromAtom a)

----------------------------------------------------------------------------------------------------

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
   -- Emit the step prototypes:
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
   -- Emit the main context class
   ------------------------------------------------------------   
   putS$ "\n\n// Here is the definition for the main application context:\n" 
   let maincontext = t$ appname++"_context"   

   putD$ cppclass (maincontext <> t " : public CnC::context" <> angles (pad maincontext)) $ 
     vcat $ 
    
     t "\n  private:" : 
     t "// Members to store instances of the users Step type:" : 
     ((flip map) stepls $ \ stp -> 
      textAtom stp <> t"* " <> (t$ step_obj$ fromAtom stp) <> semi) ++

     t "\n  public:" : 
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
       (maincontext <> parens empty <+> colon $$
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
	   (t$ step_obj$ fromAtom stp) <> t" = new " <> textAtom stp <> parens empty <> semi
	  ),
          -- Generate prescribe relations first [mandatory]:
	  (vcat $ (flip map) (zip stepls prescribers) $ \ (stp,tg) ->
           t"prescribe" <> parens (pad$ textAtom tg <>commspc<>  obj_ref stp)
           <> semi),

        t"\n#ifdef INVASIVE", 
         -- Next, consume relations:
	(let egs = concat $ 
	           map (\ (a,b,_) -> 
			      case (lab graph a, lab graph b) of 
				 (Just (CGItems i), Just (CGSteps s)) -> [(i,s)]
				 _ -> []) $ 
		   labEdges graph in 
	 vcat $ (flip map) egs $ \ (a,b) ->
          t"consume" <> parens (obj_ref b <> commspc <> (t$ fromAtom a)) <> semi),

         -- Finally, produce relations:
	(let egs = concat $ 
	           map (\ (a,b,_) -> 
			      case (lab graph a, lab graph b) of 
				 (Just (CGSteps s), Just (CGItems i)) -> [(s,i)]
			         (Just (CGSteps s), Just (CGTags  t)) -> [(s,t)]
				 _ -> []) $ 
		   labEdges graph in 
	 vcat $ (flip map) egs $ \ (a,b) ->
          t"produce" <> parens (obj_ref a <> commspc <> (t$ fromAtom b)) <> semi),

        t"\n#endif"
	     
        ])
       ] ++

     [space, t "// The context class destructor: "] ++
     [hangbraces 
       (t"~" <> maincontext <> parens empty )
       indent 
       -- Body of the constructor
       -- Destroy temporary step objects/collections:
       (vcat$ (flip map) stepls $ \ stp -> 
	t"delete " <> (t$ step_obj$ fromAtom stp) <> semi
       )]

   ------------------------------------------------------------
   -- Emit private contexts for each step:
   ------------------------------------------------------------   
   
   -- INVARIANT! Due to error checking above, we can omit some error checking here:
   putS$ "\n\n// Now we define 'private contexts'.\n"
   putS$ "// These allow steps to exist in their own little universes with special properties:\n" 

   forM_ stepls $ \stp -> 
     do let privcontext = textAtom stp <> t"_context"
	putD$ 
 	 cppclass privcontext
	  (--t "\n  private:" $$
	   --t "// A pointer to the main context for the application.  This context is a wrapper for that one:" $$ 
	   --maincontext <> t" & m_parent;"  $$ 

           t "\n  public:" $$
	   t "// Tag collections are simply aliases to the parents':" $$ 
	   (vcat$ (flip map) (AM.toList tags) $ \ (tg, Just ty) -> 
	    text "CnC::tag_collection" <> angles (dType ty) <> t" & " <> textAtom tg <> semi
	   ) $$ 

	   -- Wrap item collections:
	   t""$$ 
	   (vcat $ 
	    (flip map) (AM.toList items) $ \ (it,Just (ty1,ty2)) -> 
  	    --textAtom it <> parens (t "this")
	    let f x = hangbraces (t "inline void " <> t x <> parens (dType ty1 <> t" tag, " <> dType ty2 <> t" & ref")) 
	                         indent (t "m_"<> textAtom it <> t"." <> t x <> t "(tag,ref);") 
	        wrapper = textAtom it <> t"_wrapper"
	        member  = t"m_" <> textAtom it
	    in
	    t "// A 'NoOp' wrapper class that does nothing: "$$
	    cppclass wrapper
	             (t "public:" $$ 
		      t"CnC::item_collection" <> angles (dType ty1 <>commspc<> dType ty2) <+> member <> semi $$ t"" $$
                      t "// The constructor here needs to grab a reference from the main context:" $$
		      (hangbraces (wrapper <> parens (maincontext <> t" & p") <+> colon <+>
				   (member <> t "(p." <> textAtom it <> t ")"))
		       indent empty) $$ 
		      f "get" $$ 
		      f "put") $$ 
            t "") $$

           -- Declare MEMBERS
	   t ""$$ t "// Item collections are all wrapped for now:" $$ 
	   (vcat $ 
	    (flip map) (AM.toList items) $ \ (it,Just (ty1,ty2)) -> 
	    textAtom it <> t"_wrapper" <+> textAtom it <> semi 
	   ) $$
	   
	   t""$$ t "// Constructor for the private/custom context: " $$	   
	   hangbraces (privcontext <> parens (maincontext <> t" & p") <+> colon $$ 
		       (nest 6 $ vcat $ 
			(flip map) (AM.toList items) $ \ (it,Just (ty1,ty2)) -> 
			textAtom it <> parens (t"p") <> commspc ) $$
		       (nest 6 $ vcat$ punctuate commspc $ (flip map) (AM.toList tags) $ \ (tg,Just ty) -> 
		 	 textAtom tg <> parens (t$ "p."++fromAtom tg))
		      )
		      indent (empty) 

	   -- Have to wrap tag collections as well just to redirect references to the main context:
	  )



   ------------------------------------------------------------
   -- Finish up
   ------------------------------------------------------------
   -- This is the "footer" for the document.
   putS$ "\n\n#endif // "++appname++"_H_ALREADY_INCLUDED\n"
   return ()




--------------------------------------------------------------------------------
-- Produce the prototype for a single step.
emitStep appname name ty = putD$ 
  struct (t name)
	 (t ("template < class ctxt > ") $$
	  t "int execute(" <+> constRefType ty <+> t "tag," <+> 
	  t "ctxt & c) const;"
	  --t appname <> t "_context & c) const;"
	 )


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


