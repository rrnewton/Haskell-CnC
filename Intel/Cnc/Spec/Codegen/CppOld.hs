{-# LANGUAGE RecordWildCards, QuasiQuotes, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}


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

-- QuasiQuoting is too expensive in final binary size:
-- import Text.InterpolatedString.QQ

import Data.List
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
step_obj "env" = "env"
step_obj str = "m_" ++ str 

--obj_ref a   = t"*" <> (t$ step_obj$ fromAtom a)
obj_ref a   = (t$ step_obj$ fromAtom a)


-- The probability of any of the below stuff being reusable is pretty low.
-- (It's very C++ specific. This whole *strategy* of private context generation is probably pretty C++ specific.)
-- But I thought I would begin to at least BEGIN to abstract the syntax-construction operations.
mkPtr d = d <> t"*"
deref x y = x <> t"." <> y


-- initAssign
-- initAssignCast
assignCast ty x y = ty <+> x <+> t"=" <+> parens ty <> y <> semi
assign x y =  toDoc x <+> t"=" <+> toDoc y <> semi

app fn ls = toDoc fn <> (parens$ hcat$ intersperse (t", ")$ map toDoc ls)
thunkapp fn = app fn ([] :: [Doc])

constructor name args inits body = 
    hangbraces (app name args <+> colon $$ 
		nest 10 (vcat$ map_but_last (<>t", ")$ map (\ (a,b) -> a <> parens b) inits)) 
                indent body
param ty name = ty <+> name
mkRef tyD = tyD <> t"&"
mkConstRef tyD = t"const" <+> mkRef tyD

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

----------------------------------------------------------------------------------------------------

--emitCpp :: StringBuilder m => Bool -> Bool -> CncSpec -> m ()
-- emitCpp old_05_api genstepdefs (spec @ CncSpec{..}) = do 
emitCpp :: StringBuilder m => CodeGenConfig -> CncSpec -> m ()
emitCpp CGC{..} (spec @ CncSpec{..}) = do 

   -- First we produce the header of the file:
   --------------------------------------------------------------------------------
   putS$ "\n//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"
   putS$ "// This code was GENERATED from a CnC specification, DO NOT MODIFY.\n"
   putS$ "//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n\n"
   putS$ "#ifndef "++appname++"_H_ALREADY_INCLUDED\n"
   putS$ "#define "++appname++"_H_ALREADY_INCLUDED\n\n"

   -- Ideally this would only be included IF tuple types are used... complicated right now.
   -- putS  "#include \"boost/tuple/tuple.hpp\"\n\n"
   putS  "// For now we use C++ TR1 to provide tuples:\n"
   putS  "#include <tr1/tuple>\n"
   putS "#define cnctup std::tr1\n\n"

   putS  "#include <cnc/cnc.h>\n"
   putS  "#include <cnc/debug.h>\n"
   putS  "#include <cnc/internal/tls.h>\n"

   -- [2010.08.16] This is an INSUFFICIENT forward declaration:

   ------------------------------------------------------------
   -- Emit the step prototypes (two possibilities here):
   ------------------------------------------------------------
   when (genstepdefs)$ 
     putS  "\n// Next this generated file contains prototypes for each step implementation:\n"
   when (not genstepdefs)$ do 
     putS  "// The user's step types should be defined SEPARATELY from this header, and must \n"
     putS  "// be in scope before this header is included.\n"
     putS  "// As a hint, the below are valid example definitions:\n"
     putS  "/*\n"
   let -- Don't include builtins (e.g. "env"):
       stepls = filter (\ x -> not$ x `elem` builtinSteps) $
		AS.toList steps
       prescribers = map (getStepPrescriber spec) stepls
       tagtys = map (\ name -> case tags AM.! name of 
		                  Nothing -> error$ "Tag collection '"++ show name ++"' missing type, needed for C++ codegen!"
		                  Just ty -> ty)
		prescribers  
       privcontext stp = textAtom stp <> t"_context"
       privcontext_member stp = t"m_priv_" <> privcontext stp 
       tls_key stp = privcontext stp <> t"_tls_key"
		
   forM_ (zip stepls tagtys) $ \ (stp,ty) ->
     do emitStep appname (fromAtom stp) ty 
        putS "\n\n"
   when (not genstepdefs)$ putS  "*/\n"


   when (not old_05_api) $ do 
     putS$ "\n\n// Forward declarations for private contexts.\n"
     forM_ stepls $ \stp -> do
	putD$ t"class " <> privcontext stp <> semi <> t"\n"
     putS$ "\n"

   ------------------------------------------------------------
   -- Prototype for user step wrappers
   ------------------------------------------------------------   
   let maincontext = t$ appname++"_context"   
   let stepwrapper stp = textAtom stp <> t"_step_wrapper"

   when (not old_05_api)$ do
     putS  "// Forward declaration of the context class (also known as graph)\n"
     putD$ t"struct " <> maincontext <> t";\n\n"

     putS$ "// Type definitions for wrappers around steps.\n"
     forM_ (zip stepls tagtys) $ \ (stp,ty) -> do
	putD$ struct (stepwrapper stp) $ 
	  textAtom stp <+> t"m_step" <> semi $$
	  --stepwrapper stp <> parens empty <> semi
	  t"int execute(" <+> constRefType ty <+> t "tag," <+> maincontext <> t" & c) const;"
	putS$ "\n"

   ------------------------------------------------------------
   -- Emit the main context class
   ------------------------------------------------------------   
   putS$ "\n\n// Here is the definition for the main application context:\n" 

   putD$ cppclass (maincontext <> t " : public CnC::context" <> angles (pad maincontext)) $ 
     vcat $ 

     -- Discarded this API proposal already:
     -- t "\n  private:" : 
     -- t "// Members to store instances of the users Step type:" : 
     -- ((flip map) stepls $ \ stp -> 
     --  textAtom stp <> t"* " <> (t$ step_obj$ fromAtom stp) <> semi) ++

     t "\n  public:" : 

     (if old_05_api then [] else  
       t "// Step collections members:" : 
       ((flip map) stepls $ \ stp -> 
	 -- TEMPTOGGLE -- ALWAYS wrap steps for now:
         t "CnC::step_collection" <> angles (stepwrapper stp) <+> (t$ step_obj$ fromAtom stp) <> semi
       )) ++      

     t "": t "// Tag collections members:" : 
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

     [space, t "// Pointers to (children) private contexts:" ] ++
     ((flip map) stepls $ \ stp -> 
       t "// "<>privcontext stp <> t"*" <+> privcontext_member stp <> semi
      )++ 
     
     [space, t "// Keys for thread local storage:" ] ++
     ((flip map) stepls $ \stp -> t"int "<> tls_key stp <> semi) ++ 

     [space, t "// The context class constructor (prototype:) "] ++
     [maincontext <> parens empty <> semi]

     -- [2010.08.19] Don't need the destructor for now:
     -- [space, t "// The context class destructor: "] ++
     -- [hangbraces 
     --   (t"~" <> maincontext <> parens empty )
     --   indent 
     --   -- Body of the constructor
     --   empty
     --   -- Destroy temporary step objects/collections:
     --   -- (vcat$ (flip map) stepls $ \ stp -> 
     --   -- 	t"delete " <> (t$ step_obj$ fromAtom stp) <> semi)
     -- ]

   ------------------------------------------------------------
   -- Emit private contexts for each step:
   ------------------------------------------------------------   
   
   -- INVARIANT! Due to error checking above, we can omit some error checking here:
   when (not old_05_api) $ do 
     putS$ "\n\n// Next we define 'private contexts'.\n"
     putS$ "// These allow steps to exist in their own little universes with special properties:\n" 
     forM_ stepls $ \stp -> do
	putD$ 
 	 cppclass (privcontext stp) -- <+> colon <+> t"public CnC::context" <> angles (privcontext stp) )
	  (--t "\n  private:" $$
	   --t "// A pointer to the main context for the application.  This context is a wrapper for that one:" $$ 
	   --maincontext <> t" & m_parent;"  $$ 

           t "\n  public:" $$

           t "  int m_scratchpad; // TEMP, testing\n" $$

           ------------------------------------------------------------   
	   -- Wrap tag collections:
           ------------------------------------------------------------   
	   t "// Tag collections are simply aliases to the parents':" $$ 
	   (vcat$ (flip map) (AM.toList tags) $ \ (tg, Just ty) -> 
	    text "CnC::tag_collection" <> angles (dType ty) <> t" & " <> textAtom tg <> semi
	   ) $$ 

           ------------------------------------------------------------   
	   -- Wrap item collections:
           ------------------------------------------------------------   
	   t""$$ 
	   (vcat $ 
	    (flip map) (AM.toList items) $ \ (it,Just (ty1,ty2)) -> 

	    let -- A reused bit of syntax for wrapper methods:
 	        -- (This is one of those things that you don't want to duplicate, 
		--  but it has too many arguments and is poorly abstracted.)
	        wrapGP doret retty nm args = 
                       let args' = map (\ (tyD,v) -> tyD <+> text v) args 
			   decls = hcat$ intersperse (t", ") args'
			   vars  = hcat$ intersperse (t", ") $ map (text . snd) args
		       in
		       hangbraces (t "inline "<> toDoc retty <+> t nm <> parens decls) 
	                         indent 
				 (
				  --------------------------------------------------------------------------------
				  -- TODO INSERT CORRECTNESS CHECKING HERE:
				  --------------------------------------------------------------------------------
				  (if doret then t"return " else t"") <>
				  t "m_"<> textAtom it <> t"." <> t nm <> parens vars <> semi) 

		basicGP nm = wrapGP False "void" nm [(mkConstRef (dType ty1), "tag"), (mkRef (dType ty2), "ref")]

	        wrapper = textAtom it <> t"_wrapper"
	        member  = t"m_" <> textAtom it
	    in
	    t "// The item collection wrapper: A 'NoOp' wrapper class that does nothing: "$$
	    cppclass wrapper
	             (t "public:" $$  
		      mkPtr (privcontext stp) <> t" m_context;\n" $$ 
		      t"CnC::item_collection" <> angles (dType ty1 <>commspc<> dType ty2) <> t" & " <> member <> semi $$ t"" $$
                      t "// The constructor here needs to grab a reference from the main context:" $$
		      (constructor wrapper 
		                   [param (mkRef maincontext)      (t"p"),
				    param (mkPtr$ privcontext stp) (t"c")]
		                   [(member, t"p." <> textAtom it)]
		                   (assign "m_context" "c"))  $$ 
		      -- Just three methods: two variants of get and one put.
		      basicGP "get" $$ 
		      basicGP "put" $$ 
		      wrapGP True  (dType ty2) "get" [(mkConstRef (dType ty1),"tag")] ) $$
            t "") $$

           -- Declare MEMBERS
	   t ""$$ t "// Item collections are all wrapped for now:" $$ 
	   (vcat $ 
	    (flip map) (AM.toList items) $ \ (it,Just (ty1,ty2)) -> 
	    textAtom it <> t"_wrapper" <+> textAtom it <> semi 
	   ) $$
	   
           ------------------------------------------------------------   
	   t""$$ t "// Constructor for the private/custom context: " $$	   
	   hangbraces (privcontext stp <> parens (maincontext <> t" & p") <+> colon $$ 
		       (nest 6 $ vcat $ 
			(flip map) (AM.toList items) $ \ (it,Just (ty1,ty2)) -> 
			textAtom it <> parens (t"p, this") <> commspc ) $$

		       (nest 6 $ vcat$ punctuate commspc $ (flip map) (AM.toList tags) $ \ (tg,Just ty) -> 
		 	 textAtom tg <> parens (t$ "p."++fromAtom tg))
		      )
		      indent (empty) 

	   -- Have to wrap tag collections as well just to redirect references to the main context:
	  )


   ------------------------------------------------------------
   -- Execute wrapper methods 
   ------------------------------------------------------------   

   putS$ "\n\n// Execute method wrappers for each step that uses a privaate context:\n"
   forM_ (zip stepls tagtys) $ \ (stp,ty) -> do
      putD$ hangbraces 
	(t"int "<> stepwrapper stp <> t"::execute(" <+> constRefType ty <+> t "tag," <+> maincontext <> t" & c) const")
	indent $

        assignCast (mkPtr$ privcontext stp) (t"ptr") 
		   (app "CnC::Internal::CnC_TlsGetValue" [deref (t"c") (tls_key stp)]) $$ 

	hangbraces (t"if " <> parens (t"!ptr")) indent
		   (assign "ptr" (t"new " <> app (privcontext stp) ["c"]) $$
		    app "CnC::Internal::CnC_TlsSetValue" [deref (t"c") (tls_key stp), t"ptr" ] 
		    <> semi) $$

	-- t"printf(\"PTR %p\\n\", ptr);" $$

	t"return" <+> app "m_step.execute" ["tag", "*ptr"] <> semi
	-- privcontext stp <> t"* ptr = ("<> privcontext stp 
        --  <> t"*) CnC::Internal::CnC_TlsGetValue" <> parens (t"c." <> tls_key stp) <> semi


   ------------------------------------------------------------
   -- Main Context constructor
   ------------------------------------------------------------
   putS "\n\n"
   when (not old_05_api)$ putS "// Finally, define the constructor for the main context AFTER the private contexts are defined:\n"
   putD $ 
     (hangbraces 
       (maincontext <> t"::" <> maincontext <> parens empty <+> colon $$
     	-- Initializer list:
     	(nest 6 $ vcat $ punctuate commspc $ 

         (if old_05_api then [] else  
     	   t "// Initialize step collections" :
           ((flip map) stepls $ \ stp -> 
	      -- For now ALWAYS use a private context
	      t"// "<> privcontext_member stp <> parens (t"new "<> privcontext stp <> parens (t"*this")) <> commspc $$
     	      (t$ step_obj$ fromAtom stp) <> parens (t"this") 
     	   )) ++

     	 t "// Initialize tag collections" :
     	 ((flip map) (AM.toList tags) $ \ (tg,Just ty) -> 
     	   textAtom tg <> parens (t "this, false") 
     	 ) ++ 
     	 t "// Initialize item collections" :
     	 ((flip map) (AM.toList items) $ \ (it,Just (ty1,ty2)) -> 
     	   textAtom it <> parens (t "this")
     	 )))
       indent 
       -- Body of the constructor
       (vcat $ 
         [-- Create objects for all step collections:
     	  -- This was an intermediate step in our API evolution:
     	  -- (vcat$ (flip map) stepls $ \ stp -> 
     	  --  (t$ step_obj$ fromAtom stp) <> t" = new " <> textAtom stp <> parens empty <> semi
     	  -- ),	  

          t"CnC::step_collection< "<> textAtom (head stepls)  <>t" > env(this);\n"
          ]++

	  --------------------------------------------------------------------------------
	  -- FIXME:
	  -- This should be completely obsoleted when the API catches up (e.g. trace_all works properly)
	  (if gendebug then 
	    ((flip map) (zip3 stepls prescribers tagtys) $ \ (stp,tg,ty) ->
	       app "CnC::debug::trace" [stepwrapper stp <> parens empty, (dubquotes stp)] <> semi) ++
	    ((flip map) (AM.toList tags) $ \ (tg,_) -> 
     	     app "CnC::debug::trace" [textAtom tg, dubquotes tg] <> semi) ++
	    ((flip map) (AM.toList items) $ \ (it,_) -> 
	     app "CnC::debug::trace" [textAtom it, dubquotes it] <> semi) 
	   else []) ++
	  --------------------------------------------------------------------------------
	  [t"", 

          -- Generate prescribe relations first [mandatory]:
     	  (vcat $ (flip map) (zip3 stepls prescribers tagtys) $ \ (stp,tg,ty) ->
           t"prescribe" <> parens (pad$ hcat $ punctuate commspc $ 
     				     [ textAtom tg
     				     , if old_05_api 
     				       then textAtom stp <> parens empty
     				       else obj_ref stp] 
				     -- [2010.08.19] Revamp: don't need tuner/private context here anymore:
				     -- if old_05_api then [] else 
				     -- [t"CnC::default_tuner< " <> dType ty <>commspc<> privcontext stp <> t" >" <> parens empty
				     --  , t"*" <> privcontext_member stp 
     				     --  --,  privcontext stp <> parens (t"this")
     				     -- ]
				  )
           <> semi),

          -- Next, consume relations:
     	  if old_05_api then empty else 
     	    (let egs = concat $ 
     	           map (\ (a,b,_) -> 
     			      case (lab graph a, lab graph b) of 
     				 (Just (CGItems i), Just (CGSteps s)) -> [(i,s)]
     				 _ -> []) $ 
     		   labEdges graph in 
     	     vcat $ (flip map) egs $ \ (a,b) ->
              t"consume" <> parens (obj_ref b <> commspc <> (t$ fromAtom a)) <> semi),
          -- Finally, produce relations:
     	  if old_05_api then empty else 
     	    (let egs = concat $ 
     	           map (\ (a,b,_) -> 
     			      case (lab graph a, lab graph b) of 
     				 (Just (CGSteps s), Just (CGItems i)) -> [(s,i)]
     			         (Just (CGSteps s), Just (CGTags  t)) -> [(s,t)]
     				 _ -> []) $ 
     		   labEdges graph in 
     	     vcat $ (flip map) egs $ \ (a,b) ->
              t"produce" <> parens (obj_ref a <> commspc <> (t$ fromAtom b)) <> semi), 

          t"", 
          (vcat$ (flip map) stepls $ \ stp -> 
	      assign (tls_key stp) 
	             (thunkapp "CnC::Internal::CnC_TlsAlloc"))

        ]
          ----------------------------------------
          -- One more thing, turn on debugging if it has been requested.

	))

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
  TTuple ls -> t "cnctup::tuple" <> angles (hcat$ punctuate commspc$ map dType ls)
  --TTuple ls -> error$ "CppOld codegen: Tuple types of length "++ show (length ls) ++" not standardized yet!"



------------------------------------------------------------------------------------------------------------------------
-- Tag function correctness checking code.
------------------------------------------------------------------------------------------------------------------------


------------------------------------------------------------------------------------------------------------------------
-- CODING HINTS GENERATION
------------------------------------------------------------------------------------------------------------------------

codingHints :: StringBuilder m => Bool -> CncSpec -> m ()

codingHints old_05_api (spec @ CncSpec{..}) =
 do putD $ 
     (hangbraces 
       (t"foo") 4 (t"baz")
       )
    

{-

/****************************************************************************
/* The following code provides an example of what the user code that invokes
/* this graph might look, using the inputs and outputs defined for the 
/* environment (ENV).
/***************************************************************************/
    #include "mandel.h"
    
    // Create an instance of the context class which defines the graph
    mandel_context c;
    
    // Debug trace can be enabled for a collection with the call:
    //     CnC::debug::trace( c.position, "position" );
    
    // For each item from the environment (ENV), put the item using the  
    // proper tag    
    c.data.put( data_Tag, ... );
    c.max_depth.put( max_depth_Tag, ... );
    
    // For each tag value from the environment (ENV), put the tag into
    // the proper tag collection
    c.position.put( position_Tag );
    
    // Wait for all steps to finish
    c.wait();
    
    // For each output to the environment (ENV), get the item using the 
    // proper tag    
    int pixel_ENV;
    pixel.get( pair(...), pixel_ENV );

/*********************************************************************
/* The following code provides an example of what the user Step code 
/* might look like for this Step, using the inputs and outputs defined
/* in the context.
/********************************************************************/
int compute::execute(const pair & t, mandel_context & c ) const
{
     
    // For each input item for this step retrieve the item using the proper tag value
    complex data_instance;
    c.data.get( pair(...), data_instance );
    int max_depth_instance;
    c.max_depth.get( int(...), max_depth_instance );

    // Step implementation logic goes here
    ...

    // For each output item for this step, put the new item using the proper tag value   
    c.pixel.put( pair(...), ... );

    return CnC::CNC_Success;
}

-}