{-# LANGUAGE RecordWildCards, QuasiQuotes, NamedFieldPuns #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- OverloadedStrings -- TODO: currently causes ambiguities with toDoc


----------------------------------------------------------------------------------------------------
-- This is the code generator for the original (CnC/C++ 0.1-0.5) "context"-based C++ API.
-- Original Author: Ryan Newton
----------------------------------------------------------------------------------------------------

-- This should eventually be based on some intermediate representation of C code.  For a
-- first cut, however, it is easier to just generate syntax directly.  (Especially with
-- the assistance of the pretty printing libary.)

module Intel.Cnc.Spec.Codegen.CppOld where

import Intel.Cnc.Spec.AST hiding (commacat)
import Intel.Cnc.Spec.CncGraph
--import Intel.Cnc.Spec.GatherGraph
import Intel.Cnc.Spec.Util 

import qualified Intel.Cnc.EasyEmit as EE
import Intel.Cnc.EasyEmit hiding (app, not, (&&), (==))

import Control.Monad.State
import StringTable.Atom

import Text.PrettyPrint.HughesPJClass 

-- QuasiQuoting is too expensive in final binary size:
-- import Text.InterpolatedString.QQ

import qualified Data.Map as M
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


-- [2010.11.14] TEMPTOGGLE: Disabling the one-argument version of get:
oldstyle_get_method = False

commacat ls = hcat (intersperse (text ", ") $ ls)

----------------------------------------------------------------------------------------------------

--emitCpp :: StringBuilder m => Bool -> Bool -> CncSpec -> m ()
-- emitCpp old_05_api genstepdefs (spec @ CncSpec{..}) = do 
emitCpp :: StringBuilder m => CodeGenConfig -> CncSpec -> m ()
--emitCpp CGC{..} (spec @ CncSpec{..}) = do 
emitCpp CGC{..} (spec @ CncSpec{appname, steps, tags, items, graph, realmap}) = do 
-- HOWTO READ the below code:
-- This code emits a series of strings/docs to build up a file.
-- Some of the more complex looking bits are building up large lists of type [Doc].

   -- First we produce the header of the file:
   --------------------------------------------------------------------------------
   -- TODO: Try quasiquoting for multiline-strings here again when they fix the binary bloating problem:
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


   putS  "// This tells cnc.h to define certain things.  TODO: should do this ONLY if tuples are needed!\n"
   putS  "#define CNC_ASSUME_TR1 \n\n"

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

   when (wrapall && not old_05_api)$ do
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
         t "CnC::step_collection" <> angles (if wrapall then stepwrapper stp else textAtom stp) 
	   <+> (t$ step_obj$ fromAtom stp) <> semi
       )) ++      

     t "": t "// Tag collections members:" : 
     ((flip map) (AM.toList tags) $ \ (tg,mty) -> 
       case mty of 
         Nothing -> error$ "CppOld Codegen: tag collection without type: "++ (fromAtom tg)
         Just ty -> text "CnC::tag_collection" <> angles (cppType ty) <+> textAtom tg <> semi
     ) ++ 

     [space, t "// Item collections members:" ] ++
     ((flip map) (AM.toList items) $ \ (it,mty) -> 
       case mty of 
         Nothing -> error$ "CppOld Codegen: item collection without type: "++ (fromAtom it)
         Just (ty1,ty2) -> trace ("ITEM COLLECTION WITH TYPE" ++ show (ty1,ty2)) $
                           t "CnC::item_collection" <> 
                           let extra = 
                                case ty1 of  
                                  -- In the case where the tag type is dense in all dimensions, turn on CNC_VECTOR:
                                  TDense _ -> commspc<> t"CnC::cnc_tag_hash_compare" <> angles (cppType ty1) <>commspc<> t"CnC::CNC_VECTOR"
                                  _        -> empty
                           in angles (cppType ty1 <>commspc<> cppType ty2 <> extra) <+> textAtom it <> semi
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

   --------------------------------------------------------------------------------
   -- Emit private contexts for each step.  This means building wrappers.
   --------------------------------------------------------------------------------
   
   -- INVARIANT! Due to error checking above, we can omit some error checking here:
   when (wrapall && not old_05_api) $ do 
     putS$ "\n\n// Next we define 'private contexts'.\n"
     putS$ "// These allow steps to exist in their own little universes with special properties:\n" 
     forM_ stepls $ \stp -> do
	putD$ 
 	 cppclass (privcontext stp) -- <+> colon <+> t"public CnC::context" <> angles (privcontext stp) )
	  (--t "\n  private:" $$
	   --t "// A pointer to the main context for the application.  This context is a wrapper for that one:" $$ 
	   --maincontext <> t" & m_parent;"  $$ 

           t"\n  public:" $$

           t"  int m_scratchpad; // TEMP, testing\n" $$

           ------------------------------------------------------------   
	   -- Wrap tag collections:
           ------------------------------------------------------------   
	   t "// Tag collections are simply aliases to the parents':" $$ 
	   (vcat$ (flip map) (AM.toList tags) $ \ (tg, Just ty) -> 
	    text "CnC::tag_collection" <> angles (cppType ty) <> t" & " <> textAtom tg <> semi
	   ) $$ 

           ------------------------------------------------------------   
	   -- Wrap item collections:
           ------------------------------------------------------------   
	   -- FIXME: No reason to wrap ALL item collections.
	   t""$$ 
	   (vcat $ 
	    (flip map) (AM.toList items) $ \ (it,Just (ty1,ty2)) -> 

	    let -- A reused bit of syntax for wrapper get/put methods:
 	        -- (This is one of those things that you don't want to duplicate, 
		--  but it has too many arguments and is poorly abstracted.)
	        wrapGP doret retty nm args isPut = 
		       -- First let's put together the function's arguments:
                       let args' = map (\ (tyD,v) -> tyD <+> text v) args 
			   decls = commacat args'
			   vars  = commacat $ map (text . snd) args
		       in
		       hangbraces (t "inline "<> toDoc retty <+> t nm <> parens decls) 
	                         indent 
				 (-- CHECK TAG FUNCTIONS
				  --------------------------------------------------------------------------------
				  let checkTagFun stp target getnbrs = 
				        -- TODO INSERT CORRECTNESS CHECKING HERE:
				        let stpnd  = realmap M.! (CGSteps stp)
				            itemnd = realmap M.! target
				            gctxt  = context graph itemnd
				            lnhbrs = getnbrs gctxt-- lpre' gctxt ++ lsuc' gctxt
				            ls =  filter (\ (nd,lab) -> nd == stpnd) lnhbrs
	 			            -- [(_,tf)] = filter (\ (nd,lab) -> nd == stpnd)$  lpre' gctxt				            
				            --[(_,tf)] = trace (" LENGTH "++ show (length ls) ++" "++ show stpnd ++" "++ show itemnd++" "++ show lnhbrs) ls
				        in 
				           case ls of 
				             -- If we have no relationship to that collection its an error to access it:
				             -- [] -> do fprintf [stderr, dubquotes$ printf "CnC graph violation: should not access collection '%s' from '%s'" 
					     -- 		                                 (graphNodeName target)  (show stp)]
				             --          abort[]

                                             [] -> t$"fprintf(stderr, \"CnC graph violation: should not access collection '"
				                     ++ graphNodeName target ++"' from step '"++ show stp ++"'\\n\"); abort();"
				             [(_,Nothing)] -> t"// No tag function to check..."
				             --((_,Just (TF args exps)) : []) -> 
				             ((_,Just (TF args exps)) : tl) ->  -- TEMPTOGGLE ... permissive, ignoring additional tag functions!
				               t("// CHECK args "++ show args) $$
				               t("// exps "++ show exps)
				             _ -> error$ "internal error: tag function correctness codegen: \n "++show ls
				  in
				  (if gendebug
        	  	            then checkTagFun stp (CGItems it) (if isPut then lpre' else lsuc') 
				    else empty) $$
				  --------------------------------------------------------------------------------
				  (if doret then t"return " else t"") <>
				  t "m_"<> textAtom it <> t"." <> t nm <> parens vars <> semi) 

                -- Basic get or put:
		basicGP nm isPut = wrapGP False "void" nm [(mkConstRef (cppType ty1), "tag"), 
							   ((if isPut then mkConstRef else mkRef) (cppType ty2) , "ref")] isPut

	        wrapper = textAtom it <> t"_wrapper"
	        member  = t"m_" <> textAtom it
	    in
	    t "// The item collection wrapper: A 'NoOp' wrapper class that does nothing: "$$
	    cppclass wrapper
	             (t "public:" $$  
		      mkPtr (privcontext stp) <> t" m_context;\n" $$ 
		      t"CnC::item_collection" <> angles (cppType ty1 <>commspc<> cppType ty2) <> t" & " <> member <> semi $$ t"" $$
                      t "// The constructor here needs to grab a reference from the main context:" $$
		      (constructor wrapper 
		                   [param (mkRef maincontext)      (t"p"),
				    param (mkPtr$ privcontext stp) (t"c")]
		                   [(member, t"p." <> textAtom it)]
		                   (assign "m_context" "c"))  $$ 
		      -- Just three methods: two variants of get and one put.
		      basicGP "get" False $$ 
		      basicGP "put" True $$
		      if oldstyle_get_method 
		      then wrapGP True  (cppType ty2) "get" [(mkConstRef (cppType ty1),"tag")] False
		      else empty
		      ) $$
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
	putS$ "\n"
	putS$ "\n"

   ------------------------------------------------------------
   -- Execute wrapper methods 
   ------------------------------------------------------------   

   when (wrapall) $ do
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
				     -- [t"CnC::default_tuner< " <> cppType ty <>commspc<> privcontext stp <> t" >" <> parens empty
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

        ] ++
	--------------------------------------------------------------------------------
	-- FIXME:
	-- This should be completely obsoleted when the API catches up (e.g. trace_all works properly)
	[hangbraces (t"if " <> parens (if gentracing then t"1" else t"0")) indent (vcat $
	  ((flip map) (zip3 stepls prescribers tagtys) $ \ (stp,tg,ty) ->
	     app "CnC::debug::trace" [(text$ step_obj$ fromAtom stp), (dubquotes stp)] <> semi) ++
	  ((flip map) (AM.toList tags) $ \ (tg,_) -> 
	   app "CnC::debug::trace" [textAtom tg, dubquotes tg] <> semi) ++
	  ((flip map) (AM.toList items) $ \ (it,_) -> 
	   app "CnC::debug::trace" [textAtom it, dubquotes it] <> semi) 
	 )]
	--------------------------------------------------------------------------------


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


-- Make a type both const and a reference.
-- TODO: add some error checking here to see if it is already const or reference...
constRefType ty = t "const" <+> cppType ty <+> t "&"



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