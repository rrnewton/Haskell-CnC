{-# LANGUAGE RecordWildCards, QuasiQuotes, NamedFieldPuns, ScopedTypeVariables #-}
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

import Intel.Cnc.Spec.Codegen.CodegenShared

import Intel.Cnc.Spec.AST hiding (commacat)
import Intel.Cnc.Spec.CncGraph
--import Intel.Cnc.Spec.GatherGraph
import Intel.Cnc.Spec.Util as U

import qualified Intel.Cnc.EasyEmit as EE
import Intel.Cnc.EasyEmit hiding (app, not, (&&), (==), (||))

import Control.Monad.State
import StringTable.Atom

import Text.PrettyPrint.HughesPJClass 
import Text.Printf

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
step_obj e | e == special_environment_name = e
step_obj str = "m_" ++ str 

--obj_ref a   = t"*" <> (t$ step_obj$ fromAtom a)
obj_ref a   = (t$ step_obj$ fromAtom a)


-- [2010.11.14] TEMPTOGGLE: Disabling the one-argument version of get:
oldstyle_get_method = False

commacat ls = hcat (intersperse (text ", ") $ ls)

s = Syn . text 
doc2Ty = TSym . toAtom . render

instance FromAtom Doc where
  fromAtom = text . fromAtom

-- If a block of text is nonempty, then preface it with a comment:
maybeCommentDoc ls txt = 
  if txt == t""
  then txt
  else vcat (map (\ cmnt -> if cmnt == t"" then cmnt else t"// "<> cmnt) ls) $$ 
            txt

maybeComment ls mnd = 
  -- FIXME FIXME:
  -- FIXME!! THIS EXECUTES IT TWICE!
  let doc = execEasyEmit mnd in
  if doc == t""
  then return ()
--  else putD$ maybeCommentDoc ls doc
  else do forM_ ls $ \ cmnt ->
	    putD$ if cmnt == t"" then cmnt else t"// "<> cmnt
	  mnd

----------------------------------------------------------------------------------------------------

-- This predicate determines whether we can successfully determine all
-- the data dependencies of a step, for example to generate a depends function.
all_tagfuns_tractible :: CncGraph -> Atom -> Bool
all_tagfuns_tractible graph step =
  True
 -- FIXME -- TODO -- FINISH THIS!!!


-- Name for a private context:
privcontext stp = textAtom stp <> t"_context"

----------------------------------------------------------------------------------------------------

--emitCpp :: StringBuilder m => CodeGenConfig -> CncSpec -> m ()
emitCpp :: CodeGenConfig -> CncSpec -> EasyEmit ()

emitCpp (config@CodeGenConfig{..}) (spec @ CncSpec{appname, steps, tags, items, graph, realmap}) = do 
-- HOWTO READ the below code:
-- This code emits a series of strings/docs to build up a file.
-- Some of the more complex looking bits are building up large lists of type [Doc].
--
-- [2010.11.15] NOTE: This will get more complex for a little while
-- because I am IN THE MIDDLE OF switching to use EasyEmit functionality.

   --------------------------------------------------------------------------------
   -- Prelude: set up some bindings for EasyEmit functions and other helpers:
   --------------------------------------------------------------------------------
   let 
       -- Don't include builtins (e.g. the environment):
       stepls = filter (\ x -> not$ x `elem` builtinSteps) $
		AS.toList steps
       stepls_with_types = zip stepls tagtys

       -- Call each plugin's "constructor" so it can analyze the spec.
       initialized_plugins = map (\pg -> pg spec) plugins
       
       -- Then create a map of which plugins apply to which steps:
       plug_map = AM.fromList$
		  filter (not . null . snd) $
		  map (\stpC -> (stpC, 
				 catMaybes$ map (\pg -> pg stpC) 
				                initialized_plugins))
		      (toAtom special_environment_name : stepls)

       -- This predicate determines whether there is any requirement to wrap a particular step collection:
       shouldWrapStep stpname = 
	 wrapall || AM.member stpname plug_map
	 --wrapall || any (\ cgh -> hooksPredicate cgh stpname ) plugins       
	 

       areAnyWrapped = wrapall || any shouldWrapStep stepls

       tractible_depends_steps = AS.fromList (filter (all_tagfuns_tractible graph) stepls)

       prescribers = map (getStepPrescriber spec) stepls
       tagtys = map (\ name -> case tags AM.! name of 
		                  Nothing -> error$ "Tag collection '"++ show name ++"' missing type, needed for C++ codegen!"
		                  Just ty -> ty)
		prescribers  

       privcontext_member stp = t"m_priv_" <> privcontext stp 
       tls_key stp = privcontext stp <> t"_tls_key"

       tuner_name stp = toDoc stp <> t"_tuner"


   --------------------------------------------------------------------------------
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
   when areAnyWrapped$ putS  "#include <cnc/internal/tls.h>\n"

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
		
   forM_ stepls_with_types $ \ (stp,ty) ->
     do emitStep appname (fromAtom stp) ty 
        putS "\n\n"
   when (not genstepdefs)$ putS  "*/\n"


   when (not old_05_api) $ do 
     maybeComment [t"", t"", t"Forward declarations for private contexts."] $
	forM_ stepls $ \stp -> 
	   when (shouldWrapStep stp) $
	     putD$ t"class " <> privcontext stp <> semi <> t"\n"
     putS$ "\n"

   ------------------------------------------------------------
   -- Prototype for user step wrappers
   ------------------------------------------------------------   
   let
--       maincontext = t$ appname++"_context_RAW" -- The top-level context (internal)
       maincontext = t$ appname++"_context" -- The top-level context (internal)
       usercontext = t$ appname++"_context_TEMP"   -- The top-level context exposed to the user
   let stepwrapper stp = textAtom stp <> t"_step_wrapper"

   when (not old_05_api)$ do
     putS  "// Forward declaration of the context class (representing the CnC graph)\n"
     putD$ t"class " <> maincontext <> t";"
     putD$ t"class " <> usercontext <> t";"

     maybeComment [t"", t"Type definitions for wrappers around steps."]
	(forM_ stepls_with_types $ \ (stp,ty) -> when (shouldWrapStep stp) $ do
	    putD$ struct (stepwrapper stp) $ 
	      textAtom stp <+> t"m_step" <> semi $$
	      --stepwrapper stp <> parens empty <> semi
	      t"int execute(" <+> constRefType ty <+> t "tag," <+> maincontext <> t" & c) const;"
	    putS$ "\n")

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
         t "CnC::step_collection" <> angles (if shouldWrapStep stp then stepwrapper stp else textAtom stp)
	   <+> (t$ step_obj$ fromAtom stp) <> semi
       )) ++      

     t "": t "// Tag collections members:" : 
     ((flip map) (AM.toList tags) $ \ (tg,mty) -> 
       case mty of 
         Nothing -> error$ "CppOld Codegen: tag collection without type: "++ (fromAtom tg)
         Just ty -> text "CnC::tag_collection" <> angles (cppType ty) <+> textAtom tg <> semi
     ) ++ 

     [space, t "// Item collections members:" ] ++
     ((flip map) (AM.toList items) $ \ (itC,mty) -> 
       case mty of 
         Nothing -> error$ "CppOld Codegen: item collection without type: "++ (fromAtom itC)
         Just (ty1,ty2) -> trace ("ITEM COLLECTION WITH TYPE" ++ show (ty1,ty2)) $
                           t "CnC::item_collection" <> 
                           let extra = 
                                case ty1 of  
                                  -- In the case where the tag type is dense in all dimensions, turn on CNC_VECTOR:
                                  TDense _ -> commspc<> t"CnC::cnc_tag_hash_compare" <> angles (cppType ty1) <>commspc<> t"CnC::CNC_VECTOR"
                                  _        -> empty
                           in angles (cppType ty1 <>commspc<> cppType ty2 <> extra) <+> textAtom itC <> semi
     ) ++ 
     
     [execEasyEmit$ 
       maybeComment [t"", t" Keys for thread local storage:"] 
         (forM_ stepls $ \stp -> 
	   when (shouldWrapStep stp) (putD$ t"int "<> tls_key stp <> semi))] ++ 

     [maybeCommentDoc [t"",t"Next, global state used/generated by plugins:"]
        (execEasyEmit$ 
	 forM_ (AM.toList plug_map) $ \ (stpC,hooks) ->
	   sequence_ $ map (fst . addGlobalState) hooks
	)] ++

     [space, t "// The context class constructor (prototype): "] ++
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
   -- Emit the tuner for each step collection:
   ------------------------------------------------------------   

   maybeComment [t"", t"Automatically generated tuners, where possible."]$
     when gendepends $ 
       forM_ stepls_with_types $ \ (stp,ty) -> 
	 when (AS.member stp tractible_depends_steps) $
	 do
  --	      cname = 
	    putD$ struct (tuner_name stp <> t" : public CnC::default_tuner< int, tagfun_depends_context >") $ 
	      t"bool preschedule() const { return false; }" $$
	      t"template< class dependency_consumer >" $$
	      (execEasyEmit$ 
		 constFunDef (TSym$ toAtom "void") (s"depends") 
			[TConst$ TRef$ ty, 
			 TRef$ TSym$ toAtom$ render maincontext, 
			 TRef$ TSym$ toAtom$ "dependency_consumer"] $ 
			\ (Syn tag, contextref, deps) -> do
			  comm "BODY"
			  let dep = function$ Syn$t$ synToStr$ deps `dot` s"depends"
			  -- Now iterate through all data collections connected to the step collection:
			  forM_ (lpre graph$ realmap M.! (CGSteps stp)) $ \ (nd, tgfn) -> 
			    case lab graph nd of 
			     Just (CGItems itC) -> do
			       comm$ "Dependency on collection "++ (graphNodeName$ fromJust$ lab graph nd) ++", tagfun " ++ show tgfn
			       case tgfn of 
				 Just fn -> forM_ (applyTagFun fn tag) $ \ resulttag -> 
					      EE.app dep [contextref `dot` Syn (toDoc itC), Syn resulttag]
				 Nothing -> comm "Ack.. tag function not available..."
			     _ -> return ()
	       )

-- public CnC::default_tuner< int, tagfun_depends_context > // TODO3: derive from default tuner


   --------------------------------------------------------------------------------
   -- Emit private contexts for each step.  This means building wrappers.
   --------------------------------------------------------------------------------
   
   -- INVARIANT! Due to error checking above, we can omit some error checking here:
   when (not old_05_api) $ do 
     maybeComment [t"", t"", t"Next we define 'private contexts'.",
                   t"These allow steps to exist in their own little universes with special properties:"] $
      forM_ stepls_with_types $ \ (stp,stpty) -> when (shouldWrapStep stp) $ do
        generate_wrapper_context config spec stp stpty plug_map maincontext

   ------------------------------------------------------------
   -- Execute wrapper methods 
   ------------------------------------------------------------   

   maybeComment [t"", t"", t"Execute method wrappers for each step that uses a private context:"] $
    forM_ stepls_with_types $ \ (stp,ty) -> when (shouldWrapStep stp) $ do
      putD$ hangbraces 
	(t"int "<> stepwrapper stp <> t"::execute(" <+> constRefType ty <+> t "tag," <+> maincontext <> t" & c) const")
	indent $
	t"// To access the (thread local) state we fetch a pointer to the private context object:"$$
	assignCast (mkPtr$ privcontext stp) (t"ptr") 
		   (U.app "CnC::Internal::CnC_TlsGetValue" [deref (t"c") (tls_key stp)]) $$ 

	hangbraces (t"if " <> parens (t"!ptr")) indent
		   (assign "ptr" (t"new " <> U.app (privcontext stp) ["c"]) $$
		    U.app "CnC::Internal::CnC_TlsSetValue" [deref (t"c") (tls_key stp), t"ptr" ] 
		    <> semi) $$

	t"ptr->tag = &tag;\n" $$
	-- t"printf(\"PTR %p\\n\", ptr);" $$

	(execEasyEmit$ 
	 forM_ (plug_map AM.! stp) $ \hks -> 
	   beforeStepExecute hks (Syn$ t"ptr", Syn$t"c") (Syn$ t"tag", Syn$ t"ptr", Syn$t"c")) $$
	t"int result = " <+> U.app "m_step.execute" ["tag", "*ptr"] <> semi $$
	(execEasyEmit$ 
	 forM_ (plug_map AM.! stp) $ \hks -> 
	   afterStepExecute hks (Syn$ t"ptr", Syn$t"c") (Syn$ t"tag", Syn$ t"ptr", Syn$t"c")) $$
	t"return result;"

      -- privcontext stp <> t"* ptr = ("<> privcontext stp 
      --  <> t"*) CnC::Internal::CnC_TlsGetValue" <> parens (t"c." <> tls_key stp) <> semi


   ------------------------------------------------------------
   -- Main Context constructor
   ------------------------------------------------------------
   putS "\n\n"
   putS "// Finally, define the constructor for the main context:\n"
   when (areAnyWrapped && not old_05_api)$ putS "// (Note that this occurs AFTER the private contexts are defined.)\n"
   putD $ 
     (hangbraces 
       (maincontext <> t"::" <> maincontext <> parens empty <+> colon $$
     	-- Initializer list:
     	(nest 6 $ vcat $ punctuate commspc $ 

         (if old_05_api then [] else  
     	   t "// Initialize step collections" :
           ((flip map) stepls $ \ stp -> 
	      -- Disabling this:
	      -- t"// "<> privcontext_member stp <> parens (t"new "<> privcontext stp <> parens (t"*this")) <> commspc $$

     	      (t$ step_obj$ fromAtom stp) <> parens (t"this") 
     	   )) ++

     	 t "// Initialize tag collections" :
     	 ((flip map) (AM.toList tags) $ \ (tg,Just ty) -> 
     	   textAtom tg <> parens (t "this, false") 
     	 ) ++ 
     	 t "// Initialize item collections" :
     	 ((flip map) (AM.toList items) $ \ (itC,Just (ty1,ty2)) -> 
     	   textAtom itC <> parens (t "this")
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
				     ++ if gendepends && AS.member stp tractible_depends_steps
				        then [thunkapp (tuner_name stp)]
				        else []
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
	      if shouldWrapStep stp
	      then assign (tls_key stp) (thunkapp "CnC::Internal::CnC_TlsAlloc")
	      else empty)

        ] ++
	--------------------------------------------------------------------------------
	-- FIXME:
	-- This should be completely obsoleted when the API catches up (e.g. trace_all works properly)
	[hangbraces (t"if " <> parens (if gentracing then t"1" else t"0")) indent (vcat $
	  ((flip map) (zip3 stepls prescribers tagtys) $ \ (stp,tg,ty) ->
	     U.app "CnC::debug::trace" [(text$ step_obj$ fromAtom stp), (dubquotes stp)] <> semi) ++
	  ((flip map) (AM.toList tags) $ \ (tg,_) -> 
	   U.app "CnC::debug::trace" [textAtom tg, dubquotes tg] <> semi) ++
	  ((flip map) (AM.toList items) $ \ (itC,_) -> 
	   U.app "CnC::debug::trace" [textAtom itC, dubquotes itC] <> semi) 
	 )] ++
	--------------------------------------------------------------------------------

        [nest 6$ execEasyEmit$
	 (maybeComment [t"",t"Finally global state added by plugins is initalized:"] $
	   forM_ (AM.toList plug_map) $ \ (stpC,hooks) ->
	     sequence_ $ map (snd . addGlobalState) hooks )]
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
------------------------------------------------------------------------------------------------------------------------

-- NOTE: This routine is multiplexed so that it can generate a wrapper
-- either for the top-level context (for use by the environment) or for individual step's contexts.
-- To call for the environment, simply call with name (stp) = special_environment_name
-- 
--generate_wrapper_context :: Atom -> Type -> EasyEmit ()
generate_wrapper_context (CodeGenConfig{gendebug})
			 (spec @ CncSpec{appname, steps, tags, items, graph, realmap}) 
			 stp stpty plug_map maincontext = 
   let 
       fprintf = function$ Syn$t "fprintf"
       stderr  = constant "stderr"
       abort   = function$ Syn$t "abort"

       forEnvironment = (stp == toAtom special_environment_name)

   in do cppClass (Syn$ privcontext stp) (Syn empty)
	  (do 
           putS "\n  public:\n"

	   comm "First, state for the private context.  Note one private context object is instantiated for EACH thread."
	   --putD$ t"  const "<> (cppType$ TPtr stpty) <+> t"tag" <> semi <> t" // Cache the tag for each step instance." 
	   comm "Cache the tag for each step instance:" 
	   var (TConst$ TPtr stpty) (s "tag")

           -- Execute plugin hooks:
	   maybeComment [t"",t"Next, collection/thread local state used/generated by plugins:"] $
              sequence_$ map (fst . addLocalState) (plug_map AM.! stp)

           ------------------------------------------------------------   
	   -- Wrap tag collections:
           ------------------------------------------------------------   
           comm ""
	   comm "Wrappers for tag collections:" 

-- FIXME FIXME TODO TODO: Factor this to merge it with Item collections.
	   --putD$ t"// Tag collections are simply aliases to the parents' (NO WRAPPING YET):" 
	   forM_ (AM.toList tags) $ \ (tgC, Just ty) -> 
	     --var (TRef TTemplate "CnC::tag_collection" ty) (atomToSyn tgC)
	     -- Unwrapped option, just a reference:
	     --putD$ text "CnC::tag_collection" <> angles (cppType ty) <> t" & " <> textAtom tgC <> semi
             let wrapper = textAtom tgC <> t"_wrapper"
	         member  = t"m_" <> textAtom tgC
	     in
	     cppClass (Syn wrapper) (s"")
		   (do putS "public:"  
		       var (TPtr$ doc2Ty$ privcontext stp) (s"m_context")
		       var (TRef$ doc2Ty$ maincontext) (s"parent_context")

		       putD$ t"CnC::tag_collection" <> angles (cppType ty) <> t" & " <> member <> semi $$ t"" 
		       comm "The constructor here needs to grab a reference from the main context:"

		       cppConstructor (Syn wrapper)
			    [Syn$ param (mkRef maincontext)      (t"p"),
			     Syn$ param (mkPtr$ privcontext stp) (t"c")] -- Args.
			    [(Syn$ member, Syn$ t"p." <> textAtom tgC),
			     (s"parent_context", s"p"),
			     (s"m_context", s"c")]  -- Initialization.
			    (do return ()
			        -- set (s"m_context")      (s"c")           -- Body.
			        -- set (s"parent_context") (s"&p")
			        )

		       inlineFunDef (TSym$ toAtom "void") (strToSyn "put") [TConst$ TRef ty] $ \ tagArg -> 
		         do let doplugs project = 
				 forM_ (plug_map AM.! stp) $ \ hooks ->
				    project hooks (s"m_context", s"parent_context", tgC)
				 	          tagArg
			    doplugs beforeTagPut
			    EE.app (function$ Syn$ t "m_"<> textAtom tgC <> t".put") [tagArg]
			    doplugs afterTagPut
			     
		       return ()
-- FIXME FIXME TODO TODO: Factor this to merge it with Item collections.
		       ) 

  	   comm ""
 	   comm "PUBLIC MEMBERS: Tag collections are all wrapped for now:" 
	   forM_ (AM.toList tags) $ \ (tgC, Just ty1) -> 
	     (putD$ textAtom tgC <> t"_wrapper" <+> textAtom tgC <> semi)

           ------------------------------------------------------------   
	   -- Wrap item collections:
           ------------------------------------------------------------   
	   -- FIXME: No reason to wrap ALL item collections.

           comm ""
	   comm "Wrappers for tag collections:" 

           forM_ (AM.toList items) $ \ (itC,Just (ty1,ty2)) -> 
	    (
	    let tagty = cppType ty1

	        -- A reused bit of syntax for wrapper get/put methods:
 	        -- (This is one of those things that you don't want to duplicate, 
		--  but it has too many arguments and is poorly abstracted.)
	        wrapGP doret retty nm args isPut = 
		       -- First let's put together the function's arguments:
                       let args' = map (\ (tyD,v) -> tyD <+> text v) args 
			   decls = commacat args'
			   vars  = commacat $ map (text . snd) args
			   doplugs project = execEasyEmit$ 
		               forM_ (plug_map AM.! stp) $ \ hooks ->
				   project hooks (Syn$ t$ snd$ head args, Syn$ t$ snd$ head$tail args, itC)
					         (Syn$ t$ snd$ head args, Syn$ t$ snd$ head$tail args)
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
				           execEasyEmit$
				           case ls of 
				             -- If we have no relationship to that collection its an error to access it:

				             [] -> do let str = printf "CnC graph violation: should not access collection '%s' from '%s'" 
							               (graphNodeName target :: String) (show stp) 
						      EE.app fprintf [stderr, stringconst$ str]
				                      EE.app abort[]

				             [(_,Nothing)] -> comm "No tag function to check..."

				             --((_,Just (TF args exps)) : []) -> 
					     
				             ((_,Just (TF args exps)) : tl) -> -- TEMPTOGGLE ... permissive, ignoring additional tag functions!
					       case (args,exps) of  
					         ([arg], [exp]) -> 
						    do comm ("Checking tag function "++ show arg ++" -> "++ show exp)
						       -- TODO: use normal variable decl here:
						       putD$ tagty <+> (fromAtom arg) <+> t"=" <+> t"* m_context->tag" <> semi
						       assert (Syn (t"tag") EE.== Syn (pPrint exp))
						 _ -> error "internal error: tag function correctness not fully implemented yet.  Finish me."

				             _ -> error$ "internal error: tag function correctness codegen: \n w"++show ls
				  in				  
				  (if gendebug -- Optionally include debugging assertions.
        	  	            then checkTagFun stp (CGItems itC) (if isPut then lpre' else lsuc') 
				    else empty) $$
				  --------------------------------------------------------------------------------
				  -- TODO: Factor tagfun correctness into a Plugin
				  --------------------------------------------------------------------------------
				  -- Execute plugin hooks:
				  -- FIXME: This should be put OR get..
				  (if isPut 
				   then doplugs beforeItemPut
				   else doplugs beforeItemGet) $$
				  --------------------------------------------------------------------------------
				  (if doret then t"return " else t"") <>
				  t "m_"<> textAtom itC <> t"." <> t nm <> parens vars <> semi $$
				  --------------------------------------------------------------------------------
				  (if isPut 
				   then doplugs afterItemPut
				   else doplugs afterItemGet)
				 )

                -- Basic get or put:
		basicGP nm isPut = wrapGP False "void" nm [(mkConstRef tagty, "tag"), 
							   ((if isPut then mkConstRef else mkRef) (cppType ty2) , "ref")] isPut

	        wrapper = textAtom itC <> t"_wrapper"
	        member  = t"m_" <> textAtom itC
	    in do 
	    comm "The item collection wrapper: A 'NoOp' wrapper class that does nothing: "
	    cppClass (Syn wrapper) (s"")
	          (do putS "public:"  
		      putD$ mkPtr (privcontext stp) <> t" m_context;" 
		      putD$ mkPtr maincontext <> t" parent_context;\n" 

		      putD$ t"CnC::item_collection" <> angles (cppType ty1 <>commspc<> cppType ty2) <> t" & " <> member <> semi $$ t"" 
                      comm "The constructor here needs to grab a reference from the main context:"
		      putD (constructor wrapper 
		                   [param (mkRef maincontext)      (t"p"),
				    param (mkPtr$ privcontext stp) (t"c")]
		                   [(member, t"p." <> textAtom itC)]
		                   (assign "m_context" "c" $$ 
		                    assign "parent_context" "&p"))
		      -- Just three methods: two variants of get and one put.
		      putD$ basicGP "get" False 
		      putD$ basicGP "put" True 
		      putD$ if oldstyle_get_method 
		            then wrapGP True  (cppType ty2) "get" [(mkConstRef (cppType ty1),"tag")] False
		            else empty
		      ) 
            putS "")

           -- Declare MEMBERS
  	   comm ""
 	   comm "PUBLIC MEMBERS: Item collections are all wrapped for now:" 
	   forM_ (AM.toList items) $ \ (itC, Just (ty1,ty2)) -> 
	     (putD$ textAtom itC <> t"_wrapper" <+> textAtom itC <> semi)

           ------------------------------------------------------------   

	   comm ""
	   comm "Constructor for the private/custom context: "
	   cppConstructor  
	       (Syn$ privcontext stp)         -- Name.
	       [Syn$ maincontext <> t" & p"]  -- Args.
	       -- Lots of initializers:
	       (((flip map) (AM.toList items) $ \ (itC, Just (ty1,ty2)) -> 
		 (atomToSyn itC, s"p, this")
		)  ++		
		((flip map) (AM.toList tags) $ \ (tgC, Just ty) -> 
		 --(atomToSyn tgC, s"p" `dot` atomToSyn tgC)
		 (atomToSyn tgC, s"p, this") -- This is for the new, WRAPPED, tag collection.
		))
	       (maybeComment [t"",t"Next, collection/thread-local state is initalized:"] $
		sequence_$ map (snd . addLocalState) (plug_map AM.! stp)
	       )
	   -- Have to wrap tag collections as well just to redirect references to the main context:
	  )
	 putS$ "\n"
	 putS$ "\n"



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
    
--------------------------------------------------------------------------------
-- EXAMPLE: OLD CODING HINTS:

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