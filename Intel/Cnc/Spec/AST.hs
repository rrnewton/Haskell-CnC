{-# LANGUAGE DeriveDataTypeable  #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Intel.Cnc.Spec.AST where 

import StringTable.Atom
import Data.Data
import Data.List
import Data.Char
import Text.PrettyPrint.HughesPJClass
--import Data.Generics.Serialization.SExp
--import Data.Generics.Serialization.Streams
import Intel.Cnc.Spec.Util
import Intel.Cnc.Spec.SrcLoc

-- Export this friendly shortcut:
pp x = pPrint x -- Eta expand, monomorphism restriction.

-- For now there is exactly one predefined step collection:
isBuiltin "env" = True
isBuiltin _     = False

-- Everything that is decorated with annotations (e.g. source
-- locations) should be able to provide them or strip them.
-- This replicates most of the benefit of using a "Located" type.
--
-- Some generic programming could probably provide this for free.
class Decorated t where 
  mapDecor   :: (a -> b) -> t a -> t b
  getDecor   :: t a -> a
  stripDecor :: t a -> t ()
  stripDecor = mapDecor (\_ -> ())

-- Oops, I don't know how to compose type constructors in a "curried" way:
--instance Decorated t => Decorated ([] . t) where 
--  stripDecor = map stripDecor
--  getDecor = 

--------------------------------------------------------------------------------
-- Expressions and Literals
--------------------------------------------------------------------------------

-- Expressions are the code used inside step skeletons.

data Lit = 
   LitInt Int 
 | LitFloat Float
 deriving (Eq, Ord, Show, Data, Typeable)

-- Expressions are decorated with values of an arbitrary type:
data Exp dec = 
   Lit dec Lit
 | Var dec Atom
 | App dec (Exp dec) [Exp dec]
 | If  dec (Exp dec) (Exp dec) (Exp dec) 
 deriving (Eq, Ord, Show, Data, Typeable)


instance Pretty Lit where 
 pPrint (LitInt i)   = pPrint i
 pPrint (LitFloat f) = pPrint f

instance Pretty (Exp dec) where 
 pPrint (Lit _ l) = pPrint l
 pPrint (Var _ s) = text (fromAtom s)
 pPrint (App _ rator rands) = 
     case (rator,rands) of 
       -- If it's a binop we should print appropriately.
       (Var _ name, [left,right]) | (not $ isAlpha (head (fromAtom name))) -> 
	     -- Sep can actually yield some very wierd behavior:
--	     pPrint left `sep2` text name `sep2` pPrint right
	     parens (pp left <+> text (fromAtom name) <+> pp right)
       _ ->  pPrint rator <> (parens $ commasep rands)

 pPrint (If _ a b c) = 
     sep [text "if (" <> pPrint a <> text ")",
          nest 5 $ pPrint b, 
	  text "else " <> pPrint c]


commacat ls = hcat (intersperse (text ", ") $ map pPrint ls)
commasep ls = sep (intersperse (text ", ") $ map pPrint ls)

instance Decorated Exp where 
  mapDecor f e = 
   case e of 
    Lit s l        -> Lit (f s) l
    Var s v        -> Var (f s) v
    App s r d      -> App (f s) (mapDecor f r) (map (mapDecor f) d)
    If  s a b c    -> If  (f s) (mapDecor f a) (mapDecor f b) (mapDecor f c)

  -- This is the price of tagging the source locations (and other
  -- decorations) right on the Exprs rather than the even/odd
  -- alternating expression/decoration types.
  getDecor e = 
   case e of 
    Lit s _        -> s
    Var s _        -> s
    App s _ _      -> s
    If  s _ _ _    -> s

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

data Type =
   TInt
 | TFloat
 -- An abstract type not intpreted by CnC:
 | TSym Atom
 | TPtr Type
 | TDense Type -- Density annotations.
 | TTuple [Type]
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (Type) where
 pPrint (TInt)      = text "int"
 pPrint (TFloat)    = text "float"
 pPrint (TSym str)  = text (fromAtom str)
 pPrint (TPtr ty)   = pPrint ty <> text "*"
 pPrint (TDense ty) = text "dense" <+> pPrint ty 
 pPrint (TTuple ty) = text "(" <> commacat ty <> text ")"


-- Converting types to C++ concrete syntax.
cppType :: Type -> Doc
cppType ty = case ty of 
  TInt   -> t "int"
  TFloat -> t "float"
  TSym s -> textAtom s
  TPtr ty -> cppType ty <> t "*"

  -- This doesn't affect the C-type, any influence has already taken place.
  TDense ty -> cppType ty 

  -- Here is the convention for representing tuples in C++.
  --TTuple [a,b]   -> t "Pair"   <> angles (hcat$ punctuate commspc (map cppType [a,b]))
  --TTuple [a,b,c] -> t "Triple" <> angles (hcat$ punctuate commspc (map cppType [a,b,c]))
  TTuple ls -> t "cnctup::tuple" <> angles (hcat$ punctuate commspc$ map cppType ls)
  --TTuple ls -> error$ "CppOld codegen: Tuple types of length "++ show (length ls) ++" not standardized yet!"


----------------------------------------------------------------------------------------------------
-- Top level Statements in a .cnc file:
----------------------------------------------------------------------------------------------------

-- These are the statements produced by parsing (hence 'P')
-- They get converted to a different format post-parsing
data PStatement dec = 
   -- When we parse a file we allow statements to be arbitrarily long chains of relations:
   -- We represent this as a starting instance(s) followed by an arbitrary number of links.
   Chain [CollectionInstance dec] [RelLink dec]
 | DeclareTags  dec Atom (Maybe Type)
 | DeclareItems dec Atom (Maybe (Type, Type))
 | DeclareSteps dec Atom 

 -- Type synonyms are of kind * for now...
 | TypeDef dec Atom Type

 | Function
 | DeclareExtern
 | Constraints  dec (CollectionInstance dec) [Exp dec]

 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (PStatement dec) where 
 pPrint (Chain first rest) = 
     commacat first <+>
     hsep (map pPrint rest) <> text ";\n"
 pPrint (DeclareTags _ name Nothing)   = text "tags " <> text (fromAtom name) <> text ";\n"
 pPrint (DeclareTags _ name (Just ty)) = text "tags<" <> pPrint ty <> text "> " <> text (fromAtom name) <> text ";\n"
 pPrint (DeclareItems _ name Nothing)  = text "items" <+> text (fromAtom name) <> text ";\n"
 pPrint (DeclareItems _ name (Just (ty1,ty2))) = 
     text "items<" <> pPrint ty1 <> comma <+> pPrint ty2 <> text "> " <> text (fromAtom name) <> text ";\n"
 pPrint (DeclareSteps _ name) =  text "steps " <> text (fromAtom name) <> text ";\n"
 pPrint (Constraints _ inst exps) = text "constrain " <> pp inst <+> 
				    hcat (punctuate (text ", ") $ map pp exps) <> text ";\n"

 pPrint (TypeDef _ nm ty) = text "type"<+> pPrint nm <+> char '=' <+> pPrint ty <> text ";\n"

 pPrint (Function )     = text "FUNCTION NOT WORKING YET"
 pPrint (DeclareExtern) = text "DECLARE EXTERN NOT WORKING YET"


--instance Pretty [PStatement dec] where 
-- pPrint ls = vcat (map pPrint ls)

instance Decorated PStatement where 
  mapDecor f stmt = 
   case stmt of 
     Chain insts links -> Chain (map (mapDecor f) insts) (map (mapDecor f) links)
     DeclareTags  s name ty -> DeclareTags  (f s) name ty
     DeclareItems s name ty -> DeclareItems (f s) name ty
     DeclareSteps s name    -> DeclareSteps (f s) name
     Constraints  s inst ls -> Constraints  (f s) (mapDecor f inst) (map (mapDecor f) ls)

     Function -> Function 
     DeclareExtern -> DeclareExtern 
     TypeDef      s name ty -> TypeDef      (f s) name ty


  getDecor stmt = 
   case stmt of 
     Chain (hd:_) _  -> getDecor hd
     Chain [] (hd:_) -> getDecor hd
     Chain [] []     -> error "getDecor: cannot get decoration from an empty 'Chain'.  Shouldn't have such a thing anyway."
     DeclareTags  s _ _ -> s
     DeclareItems s _ _ -> s
     DeclareSteps s _   -> s
     Constraints  s _ _ -> s
     TypeDef      s _ _ -> s
     Function      -> error "getDecor: not implemented yet"
     DeclareExtern -> error "getDecor: not implemented yet"


------------------------------------------------------------
data RelLink dec = 
   ProduceLink    dec [CollectionInstance dec]
 | PrescribeLink  dec [CollectionInstance dec]
 | RevProduceLink dec [CollectionInstance dec]
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (RelLink dec) where
 pPrint (ProduceLink    _ ls) = text "->" <+> commacat ls 
 pPrint (PrescribeLink  _ ls) = text "::" <+> commacat ls 
 pPrint (RevProduceLink _ ls) = text "<-" <+> commacat ls 

instance Decorated RelLink where 
  mapDecor f link = 
    case link of 
     ProduceLink    s ls -> ProduceLink    (f s) (map (mapDecor f) ls)
     PrescribeLink  s ls -> PrescribeLink  (f s) (map (mapDecor f) ls)
     RevProduceLink s ls -> RevProduceLink (f s) (map (mapDecor f) ls)  
  getDecor link = 
    case link of 
     ProduceLink    s _ -> s
     PrescribeLink  s _ -> s
     RevProduceLink s _ -> s

------------------------------------------------------------
data CollectionInstance dec = 
   InstName dec String
   -- TEMP: until the syntax has been figured out we sometimes know
   -- that an instance is a Step OR a Tag collection, but not which.
 | InstStepOrTags dec String [Exp dec]

 | InstTagCol     dec String [Exp dec]
 | InstItemCol    dec String [Exp dec]
 | InstStepCol    dec String [Exp dec]

 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (CollectionInstance dec) where 
  pPrint (InstName _ s) = text s
--  pPrint (InstItemCol    s exps) = text s <> pPrint exps -- WEIRD indent behaviour, commasep is ALSO weird
  pPrint (InstStepOrTags _ s exps) = text s <> parens   (commacat exps)
  pPrint (InstItemCol    _ s exps) = text s <> brackets (commacat exps)
  pPrint (InstStepCol    _ s exps) = text s <> parens   (commacat exps)
  pPrint (InstTagCol     _ s exps) = text s <> angles   (commacat exps)


instance Decorated CollectionInstance where 
  mapDecor f inst = 
    case inst of 
      InstName       s n    -> InstName       (f s) n 
      InstStepOrTags s n ls -> InstStepOrTags (f s) n (map (mapDecor f) ls)
      InstItemCol    s n ls -> InstItemCol    (f s) n (map (mapDecor f) ls)
      InstStepCol    s n ls -> InstStepCol    (f s) n (map (mapDecor f) ls)
      InstTagCol     s n ls -> InstTagCol     (f s) n (map (mapDecor f) ls)
  -- We follow a "left hand rule" for data structures that are not decorated but have decorated children.
  -- That is, pick the left-most child that has a decoration.
  getDecor inst = 
    case inst of 
      InstName       s _   -> s
      InstStepOrTags s _ _ -> s
      InstItemCol    s _ _ -> s
      InstStepCol    s _ _ -> s
      InstTagCol     s _ _ -> s

----------------------------------------------------------------------------------------------------
-- Math with Tags:
----------------------------------------------------------------------------------------------------

-- Tag expressions are distinct from Exp and much more restrictive.
-- (For example, conditionals are not allowed.)

data TagExp = 
   TEVar Atom
 | TEInt Int -- Is there any conceivable need for arbitrary precision here?
 | TEApp Atom [TagExp]
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty TagExp where 
  pPrint te =  case te of
    TEVar s  -> text (fromAtom s)
    TEInt n  -> text (show n)
--    TEApp s rands -> text (fromAtom s) <> parens (commacat rands)
    TEApp rat rands -> 
     case rands of 
       [left,right] | not $ isAlpha (head (fromAtom rat)) -> 
     	     parens (pp left <+> text (fromAtom rat) <+> pp right)
       _ ->  text (fromAtom rat) <> (parens $ commasep rands)

-- A tag function of dimension N has N formal parameters and N "bodies".
data TagFun = TF [Atom] [TagExp]
  deriving (Eq, Ord)

instance Pretty TagFun where 
  pPrint (TF formals bods) =
    text "\\" <> hsep (map (text . fromAtom) formals) <+> text "->" <+> 
    commacat bods

-- Again, might as well use the pretty version
instance Show TagFun where 
  show = show . pp


-- The left argument is the "formal parameter" and the right argument the "body".
-- But really it's just an equality.
-- Note, the reason these are Exp LISTS is because these are implicitly tuples.
-- The tag functions are multi-dimensional.
mkTagFun exps1 exps2 = 
 let e1s = Prelude.map checkConvertTF exps1
     e2s = Prelude.map checkConvertTF exps2
 in if all isTEVar e1s
    then if not (Prelude.null exps1) && not (Prelude.null exps2)
	     -- length exps1 == length exps2 
         then Just (TF (Prelude.map unTEVar e1s) e2s)
	 -- Otherwise there is a mismatch in the number of tag components:
	 else if Prelude.null exps2 
	      then Nothing -- It's ok to simply leave off a tag function (but to have some var names on the step).
	      else error$ "ERROR:\n   It is not acceptable to use the following tag components without\n"++
		          "   tag components indexing the step: "
--		          "   the same number of corresponding tag components indexing the step: "
		          ++ (show$ pp exps2) ++ 
			  showSpanDetailed (foldl1 combineSrcSpans $ Prelude.map getDecor exps2)

    else error$ "Presently the tag expressions indexing step collections must be simple variables, not: " 
	        ++ (show$ pp exps1)

isTEVar (TEVar _) = True
isTEVar _ = False

-- Avoid exhaustiveness warnings here:
unTEVar = \ (TEVar name) -> name

-- This is where we convert arbitrary Exps into more restricted tag expressions that
-- support symbolic manipulation.
checkConvertTF e = 
  case e of 
    Lit s l -> case l of 
	         LitInt i -> TEInt i; 
		 _ -> locErr s "Only integer literals supported in tag functions presently."
    Var s name -> TEVar name 
    App _ (Var _ name) rands -> TEApp name (Prelude.map checkConvertTF rands)
    App s _ _  -> locErr s "Only very simple function applications allowed in tag functions presently."
    If s _ _ _ -> locErr s "Conditionals disallowed in tag functions."



----------------------------------------------------------------------------------------------------
-- CnC Graph Representation:
----------------------------------------------------------------------------------------------------

-- This is the final representation for a CnC Graph:

