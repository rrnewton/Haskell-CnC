{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Intel.Cnc.Spec.AST where 

import StringTable.Atom
import Data.Data
import Data.Char
import Text.PrettyPrint.HughesPJClass
--import Data.Generics.Serialization.SExp
--import Data.Generics.Serialization.Streams
import Intel.Cnc.Spec.Util
import Intel.Cnc.Spec.SrcLoc

-- For now there is exactly one predefined step collection:
isBuiltin e | e == special_environment_name = True
isBuiltin _     = False

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
 | TRef   Type -- quite annoying, used for C++
 | TConst Type -- quite annoying, used for C++
 | TDense Type -- Density annotations.
 | TTuple [Type]
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (Type) where
 pPrint (TInt)      = text "int"
 pPrint (TFloat)    = text "float"
 pPrint (TSym str)  = text (fromAtom str)
 pPrint (TPtr ty)   = pPrint ty <> text "*"
 pPrint (TRef ty)   = pPrint ty <> text "&"
 pPrint (TDense ty) = text "dense" <+> pPrint ty 
 pPrint (TConst ty) = text "const" <+> pPrint ty 
 pPrint (TTuple ty) = text "(" <> commacat ty <> text ")"


voidTy = TSym (toAtom "void")

-- Converting types to C++ concrete syntax.
cppType :: Type -> Doc
cppType ty = case ty of 
  TInt   -> t "int"
  TFloat -> t "float"
  TSym s -> textAtom s
  TPtr ty -> cppType ty <> t "*"
  TRef ty -> cppType ty <> t " &"
  TConst ty -> t"const" <+> cppType ty 

  -- This doesn't affect the C-type, any influence has already taken place.
  TDense ty -> cppType ty 

-- Here is the convention for representing tuples in C++.
  --TTuple [a,b]   -> t "Pair"   <> angles (hcat$ punctuate commspc (map cppType [a,b]))
  --TTuple [a,b,c] -> t "Triple" <> angles (hcat$ punctuate commspc (map cppType [a,b,c]))
  TTuple ls -> t "cnctup::tuple" <> angles (hcat$ punctuate commspc$ map cppType ls)
  --TTuple ls -> error$ "CppOld codegen: Tuple types of length "++ show (length ls) ++" not standardized yet!"


----------------------------------------------------------------------------------------------------
-- Abstract Syntax for Top level Statements in a .cnc file:
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
