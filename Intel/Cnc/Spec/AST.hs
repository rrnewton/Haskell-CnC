{-# LANGUAGE DeriveDataTypeable  #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Intel.Cnc.Spec.AST where 
--module Intel.Cnc.Translator.AST where 

import StringTable.Atom
import Data.Data
import Data.List
import Data.Char
import Text.PrettyPrint.HughesPJClass
import Data.Generics.Serialization.SExp
import Data.Generics.Serialization.Streams
--import SrcLoc

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
 | TTuple [Type]
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (Type) where
 pPrint (TInt)      = text "int"
 pPrint (TFloat)    = text "float"
 pPrint (TSym str)  = text (fromAtom str)
 pPrint (TPtr ty)   = pPrint ty <> text "*"
 pPrint (TTuple ty) = text "(" <> commacat ty <> text ")"

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

  getDecor stmt = 
   case stmt of 
     Chain (hd:_) _  -> getDecor hd
     Chain [] (hd:_) -> getDecor hd
     Chain [] []     -> error "getDecor: cannot get decoration from an empty 'Chain'.  Shouldn't have such a thing anyway."
     DeclareTags  s _ _ -> s
     DeclareItems s _ _ -> s
     DeclareSteps s _   -> s
     Constraints  s _ _ -> s

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
   InstName String
   -- TEMP: until the syntax has been figured out we sometimes know
   -- that an instance is a Step OR a Tag collection, but not which.
 | InstStepOrTags String [Exp dec]
 | InstItemCol    String [Exp dec]
 | InstTagCol    String [Exp dec]
 | InstStepCol   String [Exp dec]

 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (CollectionInstance dec) where 
  pPrint (InstName s) = text s
--  pPrint (InstItemCol    s exps) = text s <> pPrint exps -- WEIRD indent behaviour, commasep is ALSO weird
  pPrint (InstItemCol    s exps) = text s <> brackets (commacat exps)
  pPrint (InstStepOrTags s exps) = text s <> parens   (commacat exps)


instance Decorated CollectionInstance where 
  mapDecor f inst = 
    case inst of 
      InstName        n    -> InstName n 
      InstItemCol    n ls -> InstItemCol    n (map (mapDecor f) ls)
      InstStepOrTags n ls -> InstStepOrTags n (map (mapDecor f) ls)
  getDecor inst = 
    case inst of 
      InstItemCol    _ (h:_) -> getDecor h
      InstStepOrTags _ (h:_) -> getDecor h
      InstName        _    -> error "getDecor: collection references aren't currently themselves decorated"
      InstItemCol    _ [] -> error "getDecor: empty item collection reference has no decorations"
      InstStepOrTags _ [] -> error "getDecor: empty collection reference has no decorations"


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


----------------------------------------------------------------------------------------------------
-- CnC Graph Representation:
----------------------------------------------------------------------------------------------------

-- This is the final representation for a CnC Graph:
