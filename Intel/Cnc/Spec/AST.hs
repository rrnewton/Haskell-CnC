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


--      sep (pPrint rator : map pPrint rands)

commacat ls = hcat (intersperse (text ", ") $ map pPrint ls)
commasep ls = sep (intersperse (text ", ") $ map pPrint ls)
sep2 a b = sep [a,b]


-- This is the price of tagging the source locations (and other
-- decorations) right on the Exprs rather than the even/odd
-- alternating expression/decoration types.
getExpDecoration e = 
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
 | Function
 | DeclareExtern
 | DeclareTags  dec String (Maybe Type)
 | DeclareItems dec String (Maybe (Type, Type))
 | DeclareSteps dec String 
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (PStatement dec) where 
 pPrint (Chain first rest) = 
     commacat first <+>
     hsep (map pPrint rest) <> text ";\n"
 pPrint (DeclareTags _ name Nothing)   = text "tags " <> text name <> text ";\n"
 pPrint (DeclareTags _ name (Just ty)) = text "tags<" <> pPrint ty <> text "> " <> text name <> text ";\n"

 pPrint (DeclareItems _ name Nothing) = text "items " <> text name <> text ";\n"
 pPrint (DeclareItems _ name (Just (ty1,ty2))) = 
     text "items<" <> pPrint ty1 <> text ", " <> pPrint ty2 <> text "> " <> text name <> text ";\n"

 pPrint (DeclareSteps _ name) =  text "steps " <> text name <> text ";\n"


--instance Pretty [PStatement dec] where 
-- pPrint ls = vcat (map pPrint ls)

data RelLink dec = 
   ProduceLink    dec [CollectionInstance dec]
 | PrescribeLink  dec [CollectionInstance dec]
 | RevProduceLink dec [CollectionInstance dec]
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (RelLink dec) where
 pPrint (ProduceLink    _ ls) = text "->" <+> commacat ls 
 pPrint (PrescribeLink  _ ls) = text "::" <+> commacat ls 
 pPrint (RevProduceLink _ ls) = text "<-" <+> commacat ls 

data CollectionInstance dec = 
   InstName String
 | InstDataTags    String [Exp dec]
 | InstControlTags String [Exp dec]
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (CollectionInstance dec) where 
  pPrint (InstName s) = text s
--  pPrint (InstDataTags    s exps) = text s <> pPrint exps -- WEIRD indent behaviour, commasep is ALSO weird
  pPrint (InstDataTags    s exps) = text s <> brackets (commacat exps)
  pPrint (InstControlTags s exps) = text s <> parens   (commacat exps)


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
    TEVar s       -> text (fromAtom s)
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
