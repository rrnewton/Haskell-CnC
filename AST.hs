{-# LANGUAGE DeriveDataTypeable #-}
module AST where 
--module Intel.Cnc.Translator.AST where 

import StringTable.Atom
import Data.Data
import Data.List
import Text.PrettyPrint.HughesPJClass
import Data.Generics.Serialization.SExp
import Data.Generics.Serialization.Streams
--import SrcLoc


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
 | Var dec String
 | App dec (Exp dec) [Exp dec]
 | If  dec (Exp dec) (Exp dec) (Exp dec) 
 deriving (Eq, Ord, Show, Data, Typeable)


instance Pretty Lit where 
 pPrint (LitInt i)   = pPrint i
 pPrint (LitFloat f) = pPrint f

instance Pretty (Exp dec) where 
 pPrint (Lit _ l) = pPrint l
 pPrint (Var _ s) = text s
 pPrint (App _ rator rands) = 
     parens $  
        pPrint rator <+> sep (map pPrint rands)
--      sep (pPrint rator : map pPrint rands)


-- This is the price of tagging the source locations (and other
-- decorations) right on the Exprs rather than the even/odd
-- alternating expression/decoration types.
getExpDecoration e = 
 case e of 
   Lit s _        -> s
   Var s _        -> s
   App s _ _      -> s


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------


data Type =
   TInt
 | TFloat
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (Type) where
 pPrint (TInt)   = text "int"
 pPrint (TFloat) = text "float"

----------------------------------------------------------------------------------------------------
-- Top level Statements in a .cnc file:
----------------------------------------------------------------------------------------------------

-- These are the statements produced by parsing (hence 'P')
-- They get converted to a different format post-parsing
data PStatement dec = 
--   Produce dec [Instance] [Instance] 
   -- When we parse a file we allow statements to be arbitrarily long chains of relations:
   -- We represent this as a starting instance(s) followed by an arbitrary number of links.
   Chain [Instance] [RelLink dec]
 | Function
 | DeclareExtern
 | DeclareTags  dec String (Maybe Type)
 | DeclareItems dec String (Maybe (Type, Type))
 | DeclareSteps dec String 
 deriving (Eq,Ord,Show,Data,Typeable)

commasep ls = hcat (intersperse (text ", ") $ map pPrint ls)

instance Pretty (PStatement dec) where 
 pPrint (Chain first rest) = 
     commasep first <+>
     hsep (map pPrint rest) <> text "\n"
 pPrint (DeclareTags _ name Nothing)   = text "tags " <> text name <> text ";\n"
 pPrint (DeclareTags _ name (Just ty)) = text "tags " <> text name <> pPrint ty <> text ";\n"


--instance Pretty [PStatement dec] where 
-- pPrint ls = vcat (map pPrint ls)

data RelLink dec = 
   ProduceLink      dec [Instance]
 | PrescribeLink    dec [Instance]
 | RevPrescribeLink dec [Instance]
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (RelLink dec) where
 pPrint (ProduceLink _ ls) = text "->" <+> commasep ls <> text ";\n"


data Instance = 
   InstName String
 | InstDataTags    String [TagExp]
 | InstControlTags String [TagExp]
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (Instance) where 
  pPrint (InstName s) = text s
  pPrint (InstDataTags    s exps) = text s <> pPrint exps
  pPrint (InstControlTags s exps) = text s <> parens (commasep exps)


----------------------------------------------------------------------------------------------------
-- Math with Tags:
----------------------------------------------------------------------------------------------------

-- Tag expressions are distinct from Exp and much more restrictive.
-- (For example, conditionals are not allowed.)

data TagExp = 
   TEVar String 
 | TEApp String [TagExp]
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty TagExp where 
  pPrint te =  case te of
    TEVar s       -> text s 
    TEApp s rands -> text s <> parens (commasep rands)


----------------------------------------------------------------------------------------------------
-- CnC Graph Representation:
----------------------------------------------------------------------------------------------------

-- This is the final representation for a CnC Graph:
