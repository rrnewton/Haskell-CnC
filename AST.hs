{-# LANGUAGE DeriveDataTypeable #-}
module AST where 
--module Intel.Cnc.Translator.AST where 

import StringTable.Atom
import Data.Data
import Data.List
import Text.PrettyPrint.HughesPJClass
import Data.Generics.Serialization.SExp
import Data.Generics.Serialization.Streams
import SrcLoc


--------------------------------------------------------------------------------
-- Expressions and Literals
--------------------------------------------------------------------------------

data Lit = 
   LitInt Int 
 | LitFloat Float
 deriving (Eq, Ord, Show, Data, Typeable)

-- Expressions are decorated with values of an arbitrary type:
data Exp dec = 
   Lit dec Lit
 | Var dec String
 | App dec (Exp dec) [Exp dec]
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

-- This is the price of tagging the locs right on the Exprs rather
-- than the even/odd alternating location tags.
getLoc e = 
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


----------------------------------------------------------------------------------------------------
-- Top level Statements in a .cnc file:
----------------------------------------------------------------------------------------------------

-- These are the statements produced by parsing (hence 'P')
-- They get converted to a different format post-parsing
data PStatement dec = 
   Produce dec [Instance] [Instance] 
   -- When we parse a file we allow statements to be arbitrarily long chains of relations:
   -- We represent this as a starting instance(s) followed by an arbitrary number of links.
 | Chain [Instance] [RelLink dec]
 | Prescribe
 | Function
 | DeclareExtern
 | DeclareTags  dec String (Maybe Type)
 | DeclareItems dec String (Maybe (Type, Type))
 | DeclareSteps dec String 
 deriving (Eq,Ord,Show,Data,Typeable)

commasep ls = hcat (intersperse (text ", ") $ map pPrint ls)

instance Pretty (PStatement dec) where 
 pPrint (Produce _ inp out) = 
     commasep inp <+> 
     text "->" <+> 
     commasep out
 pPrint (Chain first rest) = 
     commasep first <+>
     hsep (map pPrint rest) <> text "\n"

instance Pretty (RelLink dec) where
 pPrint (ProduceLink _ ls) = text "->" <+> commasep ls

--instance Pretty [PStatement dec] where 
-- pPrint ls = vcat (map pPrint ls)

data RelLink dec = 
   ProduceLink      dec [Instance]
 | PrescribeLink    dec [Instance]
 | RevPrescribeLink dec [Instance]
 deriving (Eq,Ord,Show,Data,Typeable)

data Instance = 
   IName String
 deriving (Eq,Ord,Show,Data,Typeable)

instance Pretty (Instance) where 
 pPrint (IName s) = text s


----------------------------------------------------------------------------------------------------
-- CnC Graph Representation:
----------------------------------------------------------------------------------------------------

-- This is the final representation for a CnC Graph: