{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- This file should contain everything having to do with the structure
-- and analysis of tag functions.

----------------------------------------------------------------------------------------------------
module Intel.Cnc.Spec.TagFun where 

import StringTable.Atom
import Data.Data
import Data.Char
import Text.PrettyPrint.HughesPJClass
import Intel.Cnc.Spec.Util
import Intel.Cnc.Spec.SrcLoc
import Intel.Cnc.Spec.AST
import Debug.Trace

------------------------------------------------------------
-- Type definition.
------------------------------------------------------------
-- Tag expressions are distinct from Exp (in AST) and much more
-- restrictive.  (For example, conditionals are not allowed.)

data TagExp var = 
   TEVar var
 | TEInt Int -- Is there any conceivable need for arbitrary precision here?
 | TEApp Atom [TagExp var]
 deriving (Eq,Ord,Show,Data,Typeable)

------------------------------------------------------------
-- Instances.
------------------------------------------------------------

instance Pretty var => Pretty (TagExp var) where 
  pPrint te =  case te of
    TEVar v  -> pPrint v
    TEInt n  -> text (show n)
    TEApp rat rands -> 
     case rands of 
       [left,right] | not $ isAlpha (head (fromAtom rat)) -> 
     	     parens (pp left <+> text (fromAtom rat) <+> pp right)
       _ ->  text (fromAtom rat) <> (parens $ commasep rands)

-- A tag function of dimension N has N formal parameters and N "bodies".
data TagFun = TF [Atom] [TagExp Atom]
  deriving (Eq, Ord)

instance Pretty TagFun where 
  pPrint (TF formals bods) =
    text "\\" <> hsep (map (text . fromAtom) formals) <+> text "->" <+> 
    commacat bods

-- Again, might as well use the pretty version
instance Show TagFun where 
  show = show . pp

instance Functor TagExp where
  fmap f (TEVar v) = TEVar (f v)
  fmap _ (TEInt i) = TEInt i 
  fmap f (TEApp op ls) = TEApp op $ map (fmap f) ls


------------------------------------------------------------
-- API for tag functions.
------------------------------------------------------------

-- The left argument is the "formal parameter" and the right argument the "body".
-- But really it's just an equality.
-- Note, the reason these are Exp LISTS is because these are implicitly tuples.
-- The tag functions are multi-dimensional.
--
-- TODO: This function could solve simple equations to get the formal
-- parameters all to one side of the equation, allowing things like (2*i -> 3*i).

--mkTagFun :: (CncGraphNode, [Exp SrcSpan]) -> 
--	    (CncGraphNode, [Exp SrcSpan]) -> Maybe TagFun
--mkTagFun (node1,exps1) (node2,exps2) = 
mkTagFun :: String -> [Exp SrcSpan] -> [Exp SrcSpan] -> Maybe TagFun
mkTagFun ctxtmsg exps1 exps2 = 
 let e1s = Prelude.map checkConvertTF exps1
     e2s = Prelude.map checkConvertTF exps2
 in if all isTEVar e1s
    then
         if not (Prelude.null exps1) && not (Prelude.null exps2)
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

    else error$ ctxtmsg ++ ": mkTagFun of  "++ (show$ pp exps1) ++ " and "++ show (pp exps2)++ 
		" - Presently the tag expressions indexing step collections must be simple variables." 



-- Substititution on tag expressions.
substTagExp :: Eq a => a -> a -> TagExp a -> TagExp a
substTagExp old new exp = 
  case exp of 
   TEVar v | v == old  -> TEVar new
	   | otherwise -> TEVar v
   TEInt i -> TEInt i
   TEApp op ls -> TEApp op $ map (substTagExp old new) ls


-- Create a C++ expression that represents the application of the tag function.
-- TODO: Generalize this.
applyTagFun :: TagFun -> Doc -> [Doc]
applyTagFun (TF [formal] [body]) arg =
  [pPrint$ substTagExp formal (toAtom$ render arg) body]

applyTagFun _ _ = error "TODO: applyTagFun implement multidimensional"

isTEVar (TEVar _) = True
isTEVar _ = False

-- Avoid exhaustiveness warnings here:
unTEVar (TEVar name) = name
unTEVar _ = error "unTEVar: not TEVar"

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
