{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- This module contains the simple spec transformation pass that desugars type synonyms (TypeDefs)

-- Certainly this is a candidate for Scrap-your-boilerplate generic programming.

module Intel.Cnc.Spec.Passes.TypeDef where

import Intel.Cnc.Spec.Util
import StringTable.Atom
import qualified StringTable.AtomMap as AM

import Intel.Cnc.Spec.AST
import Test.HUnit
--import Debug.Trace


desugarTypeDefs :: [PStatement dec] -> [PStatement dec]
desugarTypeDefs input = 
    mapTypes desugarTy everything_else 
 where 
  synonyms = AM.fromList$ map (\ (TypeDef _ nm ty) -> (nm,ty)) $
	     filter isdef input
  everything_else = filter (not . isdef) input
  isdef (TypeDef _ _ _) = True
  isdef _ = False

  desugarTy ty = 
    case ty of 
     TInt      -> TInt
     TFloat    -> TFloat
     TSym atom -> case AM.lookup atom synonyms of 
		    Nothing -> TSym atom
		    Just ty -> ty -- Easy, no constructor application
     TPtr ty   -> TPtr   $ desugarTy ty
     TRef ty   -> TRef   $ desugarTy ty
     TDense ty -> TDense $ desugarTy ty
     TConst ty -> TConst $ desugarTy ty
     TTuple ls -> TTuple $ map desugarTy ls

  ----------------------------------------------------------------------------------------------------
  -- Everything below this line is total boilerplate just to traverse the data-types.
  ----------------------------------------------------------------------------------------------------

-- Process all the types, SYB eligible.  
class MapTypes a where
  mapTypes :: (Type -> Type) -> a -> a

instance MapTypes (PStatement dec) where 
  mapTypes fn stmnt = case stmnt of 
     DeclareTags  s nm mty -> DeclareTags  s nm $ fmap fn mty 
     DeclareItems s nm Nothing -> stmnt
     DeclareItems s nm (Just (ty1,ty2)) -> DeclareItems s nm $ Just (fn ty1, fn ty2)
     DeclareReductions s nm op exp Nothing    -> DeclareReductions s nm op (mapTypes fn exp) Nothing
     DeclareReductions s nm op exp (Just (ty1,ty2)) -> DeclareReductions s nm op (mapTypes fn exp) (Just (fn ty1, fn ty2))
     DeclareSteps _ _   -> stmnt
     Function           -> stmnt
     DeclareExtern      -> stmnt
     Chain insts links      -> Chain (mapTypes fn insts) (mapTypes fn links)
     Constraints s inst els -> Constraints s (mapTypes fn inst) (mapTypes fn els)
     TypeDef _ _ _ -> error "serious internal implementation error"

instance MapTypes (CollectionInstance dec) where 
  mapTypes fn inst = case inst of 
   InstName       dec str     -> inst
   InstStepOrTags dec str els -> InstStepOrTags dec str $ mapTypes fn els
   InstTagCol     dec str els -> InstTagCol     dec str $ mapTypes fn els
   InstItemCol    dec str els -> InstItemCol    dec str $ mapTypes fn els
   InstStepCol    dec str els -> InstStepCol    dec str $ mapTypes fn els

instance MapTypes (RelLink dec) where 
  mapTypes fn lnk = case lnk of 
    ProduceLink    dec insts -> ProduceLink    dec $ mapTypes fn insts
    PrescribeLink  dec insts -> PrescribeLink  dec $ mapTypes fn insts
    RevProduceLink dec insts -> RevProduceLink dec $ mapTypes fn insts

-- Expressions don't YET have any type info but we do a meaningless traversal here
-- just so we will catch future expressions added...
instance MapTypes (Exp dec) where 
  mapTypes fn e = case e of
   Lit _ _ -> e
   Var _ _ -> e
   App s rat rands -> App s (mapTypes fn rat) (mapTypes fn rands)
   If  s a b c     -> If  s (mapTypes fn a) (mapTypes fn b) (mapTypes fn c)

instance MapTypes a => MapTypes [a] where
  mapTypes fn = map (mapTypes fn)

instance (MapTypes a, MapTypes b) => MapTypes (a,b) where
  mapTypes fn (a,b) = (mapTypes fn a, mapTypes fn b)


----------------------------------------------------------------------------------------------------

mty = Just (TSym (toAtom "triple"), TPtr (TPtr (TSym (toAtom "double"))))
-- This will, oddly, compile but only hit the second part of the tuple!
strange = fmap (fmap (const TInt)) mty
strange2 = fmap (fmap (+1)) (Just (0,0))
-- Oh it must depend on compiler parameters, in a fresh GHCI this DOES NOT work:
-- Does some module I imported define this instance??  Ouch, non-local effect in this module?
strange3 = fmap (+1) (0,0)


test_desugarTypeDefs = 
  testSet "TypeDef"
    [testCase "" "basic desugarTypeDefs test"$  result ~=?  desugarTypeDefs input]
 where 
      pair   = toAtom "pair"
      triple = toAtom "triple"
      int    = toAtom "int"
      double = toAtom "double"
      lkji   = toAtom "Lkji"
      control_S2 = toAtom "control_S2"
      control_S3 = toAtom "control_S3"
      result = [DeclareItems () lkji (Just (TTuple [TSym int,TSym int,TSym int],TPtr (TPtr (TSym double)))),
		DeclareTags () control_S2 (Just (TTuple [TSym int,TSym int])),
		DeclareTags () control_S3 (Just (TTuple [TSym int,TSym int,TSym int]))]
      input = [TypeDef () pair  (TTuple [TSym int, TSym int]),
	       TypeDef () triple (TTuple [TSym int,TSym int,TSym int]),
	       DeclareItems () lkji (Just (TSym triple, TPtr (TPtr (TSym double)))),
	       DeclareTags () control_S2 (Just (TSym pair)),
	       DeclareTags () control_S3 (Just (TSym triple))]
