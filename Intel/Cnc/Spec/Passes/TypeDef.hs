{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- This module contains the simple spec transformation pass that desugars type synonyms (TypeDefs)

-- Certainl this is a candidate for Scrap-your-boilerplate generic programming.

module Intel.Cnc.Spec.Passes.TypeDef where

import Intel.Cnc.Spec.Util
import StringTable.Atom
import qualified StringTable.AtomMap as AM

import Intel.Cnc.Spec.AST
import Test.HUnit
--import Debug.Trace


desugarTypeDefs :: [PStatement dec] -> [PStatement dec]
desugarTypeDefs input = 
    map desugar everything_else 
 where 
  synonyms = AM.fromList$ map (\ (TypeDef _ nm ty) -> (nm,ty)) $
	     filter isdef input

  everything_else = filter (not . isdef) input
  isdef (TypeDef _ _ _) = True
  isdef _ = False
  
  desugar stmnt = case stmnt of 
     DeclareTags  s nm mty -> DeclareTags  s nm $ fmap doTy mty 
     DeclareItems s nm Nothing -> stmnt
     DeclareItems s nm (Just (ty1,ty2)) -> DeclareItems s nm $ Just (doTy ty1, doTy ty2)
     DeclareSteps _ _   -> stmnt
     Function           -> stmnt
     DeclareExtern      -> stmnt

     Chain insts links      -> Chain (map doInst insts) (map doLink links)
     Constraints s inst els -> Constraints s (doInst inst) (map doExp els)

     TypeDef _ _ _ -> error "serious internal implementation error"

  doTy ty = 
    case ty of 
     TInt      -> TInt
     TFloat    -> TFloat
     TSym atom -> case AM.lookup atom synonyms of 
		    Nothing -> TSym atom
		    Just ty -> ty -- Easy, no constructor application
     TPtr ty   -> TPtr   $ doTy ty
     TRef ty   -> TRef   $ doTy ty
     TDense ty -> TDense $ doTy ty
     TConst ty -> TConst $ doTy ty
     TTuple ls -> TTuple $ map doTy ls


  ----------------------------------------------------------------------------------------------------
  -- Everything below this line is total boilerplate.
  ----------------------------------------------------------------------------------------------------

  doInst inst = case inst of 
   InstName       dec str     -> inst
   InstStepOrTags dec str els -> InstStepOrTags dec str $ map doExp els
   InstTagCol     dec str els -> InstTagCol     dec str $ map doExp els
   InstItemCol    dec str els -> InstItemCol    dec str $ map doExp els
   InstStepCol    dec str els -> InstStepCol    dec str $ map doExp els

  doLink lnk = case lnk of 
    ProduceLink    dec insts -> ProduceLink    dec $ map doInst insts
    PrescribeLink  dec insts -> PrescribeLink  dec $ map doInst insts
    RevProduceLink dec insts -> RevProduceLink dec $ map doInst insts

  -- Expressions don't yet have any type info but we do a meaningless traversal here
  -- just so we will catch future expressions added...
  doExp e = case e of
   Lit _ _ -> e
   Var _ _ -> e
   App s rat rands -> App s (doExp rat) (map doExp rands)
   If  s a b c     -> If  s (doExp a) (doExp b) (doExp c)



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
