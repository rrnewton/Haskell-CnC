{-# LANGUAGE ScopedTypeVariables #-}

module Intel.Cnc.Spec.Codegen.CodegenShared where

import Intel.Cnc.EasyEmit  hiding (app, not, (&&), (==), (||))
import Intel.Cnc.Spec.Util
import Intel.Cnc.Spec.CncGraph (ColName) -- hiding (app, not, (&&), (==))


data CodeGenConfig = CodeGenConfig 
      { cgverbosity :: Bool 
      , old_05_api  :: Bool
      , genstepdefs :: Bool
      , gentracing  :: Bool
      , gendepends  :: Bool
      , gendebug    :: Bool
      , wrapall     :: Bool -- wrap ALL collections no matter what
      , plugins     :: [CodeGenHooks]
      }
  deriving Show

default_codegen_config = 
 CodeGenConfig 
     {  cgverbosity = False 
      , old_05_api  = False
      , genstepdefs = True
      , gentracing  = False
      , gendepends  = False
      , gendebug    = False
      , wrapall     = False
      , plugins     = []
     }


----------------------------------------------------------------------------------------------------
-- Here's a convenient Plugin architecture that uses EasyEmit
----------------------------------------------------------------------------------------------------

-- This represents a collection of hooks that can be used by a codegen to inject
-- code before/after steps and puts/gets.  This is an attempt to make
-- some of our different extensions into modular plugins.
data CodeGenHooks = CodeGenHooks 
    {
      -- This predicate determines which steps to which the hooks are applied.
      hooksPredicate :: ColName -> Bool,

      -- Initializes state for a collection.
      

      -- I vacillated on whether or not to put all of these hooks
      -- (functions) inside a Maybe.  That would make it more clear
      -- what a plugin DOESN'T implement, but I think it makes the
      -- plugins more clunky to use, and makes the belowe combineHooks
      -- function harder to write.  Besides, there is a sensible
      -- default hook -- it emits nothing.


      -- Takes collection name, reference to tag, reference to item.
      -- beforeItemPut :: (ColName -> Syntax -> Syntax -> EasyEmit ()),
      -- afterItemPut  :: Maybe (ColName -> Syntax -> Syntax -> EasyEmit ()),

      -- beforeReducerPut :: Maybe (ColName -> Syntax -> Syntax -> EasyEmit ()),
      -- afterReducerPut  :: Maybe (ColName -> Syntax -> Syntax -> EasyEmit ()),

      -- beforeTagPut :: Maybe (ColName -> Syntax -> EasyEmit ()),
      -- afterTagPut  :: Maybe (ColName -> Syntax -> EasyEmit ()),

      -- -- Takes a reference to the tag
      -- beforeStepExecute :: Maybe (ColName -> Syntax -> EasyEmit ()),
      -- afterStepExecute  :: Maybe (ColName -> Syntax -> EasyEmit ())

      beforeItemPut :: (ColName -> Syntax -> Syntax -> EasyEmit ()),
      afterItemPut  :: (ColName -> Syntax -> Syntax -> EasyEmit ()),

      beforeReducerPut :: (ColName -> Syntax -> Syntax -> EasyEmit ()),
      afterReducerPut  :: (ColName -> Syntax -> Syntax -> EasyEmit ()),

      beforeTagPut :: (ColName -> Syntax -> EasyEmit ()),
      afterTagPut  :: (ColName -> Syntax -> EasyEmit ()),

      -- Takes a reference to the tag
      beforeStepExecute :: (ColName -> Syntax -> EasyEmit ()),
      afterStepExecute  :: (ColName -> Syntax -> EasyEmit ())

    }
  deriving Show

--instance Show CodeGenHooks where
instance Show (a -> b) where
  show _ = "<fun>"


defaultCodeGenHooks = 
  -- The default hooks just do nothing:
  let threearg = const$ const$ const$ return ()
      twoarg   =        const$ const$ return ()
  in CodeGenHooks
  {
      hooksPredicate    = const False, -- And they apply to nothing.
      beforeItemPut     = threearg,
      afterItemPut      = threearg,
      beforeReducerPut  = threearg,
      afterReducerPut   = threearg,
      beforeTagPut      = twoarg,
      afterTagPut       = twoarg,
      beforeStepExecute = twoarg,
      afterStepExecute  = twoarg

      -- afterItemPut      = Nothing,
      -- beforeReducerPut  = Nothing,
      -- afterReducerPut   = Nothing,
      -- beforeTagPut      = Nothing,
      -- afterTagPut       = Nothing,
      -- beforeStepExecute = Nothing,
      -- afterStepExecute  = Nothing
  }


-- Combine plugins that operate on different step collections.
-- The first plugin has first dibs.
-- This is probably quite inefficient.
{-
combineHooks left right = 
  let lp = hooksPredicate left 
      rp = hooksPredicate right 
      combine project = 
	  case (project left, project right) of
	    (Nothing,Nothing) -> Nothing
	    _ -> Just$ \ name -> if lp name then beforeItemPut left else beforeItemPut right,
  in CodeGenHooks
  {
      hooksPredicate    = \ name -> lp name || rp name,
      beforeItemPut     = 
      afterItemPut      = Nothing,
      beforeReducerPut  = Nothing,
      afterReducerPut   = Nothing,
      beforeTagPut      = Nothing,
      afterTagPut       = Nothing,
      beforeStepExecute = Nothing,
      afterStepExecute  = Nothing
  }
-}


combineHooks left right = 
  let lp = hooksPredicate left 
      rp = hooksPredicate right 
      combine project name = 
	  if lp name then project left else project right
--	  if lp name then project left name else project right name

      test1 :: (ColName -> Syntax -> Syntax -> EasyEmit ()) = beforeItemPut left
      test2 = beforeItemPut right
  in CodeGenHooks
  {
      hooksPredicate    = \ name -> lp name || rp name,
      beforeItemPut     = combine beforeItemPut,
      afterItemPut      = undefined,
      beforeReducerPut  = undefined,
      afterReducerPut   = undefined,
      beforeTagPut      = undefined,
      afterTagPut       = undefined,
      beforeStepExecute = undefined,
      afterStepExecute  = undefined
  }



----------------------------------------------------------------------------------------------------
-- These should probably be moved to their own file, but I'm going to put small plugins here.
----------------------------------------------------------------------------------------------------

doneCountingPlugin =
  defaultCodeGenHooks 
  {
    beforeStepExecute = 
      --Just $ 
      \ name tag -> 
       do 
          putS "Woo!\n"
          return ()
  }


