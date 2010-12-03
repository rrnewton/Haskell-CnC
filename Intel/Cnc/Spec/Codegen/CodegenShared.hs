{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}

module Intel.Cnc.Spec.Codegen.CodegenShared where

import Intel.Cnc.Spec.AST 

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
      , plugins     :: [CodeGenPlugin]
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
--      , plugins     = [doneCountingPlugin]
     }


----------------------------------------------------------------------------------------------------
-- Here's a convenient Plugin architecture that uses EasyEmit
----------------------------------------------------------------------------------------------------

-- This represents a collection of hooks that can be used by a codegen to inject
-- code before/after steps and puts/gets.  This is an attempt to make
-- some of our different extensions into modular plugins.

type Hook        = ColName -> Syntax           -> EasyEmit ()
type HookWithVal = ColName -> Syntax -> Syntax -> EasyEmit ()

data CodeGenPlugin = CodeGenPlugin 
    {
      -- This predicate determines which steps to which the hooks are applied.
      hooksPredicate :: ColName -> Bool,

      -- I vacillated on whether or not to put all of these hooks
      -- (functions) inside a Maybe.  That would make it more clear
      -- what a plugin DOESN'T implement, but I think it makes the
      -- plugins more clunky to use, and makes the belowe combineHooks
      -- function harder to write.  Besides, there is a sensible
      -- default hook -- it emits nothing.

      -- Initializes state for a collection.
      addState :: (ColName -> EasyEmit ()),

      -- Takes collection name, reference to tag, reference to item.
      beforeItemPut :: HookWithVal,
      afterItemPut  :: HookWithVal,
      beforeItemGet :: HookWithVal,
      afterItemGet  :: HookWithVal,

      beforeReducerPut :: HookWithVal,
      afterReducerPut  :: HookWithVal,
      beforeReducerGet :: HookWithVal,
      afterReducerGet  :: HookWithVal,

      beforeTagPut :: Hook,
      afterTagPut  :: Hook,

      -- Takes the name of th step's tag as input:
      beforeStepExecute :: Hook,
      afterStepExecute  :: Hook
    }
  deriving Show

--instance Show CodeGenPlugin where
instance Show (a -> b) where
  show _ = "<fun>"


defaultCodeGenPlugin = 
  -- The default hooks just do nothing:
  let threearg = const$ const$ const$ return ()
      twoarg   =        const$ const$ return ()
  in CodeGenPlugin
  {
      hooksPredicate    = const False, -- And they apply to nothing.
      addState          = const$ return (), 
      beforeItemPut     = threearg,
      afterItemPut      = threearg,
      beforeItemGet     = threearg,
      afterItemGet      = threearg,

      beforeReducerPut  = threearg,
      afterReducerPut   = threearg,
      beforeReducerGet  = threearg,
      afterReducerGet   = threearg,

      beforeTagPut      = twoarg,
      afterTagPut       = twoarg,
      beforeStepExecute = twoarg,
      afterStepExecute  = twoarg
  }

-- Combine plugins that operate on different step collections.
-- The first plugin has first dibs.
-- This is probably quite inefficient.
-- combineHooksDisjoint left right = 
--   let lp = hooksPredicate left 
--       rp = hooksPredicate right 
--       combine project name = 
-- 	  if lp name then project left name else project right name
--   in CodeGenPlugin
--   {
--       hooksPredicate    = \ name -> lp name || rp name,
--       addState          = combine addState,
--       beforeItemPut     = combine beforeItemPut,
--       afterItemPut      = combine afterItemPut,
--       beforeReducerPut  = combine beforeReducerPut,
--       afterReducerPut   = combine afterReducerPut,
--       beforeTagPut      = combine beforeTagPut,
--       afterTagPut       = combine afterTagPut,
--       beforeStepExecute = combine beforeStepExecute,
--       afterStepExecute  = combine afterStepExecute
--   }

-- Combine plugins.  Both are executed against all step collections to which they apply.
-- This is probably quite inefficient.  Those Maybe's could make it a little more efficient.
combineHooks left right = 
  let lp = hooksPredicate left 
      rp = hooksPredicate right 
      combine3 project x y z = do project left x y z; project right x y z
      combine2 project x y   = do project left x y  ; project right x y 
      combine1 project x     = do project left x    ; project right x  
  in CodeGenPlugin
  {
      hooksPredicate    = \ name -> lp name || rp name,
      addState          = combine1 addState,

      beforeItemPut     = combine3 beforeItemPut,
      afterItemPut      = combine3 afterItemPut,
      beforeItemGet     = combine3 beforeItemGet,
      afterItemGet      = combine3 afterItemGet,

      beforeReducerPut  = combine3 beforeReducerPut,
      afterReducerPut   = combine3 afterReducerPut,
      beforeReducerGet  = combine3 beforeReducerGet,
      afterReducerGet   = combine3 afterReducerGet,

      beforeTagPut      = combine2 beforeTagPut,
      afterTagPut       = combine2 afterTagPut,
      beforeStepExecute = combine2 beforeStepExecute,
      afterStepExecute  = combine2 afterStepExecute
  }



----------------------------------------------------------------------------------------------------
-- These should probably be moved to their own file, but I'm going to put small plugins here.
----------------------------------------------------------------------------------------------------

testplugin =
  defaultCodeGenPlugin 
  { hooksPredicate = const True
  , addState = \ name -> comm "[testhooks] State goes here."

  , beforeItemPut = \ name tag val -> putS$ "// [testhooks] before putting Item: " ++ show (deSyn tag) ++" "++ show (deSyn val)
  , afterItemPut  = \ name tag val -> putS$ "// [testhooks] after putting Item: " ++ show (deSyn tag) ++" "++ show (deSyn val)
  , beforeItemGet = \ name tag val -> putS$ "// [testhooks] before getting Item: " ++ show (deSyn tag) ++" "++ show (deSyn val)
  , afterItemGet  = \ name tag val -> putS$ "// [testhooks] after getting Item: " ++ show (deSyn tag) ++" "++ show (deSyn val)

  , beforeReducerPut = \ name tag val -> putS$ "// [testhooks] before putting Reducer: " ++ show (deSyn tag) ++" "++ show (deSyn val)
  , afterReducerPut  = \ name tag val -> putS$ "// [testhooks] after putting Reducer: " ++ show (deSyn tag) ++" "++ show (deSyn val)
  , beforeReducerGet = \ name tag val -> putS$ "// [testhooks] before getting Reducer: " ++ show (deSyn tag) ++" "++ show (deSyn val)
  , afterReducerGet  = \ name tag val -> putS$ "// [testhooks] after getting Reducer: " ++ show (deSyn tag) ++" "++ show (deSyn val)

  , beforeTagPut = \ name tag -> putS$ "// [testhooks] before putting Tag: " ++ show (deSyn tag) 
  , afterTagPut  = \ name tag -> putS$ "// [testhooks] after putting Tag: " ++ show (deSyn tag) 

  , beforeStepExecute = \ name tag -> putS$ "// [testhooks] before step execute on tag reference: " ++ show (deSyn tag) 
  , afterStepExecute  = \ name tag -> putS$ "// [testhooks] after step execute on tag reference: " ++ show (deSyn tag) 

  }

doneCountingPlugin =
  defaultCodeGenPlugin 
  { hooksPredicate = const True

  , addState = \ name -> 
     do var TInt "done_counter"
	return ()

  }
