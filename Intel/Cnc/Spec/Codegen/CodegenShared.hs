
module Intel.Cnc.Spec.Codegen.CodegenShared where

import Intel.Cnc.EasyEmit 
import Intel.Cnc.Spec.Util
import Intel.Cnc.Spec.CncGraph (ColName) -- hiding (app, not, (&&), (==))


data CodeGenConfig = CodeGenConfig 
      { cgverbosity :: Bool 
      , old_05_api  :: Bool
      , genstepdefs :: Bool
      , gentracing  :: Bool
      , gendepends  :: Bool
      , gendebug    :: Bool
      , wrapall     :: Bool -- wrap all collections
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
      -- Takes collection name, reference to tag, reference to item.
      beforeItemPut :: Maybe (ColName -> Syntax -> Syntax -> EasyEmit ()),
      afterItemPut  :: Maybe (ColName -> Syntax -> Syntax -> EasyEmit ()),

      beforeReducerPut :: Maybe (ColName -> Syntax -> Syntax -> EasyEmit ()),
      afterReducerPut  :: Maybe (ColName -> Syntax -> Syntax -> EasyEmit ()),

      beforeTagPut :: Maybe (ColName -> Syntax -> EasyEmit ()),
      afterTagPut  :: Maybe (ColName -> Syntax -> EasyEmit ()),

      -- Takes a reference to the tag
      beforeStepExecute :: Maybe (ColName -> Syntax -> EasyEmit ()),
      afterStepExecute  :: Maybe (ColName -> Syntax -> EasyEmit ())

    }
  deriving Show

--instance Show CodeGenHooks where
instance Show (a -> b) where
  show _ = "<fun>"


defaultCodeGenHooks = CodeGenHooks
  {
      beforeItemPut     = Nothing,
      afterItemPut      = Nothing,
      beforeReducerPut  = Nothing,
      afterReducerPut   = Nothing,
      beforeTagPut      = Nothing,
      afterTagPut       = Nothing,
      beforeStepExecute = Nothing,
      afterStepExecute  = Nothing
  }



----------------------------------------------------------------------------------------------------
-- These should probably be moved to their own file, but I'm going to put small plugins here.
----------------------------------------------------------------------------------------------------

doneCountingPlugin =
  defaultCodeGenHooks 
  {
    beforeStepExecute = 
      Just $ \ name tag -> 
       do 
          putS "Woo!\n"
          return ()
  }


