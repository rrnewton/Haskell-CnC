{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, NamedFieldPuns #-}

-- NOTES
-- It would be nice for plugins to be able to invoke methods of all other plugins...

-- TODO / FIXME:
-- whenDone hook can be removed now.

-- Environment will currently be decremented below zero because of
-- upstream reduction collections.. only active collections should
-- count for that.

--------------------------------------------------------------------------------

module Intel.Cnc.Spec.Codegen.CodegenShared 
where

import Intel.Cnc.Spec.Codegen.Plugins

----------------------------------------------------------------------------------------------------
-- This datatype stores the configuration information for backend acode generation.
----------------------------------------------------------------------------------------------------

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
      , plugins     = [testplugin]
--      , plugins     = [autodonePlugin]
--      , plugins     = []
     }


