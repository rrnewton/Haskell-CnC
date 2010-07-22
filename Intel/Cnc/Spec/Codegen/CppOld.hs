
----------------------------------------------------------------------------------------------------
-- This is the code generator for the original (CnC/C++ 0.1-0.5) "context"-based C++ API.
----------------------------------------------------------------------------------------------------

-- This should eventually be based on some intermediate representation of C code.  For a
-- first cut, however, it is easier to just generate syntax directly.  (Especially with
-- the assistance of the pretty printing libary.)

module Intel.Cnc.Spec.Codegen.CppOld where

import Intel.Cnc.Spec.AST
import Intel.Cnc.Spec.GatherGraph
import Text.PrettyPrint.HughesPJClass



-- Could use bytestrings or handles.
emitCppOld :: CncGraph -> String
emitCppOld = undefined

