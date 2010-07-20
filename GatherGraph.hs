----------------------------------------------------------------------------------------------------
-- This module implements a pass over the parser output that coalesces
-- all the relations in a .cnc file into a more manageable graph type.
----------------------------------------------------------------------------------------------------

module GatherGraph where
import AST
import Data.Map
import StringTable.Atom

--type InData = 

data StepEntry = 
  StepEntry {
    sePrescribingTagCol :: Atom
  }
-- TagCol OutTags InData OutData

data CncGraph = 
  CncGraph { 
    steps :: (Map String StepEntry)
  }


coalesceGraph = undefined