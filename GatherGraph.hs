----------------------------------------------------------------------------------------------------
-- This module implements a pass over the parser output that coalesces
-- all the relations in a .cnc file into a more manageable graph type.
----------------------------------------------------------------------------------------------------

module GatherGraph where
import AST
import Data.Map

data CncGraph = 
  CncGraph { 
    steps :: ()
  }
  


coalesceGraph = undefined