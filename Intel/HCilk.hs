

module Intel.HCilk 
    ( 
      HCilk, Future 
    , runCilk
    , spawn, spawnDupable
    , sync, get
    )
where 

-- Reexport the best version.
import Intel.HCilk.HCilk_Sparks
