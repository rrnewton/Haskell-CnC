
-- For GHC's benefit, we need a module called "Main" in order to compile an executable.

-- We just import the library version of the same and package it appropriately.

module Main where

import Intel.Cnc.Spec.MainExecutable

main = mainExecutableCommand

