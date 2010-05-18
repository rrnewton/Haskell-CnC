
# This file is sourced by both runcnc and run_all_tests

# This represents a set of default compile-time and run-time options
#  for GHC that are used for all benchmarks.

# -fvia-C
#GHC_DEFAULT_FLAGS=" -fasm -O2"
GHC_DEFAULT_FLAGS=" -rtsopts -O2"

 # Affinity is pretty much always good.
GHC_DEFAULT_RTS="  -qa " 

