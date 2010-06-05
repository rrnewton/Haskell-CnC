
# This file is sourced by both runcnc and run_all_tests

# This represents a set of default compile-time and run-time options
#  for GHC that are used for all benchmarks.

# -fvia-C
#GHC_DEFAULT_FLAGS=" -fasm -O2"

# For 6.13:

if [ "`ghc -V`" == "The Glorious Glasgow Haskell Compilation System, version 6.12.1" ];
then GHC_DEFAULT_FLAGS=" -O2 -DUSE_GMAP "
#then GHC_DEFAULT_FLAGS=" "
else GHC_DEFAULT_FLAGS=" -rtsopts -O2 -DUSE_GMAP "
fi

 # Affinity is pretty much always good.
GHC_DEFAULT_RTS="  -qa " 

