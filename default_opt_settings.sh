
# This file is sourced by both runcnc and run_all_tests

# This represents a set of default compile-time and run-time options
#  for GHC that are used for all benchmarks.

# -fvia-C
#GHC_DEFAULT_FLAGS=" -fasm -O2"

# For 6.13:

# TOGGLE
#GMAP=
GMAP="-DUSE_GMAP"

if [ "$CNCOPT" == "" ];
then CNCOPT="-O2"
fi

# [2010.10.06] Allow user to start with their own GHC_DEFAULT_FLAGS
if [ "`$GHC -V`" == "The Glorious Glasgow Haskell Compilation System, version 6.12.1" ];
then GHC_DEFAULT_FLAGS="$GHC_DEFAULT_FLAGS $CNCOPT $GMAP "
#then GHC_DEFAULT_FLAGS=" "
else GHC_DEFAULT_FLAGS="$GHC_DEFAULT_FLAGS -rtsopts $CNCOPT $GMAP "
fi

 # Affinity is pretty much always good.
GHC_DEFAULT_RTS="$GHC_DEFAULT_RTS  -qa " 

