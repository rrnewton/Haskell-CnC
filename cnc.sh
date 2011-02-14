#!/bin/bash

# This is a simple script for running the CnC Spec tool from source
# from a DIFFERENT starting directory.

orig=`pwd`
base=`dirname "$0"`

# We, alas, need this extra shell script wrapper to make GHC's search
# process happy.
cd "$base"

make trans_inplace
runhaskell cnc_wrapper.hs "$orig" $*
CODE=$?

cd "$orig"
exit $CODE
