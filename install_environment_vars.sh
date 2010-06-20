#!/bin/sh

# Instructions:
# 'source' me from my containing directory.

export HASKELLCNC=`pwd`
export PATH="$HASKELLCNC:$PATH"


################################################################################
# My Darcs Helpers
################################################################################
# I find it extremely annoying to time travel with darcs without some explicit assistance.

function darcs-record() {
  local counter
  if [ -e ".rn_darcs_counter" ]; then
    counter=`cat .rn_darcs_counter` 
    echo "  Stored counter contained $counter."
    counter=$((counter+1))
    echo "  Incremented to $counter"       
    echo $counter  > ".rn_darcs_counter"
  else
    echo "  Initializing Ryan's darcs counter to 1, stored in .rn_darcs_counter"
    echo 1  > ".rn_darcs_counter"
    counter=1
  fi
  darcs record -a --skip-long-comment -m "RN"$counter"# $*"
}

## [2010.06.20] Darcs is only usable with some help and added conventions.
function darcs-myroll() {
  local NUM=$1
  if [ "$NUM" == "" ]; then
    echo "ERROR: darcs-myunroll function expects an RN rev number as first argument."
    return 1
  fi
  echo "  Unrolling patches up to and including patch: $1"
  darcs rollback -a --skip-long-comment -m "ROLLBACKPATCH to RN$NUM" --from-patch "RN""$NUM""#"
#  darcs rollback -a --edit-long-comment -m "ROLLBACKPATCH to RN$NUM" --from-patch "RN""$NUM""#"
}

function darcs-unroll() {
  # -a seems a little too dangerous, supply it separately
  # darcs obliterate --last=1 $*
  darcs obliterate -a --from-patch "ROLLBACKPATCH" $*
}

function darcs-againroll() {
  local NUM=$1
  if [ "$NUM" == "" ]; then
    echo "ERROR: darcs-myunroll function expects an RN rev number as first argument."
    return 1 
  fi
  echo "  Rerolling back to a different patch: $1"
  darcs-unroll && darcs-rollback $*
}
