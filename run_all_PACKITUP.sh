#!/bin/bash

## DUPLICATED CODE:
## THIS IS A HACK ON TOP OF THE run_all_examples.sh SCRIPT
## THIS ONE PACKS UP ALL THE EXECUTABLES TO BE SCP'D TO A DIFFERENT MACHINE AND RUN LATER.


##REMOTE=newton@tom.csail.mit.edu
#REMOTEDIR=`date +"%Y.%m.%d_%s"`_tmp_run_dir
#REMOTEDIR=tmp_run_dir


# ssh $REMOTE mkdir $REMOTEDIR
# scp ntimes ntimes_minmedmax timeout "$REMOTE":$REMOTEDIR/


FORLATER=for_later
NONSTRICT=1

rm -rf $FORLATER ; mkdir  $FORLATER


# Settings:
# ----------------------------------------

#export GHC=ghc-6.13.20100511 
#export GHC=~/bin/Linux-i686/bin/ghc-6.13.20100511
unset HASKELLCNC


  # Which subset of schedures should we test:
#PURESCHEDS="3"
PURESCHEDS="2 3"
#IOSCHEDS="3"
IOSCHEDS="3 4 5 6 7 8 10 11"
#IOSCHEDS="10 100"

#SEPARATESCHEDS="6"
SEPARATESCHEDS=""


if [ "$THREADSETTINGS" == "" ] 
then THREADSETTINGS="4"
#then THREADSETTINGS="0 1 2 3 4"
fi

source default_opt_settings.sh


# How many times to run a process before taking the best time:
if [ "$TRIALS" == "" ]; then 
  TRIALS=1
fi

# Determine number of hardware threads on the machine:
#  if [ -f /proc/cpuinfo ];  
  if [ -d /sys/devices/system/cpu/ ];
  then 
       MAXTHREADS=`ls  /sys/devices/system/cpu/ | grep "cpu[0123456789]*$" | wc -l`
       echo "Detected the number of CPUs on the machine to be $MAXTHREADS"
  elif [ `uname` == "Darwin" ];
  then MAXTHREADS=`sysctl -n hw.ncpu`
  else MAXTHREADS=2
  fi 

if [ "$THREADSETTINGS" == "" ]; then 
  THREADSETTINGS=$MAXTHREADS
  #for ((i=1; i <= $MAX; i++)); do THREADSETTINGS="$THREADSETTINGS $i"; done 
fi

# ================================================================================

cnt=0

function check_error() {
  CODE=$1
  MSG=$2
  # Error code 143 was a timeout
  if [ "$CODE" == "143" ]
  then echo "       Return code $CODE Params: $CNC_VARIANT $CNC_SCHEDULER $FLAGS"
       echo "       Process TIMED OUT!!"
  elif [ "$CODE" != "0" ]
  then echo $MSG
       echo "       Error code $CODE Params: $CNC_VARIANT $CNC_SCHEDULER $FLAGS"
       echo "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
       if [ "$NONSTRICT" == "" ];
       then exit $CODE
       fi
  fi
}

# Dynamic scoping.  Lame.  This uses $test.
function runit() 
{
  cnt=$((cnt+1))
  echo 
  echo "--------------------------------------------------------------------------------"
  echo "  Running Config $cnt: $test variant $CNC_VARIANT sched $CNC_SCHEDULER threads $NUMTHREADS $hashtab"
  echo "--------------------------------------------------------------------------------"
  echo 
  if [ "$NUMTHREADS" != "0" ] && [ "$NUMTHREADS" != "" ]
  then export RTS=" $GHC_DEFAULT_RTS -s -N$NUMTHREADS "
  else export RTS=""
  fi
  if [ "$hashtab" == "" ];
  then HASH="0"
  else HASH="1"
  fi

#====================================================================================================

  TARGETDIR="$FORLATER/$CNC_VARIANT/$sched"
  mkdir -p "$TARGETDIR"
  EXENAME="$TARGETDIR/$test".exe

  echo 
  echo "PACKING FOR LATER EXECUTION: $EXENAME "
  
#====================================================================================================

  if ! [ -e "$EXENAME" ]; then
     # We compile the test case using runcnc:
     NORUN=1 ./runcnc $hashtab examples/"$test".hs
     CODE=$?
     check_error $CODE "ERROR: compilation failed."
     cp examples/"$test".exe $EXENAME
  fi

}


echo "Running all tests, for THREADSETTINGS in {$THREADSETTINGS}"
echo

# Build the timeout script if it hasn't been already:
if ! [ -e ./timeout ]; then 
  ghc --make timeout.hs -threaded
  if [ "$?" != "0" ];
  then echo "GHC build of timeout.hs returned error."
       exit 1
  fi
fi

#================================================================================
mkdir -p "$FORLATER/examples/"

## COPY THE STUFF INTO THE PACKAGE:
cp -a *big_run*.sh default_opt_settings.sh ntimes ntimes_minmedmax ntimes_binsearch.sh binsearch_throughput timeout.sh \
   run_all_*.sh  run_all_FROMPACKED.sh "$FORLATER/"

# Copy data files as well:
cp -a examples/*.in "$FORLATER/examples/"

#================================================================================

# Hygiene:
rm -f examples/*.exe

# This specifies the list of tests and their arguments for a "long" run:

# Parallel benchmarks only:
#for line in "blackscholes 10000" "par_seq_par_seq 8.5" "embarrassingly_par 9.2" "primes2 200000" "mandel 300 300 4000" "mandel_opt 1 300 300 4000" "mandel_opt2 1 300 300 4000" "sched_tree 18"  "primes 200000" "nbody 1200" "cholesky"; do
#for line in  "primes 200000"; do
#for line in  "nbody 1200"; do
#for line in "cholesky"; do

for line in "nbody 10000"  "blackscholes 10000 50000000" "mandel_opt2 2 300 300 20000" "cholesky 1000 50 m1000.in"  "embarrassingly_par 9.8" "par_seq_par_seq 9.2"  "primes2 1000000" "sched_tree 19" "mandel 300 300 20000" "mandel_opt 1 300 300 20000"  "primes 1000000"; 
do

# cholesky

  set -- $line
  test=$1; shift

  if [ "$LONGRUN" == "" ];
  # If we're not in LONGRUN mode we run each executable with no
  # arguments causing it to go to its default problem size.
  then ARGS=
  else ARGS=$*
  fi

  echo "================================================================================"
  echo "                           Packing Up test: $test.exe $ARGS                     "
  echo "================================================================================"

  echo "# *** Config [$cnt ..], testing with command/args: $test.exe $ARGS " 

 export CNC_VARIANT=pure
 # Currently running the pure scheduler only in single threaded mode:
# export NUMTHREADS=0

 # [2010.06.17] Nah let's enable threading for pure/3:
 export NUMTHREADS=4
 for sched in $PURESCHEDS; do
   unset hashtab
   export CNC_SCHEDULER=$sched
   runit
 done

 export CNC_VARIANT=io
 for sched in $IOSCHEDS; do
   export CNC_SCHEDULER=$sched
   #for NUMTHREADS in 4; do

##================================================================================
## FIXME:  DONT RECOMPILE FOR DIFFERENT THREADSETTINGS EXCEPT 0
##================================================================================

   NUMTHREADS=4
#   for NUMTHREADS in $THREADSETTINGS; do
     # Running with the hashtable hack off:
     export hashtab=""
     runit

     # This one is incorrect and nondeterministic:
     # export hashtab="-DHASHTABLE_TEST";  runit
#   done # threads

 done # schedulers

 # Finally, run once through separately compiled modules to compare performance (and make sure they build).
 # This will basically use the IO based implementation with the default scheduler.
 for sched in $SEPARATESCHEDS; do
   export CNC_SCHEDULER=$sched
   export NUMTHREADS=4
   runit
 done

done

echo "Finished with all test configurations."
