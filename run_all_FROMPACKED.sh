#!/bin/bash

cd `dirname $0`


## DUPLICATED CODE:
## THIS IS A HACK ON TOP OF THE run_all_examples.sh SCRIPT
## THIS ONE RUNS THE PRECOMPILED EXECUTABLES THAT HAVE BEEN PACKED UP.

FORLATER=.

# Settings:
# ----------------------------------------

#export GHC=ghc-6.13.20100511 
#export GHC=~/bin/Linux-i686/bin/ghc-6.13.20100511
unset HASKELLCNC


  # Which subset of schedures should we test:
#PURESCHEDS=""
PURESCHEDS="2 3"

#IOSCHEDS="3 4 5 6 7 8"
IOSCHEDS="4 7 8 3 10 11"
#IOSCHEDS="10 100"

SEPARATESCHEDS=""

if [ "$THREADSETTINGS" == "" ] 
then THREADSETTINGS="4"
fi

source default_opt_settings.sh

  # Where to put the timing results:
RESULTS=results.dat
if [ -e $RESULTS ];
then BAK="$RESULTS".bak.`date +%s`
     echo "Backing up old results to $BAK"
     mv $RESULTS $BAK
fi

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
echo "# TestName Variant Scheduler NumThreads HashHackEnabled MinTime MedianTime MaxTime" > $RESULTS
echo "# "`date` >> $RESULTS
#echo "# "`uname -a` >> $RESULTS
#echo "# REMOTE EXECUTION FROM "`uname -a`" TO $REMOTE " >> $RESULTS
echo "# EXECUTION FROM PACKED BENCHMARKS ON "`uname -a`" " >> $RESULTS
# echo "# "`ghc -V` >> $RESULTS
echo "# "
echo "# Running each test for $TRIALS trials." >> $RESULTS
echo "#  ... with default compiler options: $GHC_DEFAULT_FLAGS" >> $RESULTS
echo "#  ... with default runtime options: $GHC_DEFAULT_RTS" >> $RESULTS

cnt=0

source run_all_shared.sh

# Dynamic scoping.  Lame.  This uses $test.
function runit() 
{
  cnt=$((cnt+1))
  echo 
  echo "--------------------------------------------------------------------------------"
  echo "  Running Config $cnt: $test variant $CNC_VARIANT sched $CNC_SCHEDULER threads $NUMTHREADS $hashtab"
  echo "--------------------------------------------------------------------------------"
  echo 
  echo "(In directory `pwd`)"
  if [ "$NUMTHREADS" != "0" ] && [ "$NUMTHREADS" != "" ]
  then export RTS=" $GHC_DEFAULT_RTS -s -N$NUMTHREADS "
  else export RTS=""
  fi
  if [ "$hashtab" == "" ];
  then HASH="0"
  else HASH="1"
  fi

  # We compile the test case using runcnc:
  # NORUN=1 ./runcnc $hashtab examples/"$test".hs
  # CODE=$?
  # check_error $CODE "ERROR: compilation failed."


#====================================================================================================

  TARGETDIR="$FORLATER/$CNC_VARIANT/$sched"
  # mkdir -p "$TARGETDIR"
  EXENAME="$TARGETDIR/$test".exe

  echo 
  echo "RUNNING PRE-PACKED EXECUTABLE: $EXENAME "  
  
  if ! [ -e "$EXENAME" ]; then 
    echo "# ERROR FILE DOES NOT EXIST: $EXENAME"
    echo "# ERROR FILE DOES NOT EXIST: $EXENAME" >> $RESULTS
#    exit 1
  fi

#====================================================================================================

  echo "Executing $NTIMES $TRIALS $EXENAME $ARGS +RTS $RTS -RTS "
  if [ "$LONGRUN" == "" ]; then export HIDEOUTPUT=1; fi
  
  if [ -e "$EXENAME" ]; then 
    times=`$NTIMES "$TRIALS" $EXENAME $ARGS +RTS $RTS -RTS`
  else 
    times="ERR ERR ERR"
  fi
  CODE=$?

#====================================================================================================

  echo " >>> MIN/MEDIAN/MAX TIMES $times"

  check_error $CODE "ERROR: run_all_tests this test failed completely: $test.exe"

  if [ "$CODE" == "143" ];
  then echo "$test.exe" "$CNC_VARIANT" "$CNC_SCHEDULER" "$NUMTHREADS" "$HASH" "TIMEOUT TIMEOUT TIMEOUT" >> $RESULTS
  elif [ "$CODE" != "0" ] ;
  then echo "$test.exe" "$CNC_VARIANT" "$CNC_SCHEDULER" "$NUMTHREADS" "$HASH" "ERR ERR ERR" >> $RESULTS
  else 
       echo "$test.exe" "$CNC_VARIANT" "$CNC_SCHEDULER" "$NUMTHREADS" "$HASH" "$times" >> $RESULTS
  fi
}


echo "Running all tests, for THREADSETTINGS in {$THREADSETTINGS}"
echo


# Hygiene:
rm -f examples/*.exe

#====================================================================================================

function run_benchmark() {
  set -- $line
  test=$1; shift

  if [ "$LONGRUN" == "" ];
  # If we're not in LONGRUN mode we run each executable with no
  # arguments causing it to go to its default problem size.
  then ARGS=
  else ARGS=$*
  fi

  echo "================================================================================"
  echo "                           Running Test: $test.exe $ARGS                        "
  echo "================================================================================"

  echo "# *** Config [$cnt ..], testing with command/args: $test.exe $ARGS " >> $RESULTS

 export CNC_VARIANT=io
 for sched in $IOSCHEDS; do
   export CNC_SCHEDULER=$sched
   #for NUMTHREADS in 4; do

   for NUMTHREADS in $THREADSETTINGS; do
     # Running with the hashtable hack off:
     export hashtab=""
     runit

     # This one is incorrect and nondeterministic:
     # export hashtab="-DHASHTABLE_TEST";  runit
   done # threads
   echo >> $RESULTS;
 done # schedulers

 export CNC_VARIANT=pure
 # Currently running the pure scheduler only in single threaded mode:
 for sched in $PURESCHEDS; do
   if [ "$sched" == "3" ]; then 
     for NUMTHREADS in $THREADSETTINGS; do
       export hashtab=""
       runit
     done # threads
   elif [ "$sched" == "2" ]; then
      # Pure 2:
      export NUMTHREADS=0
      unset hashtab
      export CNC_SCHEDULER=$sched
      runit
    else 
    echo "# ERROR Problem in script in handling pure Sched $sched " >> $RESULTS;
   fi
   echo >> $RESULTS;
 done

 # Finally, run once through separately compiled modules to compare performance (and make sure they build).
 # This will basically use the IO based implementation with the default scheduler.
 # for sched in $SEPARATESCHEDS; do
 #   export CNC_SCHEDULER=$sched
 #   export NUMTHREADS=4
 #   runit
 # done

 echo >> $RESULTS;
 echo >> $RESULTS;

}

# Read $line and do the benchmark with ntimes_binsearch.sh
function run_binsearch_benchmark() {
   NTIMES=./ntimes_binsearch.sh
   run_benchmark
   NTIMES=UNSET
}

# Read $line and do the benchmark with ntimes_minmedmax
function run_normal_benchmark() {
   NTIMES=./ntimes_minmedmax
   run_benchmark
   NTIMES=UNSET
}


#====================================================================================================

# This specifies the list of tests and their arguments for a "long" run:

#for line in "mandel_opt 1 300 300 4000" "mandel_opt 2 300 300 4000" "mandel_opt 3 300 300 4000" "mandel 300 300 4000"; do

#  
#for line in  "par_seq_par_seq 8.5" "embarrassingly_par 9.2" "primes2 200000" "mandel 300 300 4000" "mandel_opt 1 300 300 4000" "sched_tree 18" "fib 20000" "threadring 50000000 503" "nbody 1200" "primes 200000"; do

# Parallel benchmarks only:
#for line in  "par_seq_par_seq 8.5" "embarrassingly_par 9.2" "primes2 200000" "mandel 300 300 4000" "mandel_opt 1 300 300 4000" "sched_tree 18" "nbody 1200" "primes 200000"; do

# Finish up...
#for line in  "mandel 300 300 4000" "mandel_opt 1 300 300 4000" "sched_tree 18" "nbody 1200" "primes 200000"; do



#------------------------------------------------------------
# BIG RUNS FOR HEFTY MACHINES
#------------------------------------------------------------

# for line in   ; do
# for line in "sched_tree 20" "nbody 2400" "primes 1000000" "mandel_opt 300 300 20000"; do


# Run benchmarks of linear complexity
# for line in "blackscholes 10000" "embarrassingly_par " "par_seq_par_seq "  "mandel_opt 2 300 300"  "mandel 300 300 " ; do
# #for line in "mandel_opt 2 300 300"  "mandel 300 300 " ; do
#   run_binsearch_benchmark
# done

# These are of non-linear complexity or I just want to put hem last:
#for line in  "primes2 1000000" "primes 1000000" "nbody 2400"  "sched_tree 19"; do


# Ok, giving up and going back to normal mode:

for line in "nbody 10000"  "blackscholes 10000 50000000" "mandel_opt2 2 300 300 20000" "cholesky 1000 50 m1000.in"  "embarrassingly_par 9.8" "par_seq_par_seq 9.2"  "primes2 1000000" "sched_tree 19" "mandel 300 300 20000" "mandel_opt 1 300 300 20000"  "primes 1000000"; 

#for line in "cholesky 2000 100 m1000.in"; 
do
  run_normal_benchmark
done
echo "Finished with all test configurations."
