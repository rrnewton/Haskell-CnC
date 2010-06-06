#!/bin/bash

# ---------------------------------------------------------------------------
#  Intel Concurrent Collections for Haskell
#  Copyright (c) 2010, Intel Corporation.
# 
#  This program is free software; you can redistribute it and/or modify it
#  under the terms and conditions of the GNU Lesser General Public License,
#  version 2.1, as published by the Free Software Foundation.
# 
#  This program is distributed in the hope it will be useful, but WITHOUT
#  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#  FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
#  more details.
# 
#  You should have received a copy of the GNU Lesser General Public License along with
#  this program; if not, write to the Free Software Foundation, Inc., 
#  51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
# ---------------------------------------------------------------------------


# This will put all the example programs through the paces under all
# possible scheduler configurations.  

# ---------------------------------------------------------------------------
# Usage: [set env vars] ./run_all_tests

# Call it with environment variable LONGRUN=1 to get a longer run that
# can serve as benchmarks.

# Call it with THREADS="1 2 4" to run with # threads = 1, 2, or 4.

# Call it with NONSTRICT=1 to keep going after the first error.

# Call it with TRIALS=N to control the number of times each benchmark is run.
# ---------------------------------------------------------------------------


# Settings:
# ----------------------------------------

#export GHC=ghc-6.13.20100511 
#export GHC=~/bin/Linux-i686/bin/ghc-6.13.20100511
unset HASKELLCNC


  # Which subset of schedures should we test:
PURESCHEDS="2"
#IOSCHEDS="3"
IOSCHEDS="3 4 5 6 7 8"

if [ "$THREADSETTINGS" == "" ] 
then THREADSETTINGS="4"
#then THREADSETTINGS="0 1 2 3 4"
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
echo "# "`uname -a` >> $RESULTS
echo "# "`ghc -V` >> $RESULTS
echo "# "
echo "# Running each test for $TRIALS trials." >> $RESULTS
echo "#  ... with default compiler options: $GHC_DEFAULT_FLAGS" >> $RESULTS
echo "#  ... with default runtime options: $GHC_DEFAULT_RTS" >> $RESULTS

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

  # We compile the test case using runcnc:
  NORUN=1 ./runcnc $hashtab examples/"$test".hs
  CODE=$?
  check_error $CODE "ERROR: compilation failed."

  echo "Executing ./ntimes_minmedmax "$TRIALS" ./examples/$test.exe $ARGS +RTS $RTS -RTS "
  if [ "$LONGRUN" == "" ]; then export HIDEOUTPUT=1; fi
  times=`./ntimes_minmedmax "$TRIALS" ./examples/$test.exe $ARGS +RTS $RTS -RTS`
  CODE=$?

  echo " >>> MIN/MEDIAN/MAX TIMES $times"

  check_error $CODE "ERROR: run_all_tests this test failed completely: $test.exe"

  if [ "$CODE" != "0" ] && [ "$CODE" != "143" ];
  then echo "$test.exe" "$CNC_VARIANT" "$CNC_SCHEDULER" "$NUMTHREADS" "$HASH" "ERR ERR ERR" >> $RESULTS
  else 
       echo "$test.exe" "$CNC_VARIANT" "$CNC_SCHEDULER" "$NUMTHREADS" "$HASH" "$times" >> $RESULTS
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

# Hygiene:
rm -f examples/*.exe

# This specifies the list of tests and their arguments for a "long" run:

#for line in "mandel_opt 1 300 300 4000" "mandel_opt 2 300 300 4000" "mandel_opt 3 300 300 4000" "mandel 300 300 4000"; do

#  
#for line in  "par_seq_par_seq 8.5" "embarrassingly_par 9.2" "primes2 200000" "mandel 300 300 4000" "mandel_opt 1 300 300 4000" "sched_tree 18" "fib 20000" "threadring 50000000 503" "nbody 1200" "primes 200000"; do

# Parallel benchmarks only:
for line in  "par_seq_par_seq 8.5" "embarrassingly_par 9.2" "primes2 200000" "mandel 300 300 4000" "mandel_opt 1 300 300 4000" "sched_tree 18" "nbody 1200" "primes 200000"; do

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

 export CNC_VARIANT=pure
 # Currently running the pure scheduler only in single threaded mode:
 export NUMTHREADS=0
 for sched in $PURESCHEDS; do
   unset hashtab
   export CNC_SCHEDULER=$sched
   runit
 done

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

 # Finally, run once through separately compiled modules to compare performance (and make sure they build).
 # This will basically use the IO based implementation with the default scheduler.
 export CNC_VARIANT=separatemodule_io
 export CNC_SCHEDULER=6
 export NUMTHREADS=4
 runit

 echo >> $RESULTS;
 echo >> $RESULTS;
done

echo "Finished with all test configurations."
