#!/bin/bash

cd `dirname $0`

# This is an example of how to run the system for performance testing:

export THREADSETTINGS="8 6 4 3 2 1"

time TRIALS=3 NONSTRICT=1 LONGRUN=1  ./run_all_examples.sh &> /dev/stdout | tee all_tests_4core.log
