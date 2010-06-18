#!/bin/bash

cd `dirname $0`

uname -a > uname.txt

cp /proc/cpuinfo .
cp /etc/issue .

export THREADSETTINGS="32 31 30 29 28 24 20 16 12 11 10 8 6 4 2 1"

# This version assumes that things have been PACKED first:

time TRIALS=3 NONSTRICT=1 LONGRUN=1  ./run_all_FROMPACKED.sh &> /dev/stdout | tee all_tests_32core.log
