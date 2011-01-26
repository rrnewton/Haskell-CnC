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

# This version of the script uses binary search to scale up the input to the benchmark.

binsearch=`dirname $0`/binsearch_throughput

lines=`$binsearch --RTS $* | tee /dev/stderr | grep TRIAL | awk '{ print $4 }'`

echo $lines
