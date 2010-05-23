#!/bin/bash

EXAMPLES="hello_world.hs mandel.hs primes.hs primes2.hs sched_tree.hs threadring_onestep.hs threadring.hs embarrassingly_par.hs fib.hs nbody.hs"

OTHER="Intel/Cnc.hs Intel/CncPure.hs Intel/CncUtil.hs  haskell_cnc.h LICENSE  README.txt Makefile install_environment_vars.sh default_opt_settings.sh ntimes ntimes_minmedmax runcnc run_all_tests.sh timeout scaling.hs Setup.hs haskell-cnc.cabal"

DATE=`date +"%Y.%m.%d"`
DISTRO="./distro_$DATE"
#DISTRO=../../distro/haskell_cnc

rm -rf $DISTRO
mkdir -p "$DISTRO/examples"

cd examples
for file in $EXAMPLES; do 
  echo "Copying/prepending license: $file"
  cat ../HEADER.txt $file > "../$DISTRO/"examples/$file
done
cd ..

mkdir -p "$DISTRO/Intel"

for file in $OTHER; do 
  echo "Copying: $file"
  cp -a "$file" "$DISTRO"/"$file"
done


rm ./distro
ln -s "$DISTRO" ./distro

#cp $(FILES) $(DISTRO)
#cp -a haskell_cnc.h runcnc run_all_tests.sh ntimes* install_environment_vars.sh README.txt Makefile $(DISTRO)

