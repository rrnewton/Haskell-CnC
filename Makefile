

# WARNING: This Makefile is largely redundant with the .cabal file and will be deleted at some point.

# It has some crufty shortcuts used by the developers.

#====================================================================================================

all: runtime 
#all: runtime trans

ifeq (,$(GHC))
  GHC= ghc
endif

#====================================================================================================
# First, some entrypoints that help build the runtime
#====================================================================================================

# PARGHCOPTS=-feager-blackholing

default: 
        # This target builds CnC as precompiled modules:
        # Pick default schedulers as well:
	$(GHC) --make -c -cpp                   Intel/CncUtil.hs
	$(GHC) --make -c -cpp -DCNC_SCHEDULER=2 Intel/CncPure.hs
	$(GHC) --make -c -cpp -DCNC_SCHEDULER=5 Intel/Cnc.hs

runtime:
	runhaskell ./Setup.hs configure
	runhaskell ./Setup.hs build
	@echo 
# TEMP [2010.10.06] Disabling doc build
#	$(MAKE) setup_doc
#	$(MAKE) setup_test

setup_doc:
	@echo ================================================================================
	@echo   Building Documentation
	@echo ================================================================================
	runhaskell ./Setup.hs haddock

setup_test:
	@echo 
	@echo ================================================================================
	@echo   Running Test executable in the cabal distribution package
	@echo ================================================================================
	runhaskell ./Setup.hs test


interact:
	ghci -cpp -DCNC_SCHEDULER=2 Intel/CncPure.hs

interactIO:
	ghci -cpp -DCNC_SCHEDULER=5 Intel/Cnc.hs

test: 
	THREADS=1 ./run_all_tests.sh

longtest: 
	TRIALS=3 NONSTRICT=1 LONGRUN=1 THREADSETTINGS=="0 1 2 3 4 8" ./run_all_examples.sh &> /dev/stdout | tee all_tests.log
#	TRIALS=3 NONSTRICT=1 LONGRUN=1 THREADSETTINGS=="0 1 2 3 4 8" ./run_all_examples.sh &> /dev/stdout | tee all_tests.log
#	LONGRUN=1 THREADSETTINGS="0 1 2 3 4 8" ./run_all_tests.sh &> /dev/stdout | tee all_tests.log


#	TRIALS=3 NONSTRICT=1 LONGRUN=1 THREADSETTINGS="0 1 2 3 4 6 7 8 10 11 12 16 20 24 28 32 36 38 42 46 47 48 " ./run_all_FROMPACKED.sh &> /dev/stdout | tee all_tests.log


distro: pkg 
pkg:
	./build_distro.sh

wc:
	cloc --by-file $(FILES)
	wc $(FILES)


DOCBASE=html_doc

doc: 
	mkdir -p $(DOCBASE)/url/Intel/
	ls Intel/*.hs | xargs -i HsColour -html {} -ohtml_doc/url/{}
	haddock  --source-base=url/ --source-module=url/%F -o html_doc -html --optghc -cpp Intel/Cnc.hs Intel/CncPure.hs

distclean: clean
	rm -rf distro_20*
# DO NOT DELETE: Beginning of Haskell dependencies
Intel/CncUtil.o : Intel/CncUtil.hs
Intel/Cnc3.o : Intel/Cnc3.hs
Intel/Cnc3.o : Intel/CncUtil.hi
examples/test_parfor.o : examples/test_parfor.hs
examples/test_parfor.o : Intel/Cnc3.hi
# DO NOT DELETE: End of Haskell dependencies

#====================================================================================================
# For benchmark test runs:

cleanrun:
	rm -f uname.txt cpuinfo issue all_tests*.log results.dat

mvrun:
	mkdir -p last_run
	cp uname.txt cpuinfo issue all_tests*.log results.dat last_run/

UNAME=$(shell uname -n )
BENCHPACK="benchpack_$(UNAME)"

# Make a separate directory for running benchmarks.
benchpack:
	mkdir -p "$(BENCHPACK)/examples/"
	mkdir -p "$(BENCHPACK)/Intel/"
	cp -a *big_run*.sh default_opt_settings.sh ntimes ntimes_minmedmax ntimes_binsearch.sh binsearch_throughput timeout.sh timeout.hs \
          benchlist*.txt install_environment_vars.sh runcnc run_all_shared.sh run_all_examples.sh haskell_cnc.h "$(BENCHPACK)/"
	cp -a Intel/*.hs     "$(BENCHPACK)/Intel/"
	cp -a examples/*.hs  "$(BENCHPACK)/examples/"
	cp -a examples/*.dat "$(BENCHPACK)/examples/"


#====================================================================================================

clean: cleanruntime cleantests
	runhaskell ./Setup.hs clean

cleanruntime:
	rm -f ./Intel/*.o ./Intel/*.hi Intel/*~ 
	rm -f ./Intel/Cnc/*.o Intel/Cnc/*.hi 
	rm -f $(BUILDDIR)/Intel/*.o      $(BUILDDIR)/Intel/*.hi 
	rm -f $(BUILDDIR)/Intel/Cnc/*.o  $(BUILDDIR)/Intel/Cnc/*.hi 
	rm -f *.aux little*.log 
	(cd examples; $(MAKE) clean)

cleantests:
	(cd tests_spec; $(MAKE) clean)


#====================================================================================================

# Prevent odd make builtin rules re: cnc.sh
.SUFFIXES:
