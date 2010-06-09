

# PARGHCOPTS=-feager-blackholing

default: 
        # This target builds CnC as precompiled modules:
        # Pick default schedulers as well:
	ghc --make -c -cpp                   Intel/CncUtil.hs
	ghc --make -c -cpp -DCNC_SCHEDULER=2 Intel/CncPure.hs
	ghc --make -c -cpp -DCNC_SCHEDULER=5 Intel/Cnc.hs

all:
	cabal configure
	cabal build
	cabal haddock
	./Setup.hs test

interact:
	ghci -cpp -DCNC_SCHEDULER=2 Intel/CncPure.hs

interactio:
	ghci -cpp -DCNC_SCHEDULER=5 Intel/Cnc.hs

test: 
	THREADS=1 ./run_all_tests.sh

longtest: 
	TRIALS=5 NONSTRICT=1 LONGRUN=1 THREADSETTINGS="0 1 2 3 4 8" ./run_all_tests.sh &> /dev/stdout | tee all_tests.log
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


clean:
	rm -f Intel/*.o Intel/*.hi Intel/*~ 
	rm -f *.aux little*.log 
	(cd examples; $(MAKE) clean)

distclean: clean
	rm -rf distro_20*
# DO NOT DELETE: Beginning of Haskell dependencies
Intel/CncUtil.o : Intel/CncUtil.hs
Intel/Cnc3.o : Intel/Cnc3.hs
Intel/Cnc3.o : Intel/CncUtil.hi
examples/test_parfor.o : examples/test_parfor.hs
examples/test_parfor.o : Intel/Cnc3.hi
# DO NOT DELETE: End of Haskell dependencies
