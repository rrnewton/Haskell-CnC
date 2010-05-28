

# PARGHCOPTS=-feager-blackholing

default: 
        # This target builds CnC as precompiled modules:
        # Pick default schedulers as well:
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
	NONSTRICT=1 LONGRUN=1 THREADSETTINGS="0 1 2 3 4 8" ./run_all_tests.sh | tee all_tests.log
#	LONGRUN=1 THREADSETTINGS="0 1 2 3 4 8" ./run_all_tests.sh &> /dev/stdout | tee all_tests.log

distro: pkg 
pkg:
	./build_distro.sh

wc:
	cloc --by-file $(FILES)
	wc $(FILES)


doc: 
	mkdir -p html_doc/url/Intel/
	ls Intel/*.hs | xargs -i echo HsColour -html {} -o html_doc/url/Intel/{}
#	haddock  --source-base=url/ --source-module=url/%F -o html_doc -html --optghc -cpp Intel/Cnc.hs Intel/CncPure.hs
#	cp Intel/*.hs html_doc/url/Intel/


clean:
	rm -f Intel/*.o Intel/*.hi Intel/*~ 
	(cd examples; $(MAKE) clean)

distclean: clean
	rm -rf distro_20*
