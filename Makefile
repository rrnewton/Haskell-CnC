

# PARGHCOPTS=-feager-blackholing

all: 
        # This target builds CnC as precompiled modules:
        # Pick default schedulers as well:
	ghc --make -c -cpp -DCNC_SCHEDULER=2 Intel/CncPure.hs
	ghc --make -c -cpp -DCNC_SCHEDULER=6 Intel/Cnc.hs

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

clean:
	rm -f Intel/*.o Intel/*.hi Intel/*~ 
	(cd examples; $(MAKE) clean)

distclean: clean
	rm -rf distro_20*
