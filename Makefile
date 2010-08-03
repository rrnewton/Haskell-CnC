

all: runtime trans

clean: cleanruntime cleantrans

#====================================================================================================
# FIrst, some entrypoints that help build the runtime
#====================================================================================================

# PARGHCOPTS=-feager-blackholing

default: 
        # This target builds CnC as precompiled modules:
        # Pick default schedulers as well:
	ghc --make -c -cpp                   Intel/CncUtil.hs
	ghc --make -c -cpp -DCNC_SCHEDULER=2 Intel/CncPure.hs
	ghc --make -c -cpp -DCNC_SCHEDULER=5 Intel/Cnc.hs

runtime:
	cabal configure
	cabal build
	@echo 
	@echo ================================================================================
	@echo   Building Documentation
	@echo ================================================================================
	cabal haddock

	@echo 
	@echo ================================================================================
	@echo   Running Test executable in the cabal distribution package
	@echo ================================================================================
	./Setup.hs test


interact:
	ghci -cpp -DCNC_SCHEDULER=2 Intel/CncPure.hs

interactIO:
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


cleanruntime:
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


#====================================================================================================
# Below is the (entirely separate) makefile for the translator.
#====================================================================================================


HSOURCE=SrcLoc.hs Main.hs GatherGraph.hs AST.hs Codegen/CppOld.hs

trans: 
	@echo 
	@echo ================================================================================
	@echo   Building Translator.
	@echo ================================================================================
	$(MAKE) cnctrans


cnctrans: cnctrans.bloated
	@echo Stripping executable to reduce size.
	strip cnctrans.bloated -o cnctrans

cnctrans.bloated: preproc buildtrans


buildtrans: 
	ghc -c Intel/Cnc/Spec/CncLexer.hs 
	ghc -O --make Intel/Cnc/Spec/Main.hs -o cnctrans.bloated
#	ghc --make CncLexer.hs
#	ghc --make CncGrammar.hs


preproc: Intel/Cnc/Spec/CncLexer.hs Intel/Cnc/Spec/CncGrammar.hs

Intel/Cnc/Spec/CncLexer.hs: Intel/Cnc/Spec/CncLexer.x
	alex Intel/Cnc/Spec/CncLexer.x

Intel/Cnc/Spec/CncGrammar.y: Intel/Cnc/Spec/CncGrammar.y.pp
	cpp -P -CC $^ $@ 
#	gcc -x c -E $^ -o $@ 

Intel/Cnc/Spec/CncGrammar.hs: Intel/Cnc/Spec/CncGrammar.y
	happy  Intel/Cnc/Spec/CncGrammar.y


wctrans:
	(cd Intel/Cnc/Spec/; ln -f -s CncLexer.x CncLexer.temp.hs)
	(cd Intel/Cnc/Spec/; ln -f -s CncGrammar.y.pp CncGrammar.temp.hs)
	(cd Intel/Cnc/Spec/; cloc-1.08.pl --by-file CncLexer.temp.hs CncGrammar.temp.hs $(HSOURCE))

cleantrans:
	rm -f cnctrans cnctrans.bloated
	(cd Intel/Cnc/Spec/; rm -f CncGrammar.hs CncLexer.hs *.o *.hi)
	(cd Intel/Cnc/Spec/Codegen; rm -f *.o *.hi)
	(cd Intel/Cnc/Spec/tests/; rm -f *.h)

