

# WARNING: This Makefile is largely redundant with the .cabal file and will be deleted at some point.

#====================================================================================================


all: runtime 
#all: runtime trans

clean: cleanruntime cleantrans
	runhaskell ./Setup.hs clean

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

# upload datasets
upload:
	unison-2.32 upload_hcnc_datasets -batch -ui text
	ssh fort.ffh.us '(cd public_html/haskell-cnc/datasets/; tar czvf benchmarks.tgz benchmarks/)'

#fakescale:
#	ls datasets_NEW/*.dat | xargs -i ./sim_hypothetical_scalable_gc {} 

#====================================================================================================



#====================================================================================================
# Below is the (entirely separate) makefile for the translator.
#====================================================================================================


HSOURCE=SrcLoc.hs Main.hs GatherGraph.hs AST.hs Codegen/CppOld.hs Codegen/Haskell.hs \
        CncGraph.hs CncViz.hs TraceVacuum.hs Curses.hs Util.hs \
        Passes/ReadHarch.hs Passes/ReadCnC.hs Passes/TypeDefs.hs

BUILDDIR= ./build/
#BUILDDIR= ./
HCNCNAME=cnc

$(BUILDDIR):
	mkdir $(BUILDDIR)

# This is for use in the context of a complete Intel CnC distribution.
install: 
	rm -f Intel/Cnc/Spec/Version.hs
#	$(MAKE) Intel/Cnc/Spec/Version.hs $(HCNCNAME).stripped
#	cp $(BUILDDIR)/$(HCNCNAME).stripped `which $(HCNCNAME)`
	$(MAKE) Intel/Cnc/Spec/Version.hs release
	if [ -e ../../distro/ ]; then mkdir -p ../../distro/bin/$(CNC_ARCH_PLATFORM)/; fi
	if [ -e ../../distro/ ]; then cp $(BUILDDIR)/$(HCNCNAME).release ../../distro/bin/$(CNC_ARCH_PLATFORM)/cnc; fi

trans: 
	@echo 
	@echo ================================================================================
	@echo   Building Translator.
	@echo ================================================================================
	$(MAKE) $(BUILDDIR)/$(HCNCNAME)

Intel/Cnc/Spec/Version.hs: 
	runhaskell extract_version.hs

release: $(BUILDDIR)/$(HCNCNAME).release

$(BUILDDIR)/$(HCNCNAME).release: $(BUILDDIR)/$(HCNCNAME).stripped
	@echo Packing executable with UPX:
	rm -f $(BUILDDIR)/$(HCNCNAME).release
	upx $(BUILDDIR)/$(HCNCNAME).stripped -o $(BUILDDIR)/$(HCNCNAME).release

$(BUILDDIR)/$(HCNCNAME).stripped: viz
	@echo Stripping executable to reduce size.
	strip $(BUILDDIR)/$(HCNCNAME) -o $(BUILDDIR)/$(HCNCNAME).stripped

$(BUILDDIR)/$(HCNCNAME): $(BUILDDIR) preproc buildtrans
buildtrans: 
	ghc $(GHCFLAGS) --make Intel/Cnc/Spec/Main.hs -odir $(BUILDDIR) -o $(BUILDDIR)/$(HCNCNAME) -fwarn-unused-imports

viz: $(BUILDDIR) $(BUILDDIR) preproc
	ghc $(GHCFLAGS) -c Intel/Cnc/Spec/CncLexer.hs 
	ghc $(GHCFLAGS) -DCNCVIZ --make Intel/Cnc/Spec/Main.hs -odir $(BUILDDIR) -o $(BUILDDIR)/$(HCNCNAME)

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
	rm -rf $(BUILDDIR)/$(HCNCNAME)  $(BUILDDIR)/$(HCNCNAME).* 
	(cd Intel/Cnc/Spec/;         rm -f CncGrammar.hs CncLexer.hs *.hi)
	(cd Intel/Cnc/Spec/Codegen;  rm -f  *.hi)
	(cd $(BUILDDIR)/Intel/Cnc/Spec/;        rm -f *.o )
	(cd $(BUILDDIR)/Intel/Cnc/Spec/Codegen; rm -f *.o )



#====================================================================================================

# cd graphPartitioner; $(MAKE)

# Prevent odd make builtin rules re: cnc.sh
.SUFFIXES:
