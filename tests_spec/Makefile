
FLAGS= -I$(CNC_INSTALL_DIR)/include -L$(CNC_INSTALL_DIR)/lib/$(CNC_ARCH_PLATFORM) -ltbbmalloc -ltbb -lcnc 


EXES= tagfun_checking_valid.exe tagfun_depends.exe \
      simple_cycle.exe bigger_cycle.exe two_cycles.exe \
      reduction_test.exe reduction_test2.exe

ERRONEOUS = legacy/test001.cnc \
            legacy/test007.cnc \
            legacy/test008.cnc \
            legacy/test009.cnc \
            legacy/test010.cnc \
            legacy/test011.cnc \
            legacy/test013.cnc \
            legacy/test021.cnc \
            legacy/test026.cnc \
            legacy/test030.cnc \
            legacy/test038.cnc \
            legacy/test047.cnc \
            legacy/test053.cnc \
            legacy/test054.cnc \
            legacy/test055.cnc \
            legacy/test056.cnc \
            legacy/test059.cnc \
            legacy/test060.cnc \
            legacy/test061.cnc \
            legacy/test062.cnc \
            legacy/test063.cnc \
            legacy/test064.cnc \
            legacy/test065.cnc \
            legacy/test066.cnc \
            legacy/test067.cnc \
            legacy/test068.cnc \
            legacy/test069.cnc \
            legacy/test070.cnc \
            legacy/test071.cnc \
            legacy/test072.cnc \
            legacy/test073.cnc \
            legacy/test074.cnc \
            legacy/test075.cnc \
            legacy/test076.cnc \
            legacy/test077.cnc \
            legacy/test078.cnc \
            legacy/test079.cnc \
            legacy/test080.cnc \
            legacy/test081.cnc \
            legacy/test082.cnc \
            legacy/test083.cnc \
            legacy/test084.cnc \
            legacy/test085.cnc \
            legacy/test086.cnc \
            legacy/test088.cnc \
            legacy/test089.cnc \
            legacy/test090.cnc \
            legacy/test091.cnc \
            legacy/test092.cnc \
            legacy/test093.cnc \
            legacy/test094.cnc \
            legacy/test095.cnc \
            legacy/test096.cnc \
            legacy/test097.cnc \
            legacy/test098.cnc \
            legacy/test099.cnc \
            legacy/test101.cnc \
            legacy/test102.cnc \
            legacy/test103.cnc \
            legacy/test104.cnc \
            legacy/test105.cnc \
            legacy/test106.cnc \
            legacy/test107.cnc \
            legacy/test108.cnc \
            legacy/test109.cnc \
            legacy/test110.cnc \
            legacy/test111.cnc \
            legacy/test112.cnc \
            legacy/test113.cnc \
            legacy/test114.cnc \
            legacy/test115.cnc \
            legacy/test116.cnc \
            legacy/test117.cnc \
            legacy/testencoder.cnc \

# These are things that used to be accepted by the old version but now are NOT.
DISCREPANCIES = \
            legacy/test004.cnc \
            legacy/test014.cnc \
            legacy/test016.cnc \
	    legacy/test017.cnc \
	    legacy/test018.cnc \
	    legacy/test019.cnc \
	    legacy/test020.cnc \
	    legacy/test022.cnc \
	    legacy/test023.cnc \
	    legacy/test024.cnc \
	    legacy/test025.cnc \
	    legacy/test027.cnc \
	    legacy/test028.cnc \
	    legacy/test029.cnc \
            legacy/test033.cnc \
            legacy/test048.cnc \
             legacy/test049.cnc \
             legacy/test051.cnc \
             legacy/test052.cnc \
             legacy/test057.cnc \
             legacy/test058.cnc \
             legacy/test087.cnc \
             legacy/testcholesky_priorities.cnc \
             legacy/testMatrixInvert.cnc \
             legacy/testnq_cnc.cnc \
             legacy/testsearchinsortedtree.cnc \

# Reasons:
#   004 - prescription is no longer bidirectional.
#   014 - duplicate declarations no longer allowed.
#   16-29,048,49,51,52,57,58 - tag components not supported in legacy syntax
#       I ported 87 to 87B though... it's a useful test without the tag components.
#   33 - capitalized ENV is not a keyword.
#   34/35/39 - so called "complicated relations" are supported now.
#   36 -- ok this one should be at least a warning due to DUPLICATED relations
#   45 -- rule about environment using items, more of a style guide than language def.
#
#  testcholesky_priorities - no support for this yet, need arbitrary property lists.
#  testMatrixInvert - no support for tuners either.
#  testnq_cnc.cnc -- no tag collection -> env edges!
#  testsearchinsortedtree -- DITTO

WELLFORMED = legacy/test002.cnc \
             legacy/test003.cnc \
             legacy/test012.cnc \
             legacy/test015.cnc \
             legacy/test031.cnc \
             legacy/test032.cnc \
             legacy/test037.cnc \
             legacy/test040.cnc \
             legacy/test041.cnc \
             legacy/test042.cnc \
             legacy/test043.cnc \
             legacy/test044.cnc \
             legacy/test046.cnc \
             legacy/test050.cnc \
             legacy/test087B.cnc \
             legacy/testblackscholes.cnc \
             legacy/testcholesky.cnc \
             legacy/testdivconq.cnc \
             legacy/testfacedetector.cnc \
             legacy/testfindprimes.cnc \
             legacy/testgenIter.cnc \
             legacy/testpartstr.cnc \
             legacy/testprodcons.cnc \
             legacy/testredRows.cnc \
\
             legacy/test005.cnc \
             legacy/test006.cnc \
             legacy/test034.cnc \
             legacy/test035.cnc \
             legacy/test036.cnc \
             legacy/test039.cnc \
             legacy/test045.cnc \

# TODO: Put a warning on test036!!


ifeq (Darwin,$(shell uname))
  FLAGS+= -m32 
endif

all: $(EXES)

test: run
run:
	@echo
	@echo 
	@echo "Running known-good translation tests:"
	@echo "================================================================================"
	@$(MAKE) check_good
	@echo
	@echo
	@echo "Running known-bad translation tests:"
	@echo "================================================================================"
	@$(MAKE) check_bad
	@echo
	@echo 
	@$(MAKE) all
	@echo
	@echo "Running full system tests (code and spec):" 
	@echo "================================================================================"
	runhaskell ./run_tests.hs

bad: check_bad
check_bad:
	./run_erroneous.sh $(ERRONEOUS:.cnc=.h)

good: check_good
check_good: 
	$(MAKE) $(WELLFORMED:.cnc=.h)

# Here we have a little "search path" for the cnc executable, which
# may have been built in different ways or not built in binary
# executable form at all.
CNCTOOL=$(shell ./first_existing.sh ../build/cnc ../dist/build/cnc/cnc ../cnc.sh)

#============================================================
# Currently we give a separate rules for some tests that have
# non-standard arguments:

tagfun_checking_valid.h: tagfun_checking_valid.cnc  
	$(CNCTOOL) trans --autodonedbg --debug $<

tagfun_depends.h: tagfun_depends.cnc
	$(CNCTOOL) trans --autodonedbg --depends $<

# Here is a default rule to fall back on:
%.h: %.cnc
	$(CNCTOOL) trans --autodonedbg  $<

#============================================================

%.exe : %.cpp %.h
	time g++ $(FLAGS) $< -o $@ 
	@echo
	@echo

clean:
	rm -f *.o *.exe tagfun_checking_valid.h tagfun_depends.h simple_cycle.h bigger_cycle.h
	rm -f $(ERRONEOUS:.cnc=.h)  $(WELLFORMED:.cnc=.h)

