
FLAGS= -I$(CNC_INSTALL_DIR)/include -L$(CNC_INSTALL_DIR)/lib/$(CNC_ARCH_PLATFORM) -ltbbmalloc -ltbb -lcnc 


EXES= tagfun_checking_valid.exe tagfun_depends.exe \
      simple_cycle.exe bigger_cycle.exe two_cycles.exe \
      reduction_test.exe reduction_test2.exe

ERRONEOUS = legacy/test001.cnc \
            legacy/test004.cnc \
            legacy/test007.cnc \
            legacy/test008.cnc \
            legacy/test009.cnc \
            legacy/test010.cnc \
            legacy/test011.cnc \
            legacy/test013.cnc \
            legacy/test014.cnc \


WELLFORMED = legacy/test002.cnc \
             legacy/test003.cnc \
             legacy/test005.cnc \
             legacy/test006.cnc \
             legacy/test012.cnc \
             legacy/test015.cnc \
             legacy/test031.cnc \


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


CNCTOOL="../build/cnc"
# CNCTOOL="../cnc.sh"

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

