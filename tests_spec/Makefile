
FLAGS= -I$(CNC_INSTALL_DIR)/include -L$(CNC_INSTALL_DIR)/lib/$(CNC_ARCH_PLATFORM) -ltbbmalloc -ltbb -lcnc 

EXES= tagfun_checking_valid.exe tagfun_depends.exe \
      simple_cycle.exe bigger_cycle.exe two_cycles.exe \
      reduction_test.exe reduction_test2.exe

ifeq (Darwin,$(shell uname))
  FLAGS+= -m32 
endif

all: $(EXES)

CNCTOOL="../build/cnc"
#CNCTOOL="../cnc.sh"

#============================================================
# Currently we give a separate rule for each test to exercise
# different translator options:

tagfun_checking_valid.h: tagfun_checking_valid.cnc  
	$(CNCTOOL) trans --autodonedbg --debug $<

tagfun_depends.h: tagfun_depends.cnc
	$(CNCTOOL) trans --autodonedbg --depends $<

simple_cycle.h: simple_cycle.cnc
	$(CNCTOOL) trans --autodonedbg  $<

bigger_cycle.h: bigger_cycle.cnc
	$(CNCTOOL) trans --autodonedbg  $<

two_cycles.h: two_cycles.cnc
	$(CNCTOOL) trans --autodonedbg  $<

reduction_test.h: reduction_test.cnc
	$(CNCTOOL) trans --autodonedbg  $<

reduction_test2.h: reduction_test2.cnc
	$(CNCTOOL) trans --autodonedbg  $<

#============================================================


%.exe : %.cpp %.h
	time g++ $(FLAGS) $< -o $@ 
#	time g++ $(FLAGS) $^ -o $@ 
	@echo
	@echo

run:
	@echo; echo; echo "================================================================================"
	./tagfun_checking_valid.exe 
	@echo; echo; echo "================================================================================"
	./tagfun_depends.exe 
	@echo; echo; echo "================================================================================"
	./simple_cycle.exe 
	@echo; echo; echo "================================================================================"
	./bigger_cycle.exe
	@echo; echo; echo "================================================================================"
	./two_cycles.exe
	@echo; echo; echo "================================================================================"
	./reduction_test.exe
	@echo; echo; echo "================================================================================"
	./reduction_test2.exe

clean:
	rm -f *.o *.exe tagfun_checking_valid.h tagfun_depends.h simple_cycle.h bigger_cycle.h

