
FLAGS= -I$(CNC_INSTALL_DIR)/include -L$(CNC_INSTALL_DIR)/lib/$(CNC_ARCH_PLATFORM) -ltbbmalloc -ltbb -lcnc 

EXES= tagfun_checking_valid.exe tagfun_depends.exe \
      simple_cycle.exe bigger_cycle.exe two_cycles.exe \
      reduction_test.exe reduction_test2.exe

ifeq (Darwin,$(shell uname))
  FLAGS+= -m32 
endif

all: $(EXES)

CNCTOOL="../build/cnc"

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

reduction_test2.h: reduction_test.cnc
	$(CNCTOOL) trans --autodonedbg  $<

#============================================================

# tagfun_checking_valid.exe: tagfun_checking_valid.cpp tagfun_checking_valid.h
# 	g++ $(FLAGS) tagfun_checking_valid.cpp -o $@

# tagfun_depends.exe: tagfun_depends.cpp tagfun_depends.h
# 	g++ $(FLAGS) tagfun_depends.cpp -o $@


# simple_cycle.exe: simple_cycle.cpp simple_cycle.h
# 	g++ $(FLAGS) simple_cycle.cpp -o $@

# bigger_cycle.exe: bigger_cycle.cpp bigger_cycle.h
# 	g++ $(FLAGS) bigger_cycle.cpp -o $@

# reduction_test.exe: reduction_test.cpp reduction_test.h
# 	g++ $(FLAGS) reduction_test.cpp -o $@

# reduction_test2.exe: reduction_test2.cpp reduction_test2.h
# 	g++ $(FLAGS) reduction_test2.cpp -o $@


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

