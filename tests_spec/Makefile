
FLAGS= -I$(CNC_INSTALL_DIR)/include -L$(CNC_INSTALL_DIR)/lib/$(CNC_ARCH_PLATFORM) -ltbbmalloc -ltbb -lcnc 

ifeq (Darwin,$(shell uname))
  FLAGS+= -m32 
endif

all: tagfun_checking_valid.exe tagfun_depends.exe simple_cycle.exe bigger_cycle.exe reduction_test.exe reduction_test2.exe

#	wtf g++ -m32 tagfuns1.cpp -ltbbmalloc  -ltbb -lcnc -I$(CNC_INSTALL_DIR)/include/

tagfun_checking_valid.h: tagfun_checking_valid.cnc  
	../cnc trans --autodone --debug $<

tagfun_depends.h: tagfun_depends.cnc
	../cnc trans --autodone --depends $<

simple_cycle.h: simple_cycle.cnc
	../cnc trans --autodone  $<

bigger_cycle.h: bigger_cycle.cnc
	../cnc trans --autodone  $<

reduction_test.h: reduction_test.cnc
	../cnc trans --autodone  $<

reduction_test2.h: reduction_test.cnc
	../cnc trans --autodone  $<

#============================================================

tagfun_checking_valid.exe: tagfun_checking_valid.cpp tagfun_checking_valid.h
	g++ $(FLAGS) tagfun_checking_valid.cpp -o $@

tagfun_depends.exe: tagfun_depends.cpp tagfun_depends.h
	g++ $(FLAGS) tagfun_depends.cpp -o $@


simple_cycle.exe: simple_cycle.cpp simple_cycle.h
	g++ $(FLAGS) simple_cycle.cpp -o $@

bigger_cycle.exe: bigger_cycle.cpp bigger_cycle.h
	g++ $(FLAGS) bigger_cycle.cpp -o $@

reduction_test.exe: reduction_test.cpp reduction_test.h
	g++ $(FLAGS) reduction_test.cpp -o $@

reduction_test2.exe: reduction_test2.cpp reduction_test2.h
	g++ $(FLAGS) reduction_test2.cpp -o $@

run:
	./tagfun_checking_valid.exe 
	./tagfun_depends.exe 
	./simple_cycle.exe 
	./bigger_cycle.exe
	./reduction_test.exe

clean:
	rm -f *.o *.exe tagfun_checking_valid.h tagfun_depends.h simple_cycle.h bigger_cycle.h

