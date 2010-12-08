



FLAGS= -I$(CNC_INSTALL_DIR)/include -L$(CNC_INSTALL_DIR)/lib/$(CNC_ARCH_PLATFORM) -ltbbmalloc -ltbb -lcnc 

ifeq (Darwin,$(shell uname))
  FLAGS+= -m32 
endif


all: tagfun_checking_valid.exe tagfun_depends.exe simple_cycle.exe bigger_cycle.exe

#	wtf g++ -m32 tagfuns1.cpp -ltbbmalloc  -ltbb -lcnc -I$(CNC_INSTALL_DIR)/include/

tagfun_checking_valid.h: tagfun_checking_valid.cnc  
	../cnc trans --debug $<

tagfun_depends.h: tagfun_depends.cnc
	../cnc trans --depends $<

simple_cycle.h: simple_cycle.cnc
	../cnc trans --autodone  $<

bigger_cycle.h: bigger_cycle.cnc
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


clean:
	rm -f *.o *.exe tagfun_checking_valid.h tagfun_depends.h
