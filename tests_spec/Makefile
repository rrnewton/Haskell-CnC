



FLAGS= -I$(CNC_INSTALL_DIR)/include -L$(CNC_INSTALL_DIR)/lib/$(CNC_ARCH_PLATFORM) -ltbbmalloc -ltbb -lcnc 

ifeq (Darwin,$(shell uname))
  FLAGS+= -m32 
endif


all: tagfun_checking_valid.exe tagfun_depends.exe

#	wtf g++ -m32 tagfuns1.cpp -ltbbmalloc  -ltbb -lcnc -I$(CNC_INSTALL_DIR)/include/

tagfun_checking_valid.exe: tagfun_checking_valid.cpp tagfun_checking_valid.cnc tagfun_checking_valid.h
	g++ $(FLAGS) tagfun_checking_valid.cpp -o $@


tagfun_depends.exe: tagfun_depends.cpp tagfun_depends.cnc tagfun_depends.h
	g++ $(FLAGS) tagfun_depends.cpp -o $@



clean:
	rm -f *.o *.exe
