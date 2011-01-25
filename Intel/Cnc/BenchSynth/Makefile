




CC = icc
OPT= -O3
FLAGS= -lcnc -ltbb -ltbbmalloc -I$(CNCROOT)/distro/include -L$(CNCROOT)/distro/lib/$(CNC_ARCH_PLATFORM)

all: 
	$(CC) $(OPT) $(FLAGS) cnc_produceReduce.cpp -o cnc_produceReduce.exe

clean:
	rm -rf cnc_prod*.exe*
