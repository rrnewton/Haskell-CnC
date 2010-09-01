// summer 2010
// sagnak tasirlar
// sagnak@rice.edu , sagnak.tasirlar@intel.com
// 
// parse a Profile graph and dump Metis equivalent
//
// command line arguments
// 1(needed)  input file name , representing  a Profile graph file with profiling info
// 2(optional) verbosity , if set names for vertices are strings parsed else just numbers, default is numbers
//
// output
// metis file with a preceding comment line(if chosen), that includes name and directions, see below for further info
//
// keywords: HARCHNODE name directions partitions orders
// punctuation: = ;
// i.e. % HARCHNODE name=<verticeName>; directions=[01]*;  
// <> 's above mean you can put anything, not part of representation
// [] and * as in reg-ex, [] choose one, * as kleene, 0 is a reverse edge, 1 is an actual edge
// the reason behind this is that we do not want to lose the directions of the edges (we care what depends on what)
// and metis does not, so we note it into comments and make use of it afterwards
//
//
// TODO for maintainer
// assumption are listed above, make sure they are accurate
// if file does not have profiling info, comment out line graph->parseExecutionTimes(inFile);
// if the output metis file is to be dumped to a stream other than stdout change graph->print(true, std::cout); to dump to that stream
// if the output metis file is to be dumped without the comment line above change graph->print(true, std::cout); true to false

#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>

#include "./include/Profile_Graph.h"

int main( int argc, char* argv[] ) {
	bool verbose = false;

	if( argc == 1 ) {
		std::cerr << "usage: " << argv[0] << " filename [-v]"<<std::endl;
        return 1;
	} else if (argc == 3 && !std::string(argv[2]).compare("-v")) {
		verbose = true;
	}
	std::ifstream inFile(argv[1]);

    // assumes the first line of the input file has a number and is the #vertices
    Profile_Graph* graph = Profile_Graph::create(inFile);
        
    // assumes the vertices start right after the line mentioned above
    // assumes the line is "Node # name", where those 3 are separated with space
    //
    // verbose => true: prints names of nodes parsed from file, false: prints zero indexed numbers instead
    graph->parseVertices(inFile, verbose);


    // assumes the first line after vertices has a number and is the #edges
    // assumes the line has a colon (:) that is followed by a number a string and another number all space separated
    graph->parseEdges(inFile);


    // assumes the first line after edges is a execution time profile line
    graph->parseExecutionTimes(inFile);
    // the hardcoded true is for creating the comment line needed for further info about the vertice
	graph->print(true, std::cout);

    // one might say bad practice, did not create in this scope but deallocated in this scope
    // authors perception is static create (factory like) methods are no different than a glorified new
    delete graph;
    return 0;
}

