// summer 2010
// sagnak tasirlar
// sagnak@rice.edu , sagnak.tasirlar@intel.com
// 
//
// given a metis file (root) and all the transitive-closure of its childen (in the same directory)
// extend the comment line for the vertice to include 'partitions=<number>(:<number>)*;' (read as reg-ex)
// where the 'partitions' list tells the path for that vertice from root to leaf
//
// i.e partitions=3:1:0; is read as : 
// -this vertice 
//      is a member of the fourth partition of the root,
//      is a member of second partition of that partition one line above,
//      is a member of first partition of that partition one line above
//
//
// command line arguments
// 1(needed) root metis graph on which a partitioning is applied 
// - note: for convenience the root has a copy suffixed .imported.0, not that name but the original file name
//
// 2(needed) branching factor for the whole hierarchical partition tree
//
//
// output
// metis files with a preceding comment line(if chosen), that includes name and directions, see below for further info
//
// keywords: HARCHNODE name directions partitions orders
// punctuation: = ;
// i.e. % HARCHNODE name=<verticeName>; directions=[01]*; partitions=0..<branchingFactor-1>\(:[0..<branchingFactor-1>]\)*;  
// <> 's above mean you can put anything, not part of representation
// [] and * as in reg-ex, [] choose one, * as kleene, 0 is a reverse edge, 1 is an actual edge
// the reason behind this is that we do not want to lose the directions of the edges (we care what depends on what)
// and metis does not, so we note it into comments and make use of it afterwards
//
//
// TODO for maintainer
// 
//
//
//
//

#include "./include/PartitionFileTreeTraverser.h"

#include <iostream>
#include <fstream>
#include <sstream>

#include <cstdlib>

int main ( int argc, char* argv[] ) {
    if ( argc < 3 ) {
	    std::cerr<< "Usage: " << argv[0] << " metisFileToAnnotateWithPartitions branchingFactor "<<std::endl;
	    return 1;
    }

    std::stringstream fileNamePrefix;
    // the original file name is given as command line argument so we add the suffix needed to reach the root node file
    fileNamePrefix << argv[1] << ".imported.0";

    std::ifstream originalIn(fileNamePrefix.str().c_str());
    std::string lineBuffer;
    //assumes the file starts with number of nodes and NOT with a comment
    std::getline(originalIn, lineBuffer);
    std::stringstream lineStream(lineBuffer);

    int nNodes = 0;
    // read the number of lines from the file
    lineStream >> nNodes;

    // create the traverser object with members as input file name base, branching factor and number of nodes on the root metis file
    // file name base is needed since all files representing inner and leaf nodes on the hierarchical partition tree will have the same
    // prefix + '.imported.' + a number following the labeling scheme mentioned in createPartitions.cpp, if that file name changes grep
    // for 'ASCII art'
    PartitionFileTreeTraverser tree(argv[1], std::atoi(argv[2]), nNodes);
    tree.traverse(0,1);

    originalIn.seekg(std::ios_base::beg); // return to the beginning of file
    tree.printAppendedGraph(originalIn);
    return 0;
}
