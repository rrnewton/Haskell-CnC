// summer 2010
// sagnak tasirlar
// sagnak@rice.edu , sagnak.tasirlar@intel.com
// 
//
// given a metis file and the partitioning for it, create children as new metis files
//
//
// command line arguments
// 1(needed) metis graph on which a partitioning is applied 
//
// 2(needed) metis output from a partitioning 
// - for those who are not familiar, this file is named <inputFile>.part.<branchingFactor>
// - each line is a number between [0-branchingFactor) representing which vertice is assigned which partition
//
//
// output
// metis files with a preceding comment line(if chosen), that includes name and directions, see below for further info
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
//
// TODO for maintainer
// the line below assumes the file name looks like <inputFile>.part.<branchingFactor>, which is true for metis
//      int numSubGraphs = std::atoi(filterFileName.substr(filterFileName.find_last_of('.')+1).c_str());
//      if another partitioner is used or if metis changes this naming scheme, this should be fixed
// 
// if no comments are wanted for vertice on the partitions represented as new graph files
//      this line should replace true with false curr->print(true, out); 
//
//
//
// labeling scheme (yes, in ASCII art)
// - breadth first traversal of the tree, siblings visited left to right
// - i.e. if you look at the tree as a k-heap, that is how you would set the array indexes
//
// k <- branching factor
// root                 0
//                   / .|. \
//                  / . | . \
//                 / .. | .. \
// level-1       1  ...   ...  k               } k siblings
//             / .\  ...... / .. \
//            / .. \  .... / .... \
// level-2  k+1 .. 2k .. k*k+1 . k*k+k-1
//          .............................
//

#include <iostream>
#include <fstream>
#include <sstream>

#include "./include/MetisGraph.h"

int main ( int argc, char* argv[] ) {

	if( argc <= 3 ) {
		std::cerr << "usage: " << argv[0] << " partitionedMetisFile metisPartitionOutputFile "<<std::endl;
        return 1;
	}

	std::string graphName(argv[1]);
	std::ifstream parent(graphName.c_str());

	MetisGraph* graph = MetisGraph::create ( parent );

    std::string filterFileName(argv[2]);
    // filter file names end with .branchingFactor since metis naming scheme is that way, considered safe
	int numSubGraphs = std::atoi(filterFileName.substr(filterFileName.find_last_of('.')+1).c_str());
	std::ifstream filter(filterFileName.c_str());

    // apply the filtering provided by the partitioning output
    // after this call the metis graph object 'graph', will have 'numSubGraph' partitions filled as
    // sub-graphs of type GraphBase ( parent of MetisGraph class )
    graph->applyFilter ( numSubGraphs, filter ) ;

    // leafIndex holds the value to keep track of where does this node fit in a partition graph
    // see the top of this file for the labeling scheme 
	int leafIndex = std::atoi(graphName.substr(graphName.find_last_of('.')+1).c_str()) ;
	std::string graphNameBase = graphName.substr(0,graphName.find_last_of('.'));

    std::stringstream sout;
    // 'nonEmptyCount' help label only the non-empty children from 0 to how many non empty children there are
    // this is just for convenience and reduces check on a few else-where
	for ( int i = 0, nonEmptyCount = 0 ; i < numSubGraphs ; ++i ) {
        const GraphBase* curr = graph->getPartition(i);
        // metis can return empty partitions
        if (!curr->empty()) {
            // see labeling scheme for the cryptic expression
            sout << graphNameBase <<"."<<leafIndex*numSubGraphs+nonEmptyCount+1;
            std::ofstream out(sout.str().c_str());
            curr->print(true, out);
            out.close();
            sout.str("");
            sout.clear();
            ++nonEmptyCount;
        }
	}

 	return 0;
}
