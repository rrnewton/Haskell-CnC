#include "./include/HarchTreeTraverser.h"
#include "./include/MetisGraph.h"

#include <iostream>
#include <fstream>

int main ( int argc, char* argv[] ) {
    if ( argc < 2 ) {
        std::cerr << "Usage: " << argv[0] << " fileName" << std::endl;
        return 1;
    }

    std::ifstream graphIn(argv[1]);

    MetisGraph* graph = MetisGraph::create ( graphIn );
    graphIn.clear();
    graphIn.seekg(std::ios_base::beg);

    HarchTreeTraverser traverser(graph);
    traverser.traverse();
    
    traverser.printAppendedGraph(graphIn);

    return 0;
}





