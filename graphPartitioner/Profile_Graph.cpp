#include <sstream>
#include "./include/Profile_Graph.h"

const char Profile_Graph::digits[] = "1234567890";

Profile_Graph::Profile_Graph ( unsigned int numVertices ) : GraphBase( numVertices ) { 
}

Profile_Graph* Profile_Graph::create ( std::istream& in ) {
    Profile_Graph* toBeReturned = NULL;

    try {
        std::string currLine;
        std::getline(in, currLine);
        std::istringstream currLineStream(currLine.substr(currLine.find_first_of(Profile_Graph::digits)));

        unsigned int numVertices = 0;
        currLineStream >> numVertices ;
        toBeReturned = new Profile_Graph (numVertices);
        currLineStream.clear();
    } catch (std::exception& e ){
        std::cerr << e.what() << std::endl;
    }

	return toBeReturned;
}

void Profile_Graph::parseVertices ( std::istream& inFile, bool verbose ) {
    std::string currLine;
    for ( unsigned int i = 0 ; i < m_numVertices ; ++i) {
        std::getline(inFile, currLine);
        size_t currWS = currLine.find_first_of(' ');
        std::string name;
        if (verbose){
            currWS = currLine.find_first_of(' ', currWS+1); 
            if( currWS != std::string::npos ) {
                name = currLine.substr(currWS+1);
            }
        } else {
            currWS = currLine.find_first_of(' ', currWS+1); // let's assume lhv currWS is never npos because of Node_#
            name = currLine.substr(0, currWS);
        }
        pushVertex( i+1, name );
    }
}

void Profile_Graph::parseEdges ( std::istream& inFile ) {
    std::string currLine;
    std::getline(inFile, currLine);
    std::istringstream currLineStream ( currLine.substr ( currLine.find_first_of ( digits ) ) );
    unsigned int readNumEdges;
    currLineStream >> readNumEdges;
    currLineStream.clear();

    // edges
    for ( unsigned int i = 0 ; i < readNumEdges; ++i) {
        std::string dummy;
        unsigned int from = 0;
        unsigned int to = 0;

        std::getline(inFile, currLine);
        currLineStream.str( currLine.substr( currLine.find_first_of(':') + 1 ) ); 
        currLineStream >> from >> dummy >>to;
        pushDirectedEdge(from, to);

        currLineStream.clear();
    }
}

void Profile_Graph::updateVertex(unsigned int id, double weight, unsigned int numCalls ) {
    Profile_Vertex* currVertex = dynamic_cast<Profile_Vertex*>(m_vertexTable[id]);
    currVertex->setWeight(Profile_Vertex::integerify(weight));
    currVertex->setNumCalls(numCalls);
}

void Profile_Graph::parseExecutionTimes ( std::istream& inFile ) {
    std::istringstream currLineStream;
    for ( unsigned int i = 0 ; i < m_numVertices; ++i) {
        double currWeight;
        int currCallTimes;

        std::string currLine;
        std::getline(inFile, currLine);
        size_t currWS = currLine.find_first_of(' ');
        currWS = currLine.find_first_of(' ', currWS+1); // let's assume lhv currWS is never npos because of Node_#
        currWS = currLine.find_first_of(' ', currWS+1); // let's assume lhv currWS is never npos because of Node_#_runtime
        currLineStream.str( currLine.substr(currWS+1) );
        currLineStream >> currWeight;
        currLineStream.clear();

        currLineStream.str(currLine.substr(currLine.find_last_of(' ')));
        currLineStream >> currCallTimes ;

        updateVertex( i, currWeight, currCallTimes );

        currLineStream.clear();
    }
}

Profile_Vertex* Profile_Graph::vertexGenerator ( unsigned int id, const std::string& name, int weight ) {
    return new Profile_Vertex(id, name, weight);
}
