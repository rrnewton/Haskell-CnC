#ifndef PROFILE_GRAPH_H
#define PROFILE_GRAPH_H

#include <istream>
#include "Profile_Vertex.h"
#include "GraphBase.h"

class Profile_Graph : public GraphBase {
    public:

        Profile_Graph ( unsigned int numVertices );

        static Profile_Graph* create ( std::istream& in ) ;

        void parseVertices ( std::istream& inFile, bool enumerate = false ) ;

        void parseEdges ( std::istream& inFile ) ;

        void parseExecutionTimes ( std::istream& inFile ) ;

        void updateVertex(unsigned int id, double weight, unsigned int numCalls );

    private:

        Profile_Vertex* vertexGenerator ( unsigned int id, const std::string& name, int weight );

        static const char digits[];

        static void replace ( std::string& name, char from, char to);
        
};



#endif
