#ifndef METISGRAPH_H
#define METISGRAPH_H

#include <iostream>
#include <utility>
#include <vector>
#include <map>

#include "GraphBase.h"
#include "MetisVertex.h"

class MetisGraph: public GraphBase {
    public:
        MetisGraph( unsigned int numVertices );
        ~MetisGraph();

        static MetisGraph* create( std::istream& in );

        void applyFilter ( int numSubGraphs, std::istream& in );
        inline GraphBase const * getPartition(unsigned int index) { return m_partitions[index]; }


    private:
        double totalWeight;
        int m_numPartitions;

        GraphBase** m_partitions;
        int* counters;

        void parseVerticesAndEdges ( std::istream& in );
        void assignVertexToPartition( unsigned int vertexID, unsigned int subGraphID);

        void setNumPartitions( int numSubGraphs );
        void allocatePartitions();

        void handOutFilteredVertices();

        template <typename T1, typename T2, typename T3>
        class Triple {
            public:
                T1 first;
                T2 second;
                T3 third;
                Triple( ) : first(), second(), third() {
                }
                Triple( const T1& t1, const T2& t2, const T3& t3) : first(t1), second(t2), third(t3) {
                }
        };
        void applyBufferedEdgePuts(std::vector < Triple < unsigned int, unsigned int, int > >& bufferedEdgePuts);

        MetisVertex* vertexGenerator ( unsigned int id, const std::string& name, int weight );

        MetisGraph(const MetisGraph& rhs);

};

#endif
