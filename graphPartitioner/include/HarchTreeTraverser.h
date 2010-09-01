#ifndef HARCHTREETRAVERSER_H
#define HARCHTREETRAVERSER_H

#include "MetisGraph.h"
#include "VertexBase.h"

#include <iostream>
#include <fstream>
#include <vector>
#include <map>

class HarchTreeTraverser {
    private:
        class GlobalOrdering {
            public:
                GlobalOrdering( const std::vector<VertexBase*>& currExecutableList );
                GlobalOrdering ( const GlobalOrdering& rhs ) ;
                ~GlobalOrdering ( );

                bool operator() ( VertexBase* lhs, VertexBase* rhs ) const ;

                inline std::map<int,int>& getOrderMap () { return *idToOrderP; }

            private:
                std::map<int, int>* idToOrderP;
                std::map<int, int>* idToOrderAllocation;
                size_t order;

                void visit ( const VertexBase* currRoot ) ;
                void fillOrderMap ( const std::vector<VertexBase*>& currExecutableList ) ;
        };

    public:
        HarchTreeTraverser( MetisGraph* givenGraph );
        ~HarchTreeTraverser() ;

        void traverse();
        void printAppendedGraph (std::ifstream& in, std::ostream& out = std::cout );

    private:
        MetisGraph* graph;
        GlobalOrdering* order;
        std::map<std::string, int> childrenCountMap;
        std::map<std::string, std::vector<VertexBase*> > executableListMap;
        std::map<VertexBase*, std::map<std::string, int> > verticesPartitionOrderMap;

        void allocateMemory ( );
        void setOrdering ( ) ;
        void verify( ) ;
        void traversePartitionGraph( std::string currPartitionStr = ":" ) ;
        void buildVertexOrderingMap (  ) ;
};

#endif
