#include "./include/HarchTreeTraverser.h"
#include "./include/MetisGraph.h"

#include <algorithm>
#include <vector>
#include <map>
#include <set>
#include <sstream>
#include <cassert>

HarchTreeTraverser::GlobalOrdering::GlobalOrdering( const std::vector<VertexBase*>& currExecutableList ) 
    : order(currExecutableList.size()), idToOrderAllocation( new std::map<int, int> ) { 
        idToOrderP = idToOrderAllocation;
        fillOrderMap ( currExecutableList );
    }

HarchTreeTraverser::GlobalOrdering::GlobalOrdering ( const HarchTreeTraverser::GlobalOrdering& rhs ) 
    : idToOrderAllocation(NULL), idToOrderP(rhs.idToOrderP) { 
    }

HarchTreeTraverser::GlobalOrdering::~GlobalOrdering () {
    if ( idToOrderAllocation )
        delete idToOrderAllocation;
}

bool HarchTreeTraverser::GlobalOrdering::operator() ( VertexBase* lhs, VertexBase* rhs ) const {
    std::map<int,int>& idToOrder = *idToOrderP;
    return idToOrder[lhs->getID()] < idToOrder[rhs->getID()] ;
}

void HarchTreeTraverser::GlobalOrdering::visit ( const VertexBase* currRoot ) {
    std::map<int, int>::const_iterator found = idToOrderP->find(currRoot->getID()); 
    if ( found == idToOrderP->end() ) {
        std::map< VertexBase*, int >::const_iterator nIt , nEnd = currRoot->getNeighboursEnd(); 
        for ( nIt = currRoot->getNeighboursBegin(); nIt != nEnd; ++nIt ) {
            if ( nIt-> second >= 0) {
                visit(nIt->first);
            }
        }
        (*idToOrderP)[currRoot->getID()] = --order;
    }
}

void HarchTreeTraverser::GlobalOrdering::fillOrderMap ( const std::vector<VertexBase*>& currExecutableList ) {
    std::set<VertexBase*> roots;

    std::vector<VertexBase*>::const_iterator vIt, vEnd = currExecutableList.end();
    for ( vIt = currExecutableList.begin(); vIt != vEnd; ++vIt ) {
        roots.insert(*vIt);
    }

    std::set<VertexBase*>::const_iterator setIt, setEnd = roots.end();
    for ( setIt = roots.begin(); setIt != setEnd; ++setIt ) {
        visit(*setIt);
    }
}


HarchTreeTraverser::HarchTreeTraverser( MetisGraph* givenGraph ) : graph(givenGraph) {
}

HarchTreeTraverser::~HarchTreeTraverser() {
    delete order;
}

void HarchTreeTraverser::traverse() {
    allocateMemory ();
    setOrdering();
    traversePartitionGraph();
    buildVertexOrderingMap ();
}


void HarchTreeTraverser::printAppendedGraph (std::ifstream& in, std::ostream& out ) {
    std::string lineBuffer;
    std::getline(in, lineBuffer);


    std::stringstream lineStream(lineBuffer);
    out << lineBuffer << std::endl;

    int m_nNodes = graph->getNumVertices();
    for ( int i = 0; i < m_nNodes; ++i ){
        std::getline(in, lineBuffer);
        out << lineBuffer << " order=";
        std::map<std::string, int>& curr = verticesPartitionOrderMap[graph->at(i)];
        std::map<std::string, int>::const_iterator it = curr.begin(), end = curr.end();
        if ( it != end ) {
            out<< it->second;
            ++it;
        }
        for (;  it != end; ++it) {
            out<<":"<< it->second;
        }
        out<<"; "<<std::endl;
        std::getline(in, lineBuffer);
        out << lineBuffer << std::endl;
    }
}

void HarchTreeTraverser::allocateMemory ( ) {
    std::vector<VertexBase*>::const_iterator vIt, vEnd = graph->end();
    for ( vIt = graph->begin(); vIt != vEnd; ++vIt ) {
        VertexBase* currVertex = *vIt;
        std::vector<int>::const_iterator partIt = currVertex->getPartitionBegin(), partEnd = currVertex->getPartitionEnd();
        std::string currPartitionStr(":");

        std::stringstream itoaStream;
        while ( partIt != partEnd ) {
            childrenCountMap[currPartitionStr] = std::max( 1 + *partIt, childrenCountMap[currPartitionStr]);
            executableListMap[currPartitionStr].push_back(currVertex);

            itoaStream << *partIt;
            currPartitionStr.append(itoaStream.str()).append(":");
            itoaStream.str("");
            itoaStream.clear();

            ++partIt;
        }
        childrenCountMap[currPartitionStr]; //TODO
        executableListMap[currPartitionStr].push_back(currVertex);
    }
}

void HarchTreeTraverser::setOrdering() {
    order = new GlobalOrdering (executableListMap[":"]); 
}

void HarchTreeTraverser::verify( ) {

    std::vector<VertexBase*>::const_iterator vIt, vEnd = graph->end();
    for ( vIt = graph->begin(); vIt != vEnd; ++vIt ) {
        VertexBase* currVertex = *vIt;
        std::map< VertexBase*, int>::const_iterator nIt , nEnd = currVertex->getNeighboursEnd(); 
        for ( nIt = currVertex->getNeighboursBegin(); nIt != nEnd; ++nIt ) {
            if ( nIt-> second >= 0 ) {
                std::map<int,int>& orderMap = order->getOrderMap();
                assert ( orderMap[currVertex->getID()] < orderMap[nIt->first->getID()] );
            }
        }
    }
}

void HarchTreeTraverser::traversePartitionGraph( std::string currPartitionStr ) {
    std::vector<VertexBase*>& currExecutableList = executableListMap[currPartitionStr];

    std::sort(currExecutableList.begin(), currExecutableList.end(), *order);

    std::map<std::string, int >::iterator found = childrenCountMap.find( currPartitionStr );
    if ( found != childrenCountMap.end() ) {
        int currNumChildren = found->second;

        std::stringstream itoaStream;
        for ( int i = 0 ; i < currNumChildren; ++i ) {
            itoaStream << i;
            std::string childString = currPartitionStr;
            childString.append(itoaStream.str()).append(":");
            itoaStream.str("");
            itoaStream.clear();

            traversePartitionGraph(childString);
        }
    }
}

void HarchTreeTraverser::buildVertexOrderingMap (  ) {
    std::map<std::string, std::vector<VertexBase*> >::const_iterator sortedIt = executableListMap.begin(), sortedEnd = executableListMap.end();

    for ( ; sortedIt != sortedEnd; ++sortedIt ){
        const std::string& currOrderingStr = sortedIt -> first;
        const std::vector<VertexBase*>& currOrdering = sortedIt -> second;
        std::vector<VertexBase*>::const_iterator orderIt = currOrdering.begin(), orderEnd = currOrdering.end(); 
        for (unsigned int i = 0; orderIt != orderEnd; ++orderIt ) {
            verticesPartitionOrderMap[*orderIt][currOrderingStr] = i++;
        }
    }
}
