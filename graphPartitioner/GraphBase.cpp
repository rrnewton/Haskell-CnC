#include <cassert>
#include "./include/GraphBase.h"

GraphBase::GraphBase ( unsigned int numVertices) 
    : m_numVertices(numVertices), m_numEdges(0), lastInsertedPosition(0), m_vertexTable(numVertices) {
}

GraphBase::~GraphBase () {
    VertexVectorIt_t it = m_vertexTable.begin(), end = m_vertexTable.end();
    for ( ; it != end; ++it ) {
        delete *it;
    }
}
void GraphBase::pushVertex ( unsigned int id, const std::string& name, int weight ){
    m_vertexTable[lastInsertedPosition++] = vertexGenerator ( id, name, weight );
    cumulativeWeight += weight;
}

void GraphBase::pushDirectedEdge ( unsigned int from, unsigned int to, int edgeWeight ) {
    if (to > m_vertexTable.size() || from > m_vertexTable.size()){
        std::cerr << "to "<<to<<" from "<<from<<" size "<<m_vertexTable.size()<<std::endl;
    }
    assert(to <= m_vertexTable.size() && from <= m_vertexTable.size());
    assert(to <= lastInsertedPosition && from <= lastInsertedPosition );
    m_vertexTable[from]->pushNeighbour(m_vertexTable[to], true, edgeWeight );
    m_vertexTable[to]->pushNeighbour(m_vertexTable[from], false, edgeWeight );
     ++m_numEdges;
}

void GraphBase::pushUndirectedEdge ( unsigned int from, unsigned int to, int edgeWeight ) {
    assert(to <= m_vertexTable.size() && from <= m_vertexTable.size());
    assert(to <= lastInsertedPosition && from <= lastInsertedPosition );
    bool firstSucceed = m_vertexTable[from]->pushNeighbour(m_vertexTable[to], edgeWeight);
    if ( firstSucceed ) ++m_numEdges;
}

void GraphBase::print( bool verbose , std::ostream& out ) const {
    out << m_numVertices << " " << m_numEdges << " 11"<<std::endl;
    VertexVectorIt_t it, end = m_vertexTable.end();
    for ( it = m_vertexTable.begin(); it != end; ++it ) {
        (*it)->print(verbose, out);
    }
}

VertexBase* GraphBase::vertexGenerator( unsigned int id, const std::string& name, int weight ) {
    return new VertexBase(id,name,weight);
}; 

