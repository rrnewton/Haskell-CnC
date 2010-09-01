#include "./include/MetisVertex.h"

MetisVertex::MetisVertex(unsigned int id, const std::string& name, int weight )
    : VertexBase ( id, name, weight ) , m_subGraph(0), m_idForPartition(0) {
    }

MetisVertex::MetisVertex(const MetisVertex& rhs )
	: VertexBase ( rhs ), m_idForPartition ( rhs.m_idForPartition ), m_subGraph ( rhs.m_subGraph ) {
	}

void MetisVertex::assignTo( int subGraphID , int idInSubGraph ) {
	m_subGraph = subGraphID;
	m_idForPartition = idInSubGraph;
}

bool MetisVertex::belongsTo ( int subGraphID ) const {
	return subGraphID == m_subGraph;
}

bool MetisVertex::notBelongsTo ( int subGraphID ) const {
	return subGraphID != m_subGraph;
}

