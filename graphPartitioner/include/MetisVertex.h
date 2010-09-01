#ifndef METISVERTEX_H
#define METISVERTEX_H

#include "VertexBase.h"

class MetisVertex : public VertexBase {
	public:
        MetisVertex ( unsigned int id, const std::string& name="", int weight = 0);
        MetisVertex ( const MetisVertex& rhs );

		inline unsigned int getNumNeighbours() const{ return m_neighbours.size(); }
		inline unsigned int getIDinPartition() const{ return m_idForPartition; }
		inline int getPartition() const{ return m_subGraph; }

		bool belongsTo ( int subGraphID ) const;
		void assignTo( int subGraphID , int idInSubGraph);

	private:
		unsigned int m_idForPartition, m_subGraph;

		bool notBelongsTo ( int subGraphID ) const;
};

#endif

