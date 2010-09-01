#ifndef GRAPHBASE_H
#define GRAPHBASE_H

#include "VertexBase.h"
#include <vector>

class GraphBase {
    public:

        typedef std::vector<VertexBase*> VertexVector_t;
        typedef VertexVector_t::const_iterator VertexVectorIt_t;

        GraphBase ( unsigned int numVertices);

        virtual ~GraphBase ();

		void pushVertex ( unsigned int id, const std::string& name, int weight = 0);

        void pushUndirectedEdge ( unsigned int from, unsigned int to, int edgeWeight = 1);

        void pushDirectedEdge ( unsigned int from, unsigned int to, int edgeWeight = 1);

		void print( bool verbose = true, std::ostream& out = std::cout) const;

        inline VertexBase* at(int i) const { return m_vertexTable.at(i); }

        inline VertexVectorIt_t begin() const { return m_vertexTable.begin(); }

        inline VertexVectorIt_t end() const { return m_vertexTable.end(); }

        inline bool empty() const { return begin() == end(); } 

        inline size_t getNumVertices() const { return m_vertexTable.size(); }

        inline int getWeight() const { return cumulativeWeight; }

    protected:

        unsigned int m_numVertices, m_numEdges, lastInsertedPosition;

        VertexVector_t m_vertexTable;

        int cumulativeWeight;

        virtual VertexBase* vertexGenerator( unsigned int id, const std::string& name, int weight ); 
        
    private:
		GraphBase(const GraphBase& rhs);

};

#endif
