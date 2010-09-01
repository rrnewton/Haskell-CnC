#ifndef VERTEXBASE_H
#define VERTEXBASE_H

#include <iostream>
#include <string>
#include <utility>
#include <map>
#include <vector>

#include <cstdlib>

class VertexBase;
class VertexBase{
    public:
        VertexBase ( unsigned int id, const std::string& name="", int weight = 0);
        VertexBase ( const VertexBase& rhs );
        virtual ~VertexBase();
        virtual void print ( bool verbose = false, std::ostream& out = std::cout );
        bool pushNeighbour ( VertexBase * neighbour, int edgeWeight = 1 );
        bool pushNeighbour ( VertexBase * neighbour, bool isNotReverseEdge, int edgeWeight = 1 );

		inline std::map< VertexBase*, int>::const_iterator getNeighboursBegin() const { return m_neighbours.begin();}
		inline std::map< VertexBase*, int>::const_iterator getNeighboursEnd() const { return m_neighbours.end();}
        bool isDependentOf ( VertexBase* rhs ) const;
        bool hasDependent ( VertexBase* rhs ) const;

	inline int getPartitionAt(int i) const { return m_partitions[i]; }
	inline std::vector< int >::const_iterator getPartitionBegin() const { return m_partitions.begin();}
	inline std::vector< int >::const_iterator getPartitionEnd() const { return m_partitions.end();}

        inline unsigned int getID ( ) const { return m_id; }
		inline const std::string& getName() const { return m_name; }
        inline int getWeight ( ) const { return m_weight; }
        inline void setWeight ( int weight ) { m_weight = weight ; }

    protected:
        unsigned int m_id;
        std::string m_name;
        int m_weight;
        std::map< VertexBase*, int > m_neighbours;
        std::vector< bool > m_neighbourDirectionsBuffer; //known before neighbours existence, need to buffer 

        std::vector< int > m_partitions;

        static bool nameAttributed ( const std::string& name ) ;
        static bool directionsAttributed ( const std::string& name ) ;
        static bool partitionsAttributed ( const std::string& name ) ;
        static std::string attributify( const std::string& name );
        static void deattributify( std::string& name );
        static std::vector<bool> extractDirections( const std::string& name ) ;
        static std::vector<int> extractPartitions( const std::string& name ) ;

        static std::string alphanumeric;
};


#endif
