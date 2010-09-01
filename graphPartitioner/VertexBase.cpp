#include "./include/VertexBase.h"

VertexBase::VertexBase ( unsigned int id, const std::string& name, int weight )  
    : m_id ( id ), m_name ( name ), m_weight ( weight ), m_neighbours() { 
        if ( VertexBase::directionsAttributed(m_name) ) {
            m_neighbourDirectionsBuffer = VertexBase::extractDirections( m_name ) ;
        }
        if ( VertexBase::partitionsAttributed(m_name) ) {
            m_partitions = VertexBase::extractPartitions( m_name ) ;
        }
        if ( VertexBase::nameAttributed(m_name) ) {
            VertexBase::deattributify(m_name);
        }
    }

VertexBase::VertexBase ( const VertexBase& rhs )
    : m_id ( rhs.m_id ), m_name ( rhs.m_name ), m_weight ( rhs.m_weight ), m_neighbours( rhs.m_neighbours ) { 
    }
    
VertexBase::~VertexBase() {
}

bool VertexBase::isDependentOf ( VertexBase* rhs ) const {
    std::map< VertexBase *, int >::const_iterator found = m_neighbours.find(rhs);
    return found != m_neighbours.end() && found->second < 0;
}

bool VertexBase::hasDependent ( VertexBase* rhs ) const {
    std::map< VertexBase *, int >::const_iterator found = m_neighbours.find(rhs);
    return found != m_neighbours.end() && found->second >= 0;
}

void VertexBase::print ( bool verbose , std::ostream& out ) {
    std::map< VertexBase *, int >::const_iterator it, end = m_neighbours.end();
    std::vector< int >::const_iterator pIt, pEnd = m_partitions.end();
    if ( verbose ) {
        out << "% HARCHNODE " << VertexBase::attributify(m_name) << "directions=";
        for ( it = m_neighbours.begin() ; it != end; ++it ) {
            out << (it->second >=0 ) ;
        }

        if (!m_partitions.empty()){
            out << "; partitions=";
            pIt = m_partitions.begin();
            out << *pIt++;
                for ( ; pIt != pEnd; ++pIt ) {
                    out << ":" << *pIt;
                }
        }
        out << ";" << std::endl;
    }
    out << m_weight;

    for ( it = m_neighbours.begin() ; it != end; ++it ) {
        out << " " << it->first->getID();
        int edgeWeight = it->second;
        out << " " << ( (edgeWeight >=0 ) ? edgeWeight : 0 - edgeWeight) ;
    }

    out << std::endl;
}

bool VertexBase::pushNeighbour ( VertexBase * neighbour, int edgeWeight ) {
    int directedEdgeWeight = m_neighbourDirectionsBuffer[m_neighbours.size()] ? edgeWeight : 0-edgeWeight ;
    return m_neighbours.insert(std::make_pair(neighbour,directedEdgeWeight)).first->second;
}

bool VertexBase::pushNeighbour ( VertexBase * neighbour, bool isNotReverseEdge, int edgeWeight ) {
    m_neighbours.insert( std::make_pair(neighbour, isNotReverseEdge ? edgeWeight : 0 - edgeWeight ) );
    return true;
}

std::string VertexBase::attributify( const std::string& name ) {
    std::string toReturn(name);
    size_t found = toReturn.find_first_not_of(alphanumeric);
    while ( found != std::string::npos ){
        toReturn[found] = '_';
        found = toReturn.find_first_not_of(alphanumeric, found+1 );
    }
    toReturn.insert(0, "name=");
    toReturn.insert(toReturn.length(), "; ");
    return toReturn;
}

void VertexBase::deattributify( std::string& name ) {
    size_t begin = name.find("name=")+5;
    size_t end = name.find(";",begin);
    name = name.substr(begin,end-begin);
}

std::vector<bool> VertexBase::extractDirections( const std::string& name ) {
    size_t begin = name.find("directions=")+11;
    size_t end = name.find(";",begin);
    std::string directions = name.substr(begin,end-begin);
    std::vector<bool> toBeReturned;
    size_t curr = 0, lastChar = directions.length();
    while ( curr != lastChar ) {
        toBeReturned.push_back(directions[curr] == '1');
        ++curr;
    }
    return toBeReturned;
}

std::vector<int> VertexBase::extractPartitions( const std::string& name ) {
    size_t begin = name.find("partitions=")+11;
    size_t end = name.find(";",begin);
    std::string directions = name.substr(begin,end-begin);
    std::vector<int> toBeReturned;
    size_t intBegin = 0, intEnd = directions.find(":");
    while ( intEnd != std::string::npos ) {
        int pushed = (atoi(directions.substr(intBegin, intEnd-intBegin).c_str()));
        toBeReturned.push_back(pushed);
        intBegin = intEnd + 1;
        intEnd = directions.find(":", intBegin);
    }
    int pushed = (atoi(directions.substr(intBegin).c_str()));
    toBeReturned.push_back(pushed);
    return toBeReturned;
}

bool VertexBase::nameAttributed( const std::string& name ) {
    return name.find("name=") != std::string::npos;
}

bool VertexBase::directionsAttributed ( const std::string& name ) {
    return name.find("directions=") != std::string::npos;
}

bool VertexBase::partitionsAttributed ( const std::string& name ) {
    return name.find("partitions=") != std::string::npos;
}

std::string VertexBase::alphanumeric="ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_0123456789";
