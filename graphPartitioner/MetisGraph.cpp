#include <algorithm>
#include <cassert>
#include <sstream>
#include <set>

#include "./include/MetisGraph.h"

MetisGraph::MetisGraph( unsigned int numVertices ) 
    : GraphBase(numVertices), totalWeight(0.0L), m_partitions(NULL), counters(NULL), m_numPartitions(-1) {
        MetisVertex* nullP = NULL;
        std::fill( m_vertexTable.begin(), m_vertexTable.end(), nullP );
    }

MetisGraph* MetisGraph::create( std::istream& in ){
    MetisGraph* toBeReturned = NULL;

    try {
        std::string currLine;
        std::getline(in, currLine);
        std::istringstream currLineStream(currLine);
        unsigned int numVertices = 0;
        unsigned int numEdges = 0;
        currLineStream >> numVertices >> numEdges; //do not care, may use for assert
        toBeReturned = new MetisGraph(numVertices);
        toBeReturned->parseVerticesAndEdges(in); 
    } catch ( std::exception& e ) {
        std::cerr << e.what() << std::endl;
    }

    return toBeReturned;
}

MetisGraph::~MetisGraph(){
    for(int i = 0; i < m_numPartitions; ++i) {
        delete m_partitions[i];
    }
    if(m_partitions) delete[] m_partitions;
    if(counters) delete[] counters;
}

void MetisGraph::assignVertexToPartition( unsigned int vertexID, unsigned int subGraphID) {
    dynamic_cast<MetisVertex*>(m_vertexTable[vertexID])->assignTo(subGraphID, ++counters[subGraphID]); 
}

void MetisGraph::setNumPartitions( int numSubGraphs ) { 
    m_numPartitions = numSubGraphs;
    m_partitions = new GraphBase*[m_numPartitions];
    counters = new int[m_numPartitions];
    std::fill(counters, counters+m_numPartitions,0);
}

void MetisGraph::applyFilter (int numSubGraphs, std::istream& in ) {
    setNumPartitions(numSubGraphs);

    unsigned int subGraphID;
    bool uniqueSubGraphID[m_numPartitions];
    std::fill(uniqueSubGraphID, uniqueSubGraphID+m_numPartitions, false);
    for (unsigned int i = 0; i < m_numVertices; ++i) {
        in >> subGraphID;
        uniqueSubGraphID[subGraphID] = true;
    }

    std::map<int,int> zeroIndexedMapping;
    int lastUniqueID = 0;
    for ( int i = 0; i < m_numPartitions; ++i ) {
        if ( uniqueSubGraphID[i] ) {
            zeroIndexedMapping[i]= lastUniqueID++; 
        }
    }
    
    in.seekg(std::ios_base::beg); // return to the beginning of file
    for (unsigned int i = 0; i < m_numVertices; ++i) {
        in >> subGraphID;
        assignVertexToPartition(i, zeroIndexedMapping[subGraphID]);
    }

    allocatePartitions();
    handOutFilteredVertices();
}

void MetisGraph::parseVerticesAndEdges ( std::istream& in ){
    std::vector< Triple < unsigned int, unsigned int, int> > bufferedEdgePuts;
    std::string currLine;
    std::istringstream currLineStream;
    std::string formerComment;
    for (unsigned int nodeID = 1; nodeID <= m_numVertices; ) { // get every node line
        std::getline(in, currLine);
        size_t firstNonWS = currLine.find_first_not_of("\t\n\r ");
        // if the first character after whitespace is not a comment beginning then parse line
        if( firstNonWS != std::string::npos && currLine[firstNonWS] != '%') {
            unsigned int weight = 0;
            unsigned int neighbourID = 0;
            currLineStream.str(currLine);

            currLineStream >> weight;
            pushVertex(nodeID, formerComment, weight );
            formerComment="";
            std::string lineBuffer;
            std::getline(currLineStream, lineBuffer, ' '); // consume first space
            while(std::getline(currLineStream, lineBuffer, ' ') ) { // not sure if more costly but a .good() check replicated
                neighbourID = std::atoi(lineBuffer.c_str());
                std::getline(currLineStream, lineBuffer, ' ');
                int edgeWeight = std::atoi(lineBuffer.c_str());
                bufferedEdgePuts.push_back( Triple<unsigned int, unsigned int, int>( nodeID, neighbourID, edgeWeight ) );
            }

            currLineStream.clear();
            ++nodeID;
        } else {
            formerComment = currLine.substr(firstNonWS+1);
        }
    }
    MetisGraph::applyBufferedEdgePuts( bufferedEdgePuts );
    bufferedEdgePuts.clear();
}

void MetisGraph::applyBufferedEdgePuts( std::vector< Triple < unsigned int, unsigned int, int> >& bufferedEdgePuts ){
    std::vector< Triple < unsigned int, unsigned int, int> >::const_iterator it, end = bufferedEdgePuts.end();
    for ( it = bufferedEdgePuts.begin(); it != end; ++it ) {
        pushUndirectedEdge(it->first-1, it->second-1, it->third);
    }
}

void MetisGraph::allocatePartitions() {
    for (int i = 0; i < m_numPartitions; ++i ) {
        /// std::cerr << counters[i] << std::endl;
        m_partitions[i] = new GraphBase ( counters[i] );
    }
}

void MetisGraph::handOutFilteredVertices() {
    for (int i = 0; i < m_numVertices; ++i ) {
        MetisVertex const* currVertex = dynamic_cast<MetisVertex*>(m_vertexTable[i]);
        m_partitions[currVertex->getPartition()] -> pushVertex(currVertex->getIDinPartition(), currVertex->getName(), currVertex->getWeight());
    }

    for (int i = 0; i < m_numVertices; ++i ) {
        MetisVertex const* currVertex = dynamic_cast<MetisVertex*>(m_vertexTable[i]);

        int currPartitionNumber = currVertex->getPartition();
        int idForPartition = currVertex->getIDinPartition();
        GraphBase* currPartition = m_partitions[currPartitionNumber];

        std::map< VertexBase*, int >::const_iterator it , end = currVertex->getNeighboursEnd(); 
        for( it = currVertex->getNeighboursBegin(); it != end; ++it){
            MetisVertex* currNeighbour = dynamic_cast<MetisVertex*>(it->first);
            if(currNeighbour->belongsTo(currPartitionNumber)) {
                if ( it->second >=0 ) {  // assert(idForPartition); assert(currNeighbour->getIDinPartition());
                    currPartition->pushDirectedEdge(idForPartition-1, currNeighbour->getIDinPartition()-1, it->second );
                }
            } 
        }
    }
}

MetisVertex* MetisGraph::vertexGenerator ( unsigned int id, const std::string& name, int weight ) {
    return new MetisVertex(id, name, weight);
}
