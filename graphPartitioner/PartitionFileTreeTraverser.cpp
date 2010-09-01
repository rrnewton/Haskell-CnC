#include "./include/PartitionFileTreeTraverser.h"

#include <iostream>
#include <fstream>
#include <sstream>
#include <map>
#include <algorithm>

PartitionFileTreeTraverser::PartitionFileTreeTraverser (const std::string& baseName, int branchingFactor, int nNodes ) 
    : m_baseName(baseName), m_nNodes(nNodes), m_branchingFactor(branchingFactor), m_partitionIndexes(new std::vector<int>[nNodes]) {
    }

PartitionFileTreeTraverser::~PartitionFileTreeTraverser () {
    delete [] m_partitionIndexes;
}

void PartitionFileTreeTraverser::traverse ( int globalIndex, int currDepth ) { 
    std::stringstream fileNameStream;
    fileNameStream << m_baseName << ".imported." << globalIndex << ".part." << m_branchingFactor;
    std::ifstream in(fileNameStream.str().c_str());

    std::string lineBuffer;
    if ( !in.fail() ) {
        bool uniqueSubGraphID[m_branchingFactor];
        std::fill(uniqueSubGraphID, uniqueSubGraphID+m_branchingFactor, false);

        while ( std::getline(in, lineBuffer) ) {
            uniqueSubGraphID[atoi(lineBuffer.c_str())] = true;
        }
        in.clear();
        in.seekg(std::ios_base::beg); // return to the beginning of file

        std::map<int,int> zeroIndexedMapping;
        for ( int lastUniqueID = 0, i = 0; i < m_branchingFactor; ++i ) {
            if ( uniqueSubGraphID[i] ) {
                zeroIndexedMapping[i]= lastUniqueID++; 
            }
        }
        if ( currDepth == 1 ) {
            for ( int i = 0; i < m_nNodes; ++i ){
                std::getline(in, lineBuffer);
                int read = atoi(lineBuffer.c_str());
                m_partitionIndexes[i].push_back(zeroIndexedMapping[read]);
                m_partitionIndexes[i].push_back(zeroIndexedMapping[read]+1); //omitted 0 * branchingFactor
            }
        } else {
            int lastFound[m_branchingFactor];
            std::fill(lastFound, lastFound+m_branchingFactor, -1);
            while ( std::getline (in, lineBuffer) ) {
                int zeroIndexedRead = zeroIndexedMapping[atoi(lineBuffer.c_str())];
                int i = lastFound[zeroIndexedRead]+1;
                for (; m_partitionIndexes[i].back() != globalIndex; ++i) {
                }
                m_partitionIndexes[i].back() = zeroIndexedRead;
                m_partitionIndexes[i].push_back(globalIndex*m_branchingFactor+zeroIndexedRead+1);
                lastFound[zeroIndexedRead] = i;
            }
        }
        in.close();
        for (int i = 0; i < m_branchingFactor; ++i){
            if ( uniqueSubGraphID[i] ) {
                traverse(globalIndex*m_branchingFactor+zeroIndexedMapping[i]+1, currDepth+1);
            }
        }
    } 
}

void PartitionFileTreeTraverser::printAppendedGraph ( std::ifstream& in, std::ostream& out ) const {
    std::string lineBuffer;
    std::getline(in, lineBuffer);
    out << lineBuffer << std::endl;

    for ( int i = 0; i < m_nNodes; ++i ){
        std::getline(in, lineBuffer);
        out << lineBuffer << " partitions=";
        std::vector<int>& curr = m_partitionIndexes[i];
        std::vector<int>::const_iterator it = curr.begin(), end = curr.end();
        --end;
        if ( it != end ) {
            out<<*it++;
        }
        for (;  it != end; ++it) {
            out<<":"<<*it;
        }
        out<<"; "<<std::endl;
        std::getline(in, lineBuffer);
        out << lineBuffer << std::endl;
    }
}
