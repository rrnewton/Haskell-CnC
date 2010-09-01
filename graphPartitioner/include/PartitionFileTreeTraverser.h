#ifndef PARTITIONFILETREETRAVERSER_H
#define PARTITIONFILETREETRAVERSER_H

#include <iostream>
#include <fstream>
#include <vector>

class PartitionFileTreeTraverser {
    public:
        PartitionFileTreeTraverser (const std::string& baseName, int branchingFactor, int nNodes );
        ~PartitionFileTreeTraverser () ;

        void traverse ( int globalIndex, int currDepth ) ;

        void printAppendedGraph ( std::ifstream& in, std::ostream& out = std::cout ) const ;

    private:
        std::vector<int>* m_partitionIndexes;
        std::string m_baseName;
        int m_branchingFactor;
        int m_nNodes;
};

#endif
