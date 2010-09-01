#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <map>

#define DEBUG(x) 

int main ( int argc, char* argv[] ) {
    std::ifstream in(argv[1]);
    std::string lineBuffer;
    std::stringstream lineStream;
    std::map<std::string, unsigned int> enumerationMap;
    std::vector< std::vector<std::string>* > dependsOnVector;
    std::vector< std::vector<std::string>* > dependentsVector;
    unsigned int nodeIndex = 0;
    bool verbose = true;
    while ( std::getline(in, lineBuffer) ) {
        lineStream.str(lineBuffer);

        std::string lineSubset;
        { // depends
            DEBUG(std::cout<<"depends on: "<<std::endl;)
            std::vector<std::string>* dependsOn = new std::vector<std::string>;
            std::getline(lineStream,lineSubset,'>');
            size_t searchIndex = 0;
            size_t parenIndex = lineSubset.find_first_of('(',searchIndex );
            while( parenIndex != std::string::npos ) {
                searchIndex = lineSubset.find_first_of(')', parenIndex+1);
                std::string currDependsOn = lineSubset.substr(parenIndex+1,searchIndex-parenIndex-1);
                DEBUG(std::cout<<currDependsOn <<std::endl;)
                dependsOn->push_back(currDependsOn);
                parenIndex = lineSubset.find_first_of('(',searchIndex + 1);
            }
            dependsOnVector.push_back(dependsOn);

        }{ // self
            DEBUG(std::cout<<"self: "<<std::endl;)
            std::getline(lineStream,lineSubset,'>');
            size_t parenIndex = lineSubset.find_first_of('(',0);
            size_t searchIndex = lineSubset.find_first_of(')', parenIndex+1);
            std::string self = lineSubset.substr(parenIndex+1,searchIndex-parenIndex-1);
            DEBUG(std::cout<<self<<std::endl;)
            enumerationMap[self] = ++nodeIndex;
        }{ // dependents
            DEBUG(std::cout<<"dependents: "<<std::endl;)
            std::vector<std::string>* dependents = new std::vector<std::string>;
            std::getline(lineStream,lineSubset);
            size_t searchIndex = 0;
            size_t parenIndex = lineSubset.find_first_of('(',searchIndex );
            while( parenIndex != std::string::npos ) {
                searchIndex = lineSubset.find_first_of(')', parenIndex+1);
                std::string currDependent = lineSubset.substr(parenIndex+1,searchIndex-parenIndex-1);
                DEBUG(std::cout<<currDependent<<std::endl;)
                dependents->push_back(currDependent);
                parenIndex = lineSubset.find_first_of('(',searchIndex + 1);
            }
            dependentsVector.push_back(dependents);
        }
        lineStream.clear();
        DEBUG(std::cout<<std::endl;)
        DEBUG(std::cout<<std::endl;)
        DEBUG(std::cout<<std::endl;)
    }

    std::ostream& out = std::cout;
    out << enumerationMap.size() << " ";

    int numEdges = 0;
    for ( int i = 0 ; i != dependsOnVector.size(); ++i ) {
        numEdges += dependsOnVector[i]->size();
    }
    out << numEdges << " 10" << std::endl;

    int defaultWeight = 1;
    std::map<std::string, unsigned int>::const_iterator it, end = enumerationMap.end();
    for ( it = enumerationMap.begin(); it != end; ++it ) {
        const std::string& currString = it -> first;
        const unsigned int& currIndex = it -> second;
        if ( verbose ) {
            out << "% name=" << it->first << "; directions=";
            out << std::string(dependsOnVector[currIndex-1]->size(),'0');
            out << std::string(dependentsVector[currIndex-1]->size(),'1') << ";"<< std::endl;
            out << defaultWeight << " ";
            std::vector<std::string>* currDependsOnV = dependsOnVector[currIndex-1];
            std::vector<std::string>::const_iterator vIt, vEnd = currDependsOnV->end();
            for ( vIt = currDependsOnV->begin(); vIt != vEnd; ++vIt ) {
                out << enumerationMap[*vIt] << " ";
            }
            std::vector<std::string>* currDependentsV = dependentsVector[currIndex-1];
            vEnd = currDependentsV->end();
            for ( vIt = currDependentsV->begin(); vIt != vEnd; ++vIt ) {
                out << enumerationMap[*vIt] << " ";
            }
        }
        out<<std::endl;
    }
}
