#include <functional>
#include <algorithm>

#include "./include/Profile_Vertex.h"

double Profile_Vertex::threshold = 1e-5; 
double Profile_Vertex::factor = 1e5;

Profile_Vertex::Profile_Vertex ( unsigned int id, const std::string& name, double weight )
    : VertexBase ( id, name, Profile_Vertex::integerify(weight) ) {
    }

Profile_Vertex::Profile_Vertex ( const Profile_Vertex& rhs ) 
    : VertexBase ( rhs ) {
    }
