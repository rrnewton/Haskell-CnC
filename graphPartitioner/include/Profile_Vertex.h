#ifndef PROFILE_VERTEX_H
#define PROFILE_VERTEX_H

#include "VertexBase.h"

class Profile_Vertex : public VertexBase {
	public:

        Profile_Vertex ( unsigned int id, const std::string& name="", double weight = 0.0L);

        Profile_Vertex ( const Profile_Vertex& rhs ) ;

		inline void setNumCalls( unsigned int numCalls ) { m_numCalls = numCalls; }
            
		static inline unsigned int integerify( double weight ) { return ( weight < threshold ) ? 1 : static_cast<int>(factor*weight); }
            
	private:

		unsigned int m_numCalls;

        static double threshold, factor;
};

#endif
