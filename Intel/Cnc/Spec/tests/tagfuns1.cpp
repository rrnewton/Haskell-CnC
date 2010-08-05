// This is a simple harness to compile/test the code generated from tagfuns1.cnc

#include <tr1/tuple>

// #include "boost/tuple/tuple.hpp" // Included BEFORE cnc.h
#include <cnc/cnc.h>
#include <stdio.h>

typedef char Foo;
typedef char Bar;

#include "tagfuns1.h"

namespace FOO {
    //template<class T>
    //using Tup = std::tr1::tuple<T, int>; 
   //using Vec = std::vector<T,My_alloc<T>>; 

}

template < class ctxt > 
int S::execute( const cnctup::tuple< int, int > & tag, ctxt & c) const {
    //printf("EXECUTING STEP %d %d\n", tag.get<0>(), tag.get<1>());
    printf("EXECUTING STEP %d %d\n", cnctup::get<0>(tag), cnctup::get<1>(tag));
        c.I1.put(33,44);
        // c.T1.put(55);
        return CnC::CNC_Success;
}

int main () {
    printf("Main starting\n");

    //std::tr1::tuple<int, int> mytup;
    cnctup::tuple<int, int> mytup;
    
    tagfuns1_context ctxt;

    cnctup::tuple<int, int> mytup2 = cnctup::make_tuple(9,99);

    ctxt.T1.put(mytup2);

    ctxt.wait();
    
    printf("Returned from context wait\n");


// std::vector<int> someList;
// int total = 0;
// std::for_each(
//   someList.begin(), 
//   someList.end(), 
//   [&total](int x) { total += x; }
// );
// std::cout << total;

//     printf("\nUSED LAMBDA\n");
}
