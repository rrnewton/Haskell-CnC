// This is a simple harness to compile/test the code generated from tagfuns1.cnc

#include "boost/tuple/tuple.hpp" // Included BEFORE cnc.h
#include <cnc/cnc.h>
#include <stdio.h>

#include <tr1/tuple>

typedef char Foo;
typedef char Bar;

#include "tagfuns1.h"

template < class ctxt > 
int S::execute( const boost::tuple< int, int > & tag, ctxt & c) const {
        printf("EXECUTING\n");
        c.I1.put(33,44);
        // c.T1.put(55);
}

int main () {
    printf("Main starting\n");

    std::tr1::tuple<int, int> mytup;
    
    tagfuns1_context ctxt;
    ctxt.wait();

    printf("Returned from context wait\n");
}
