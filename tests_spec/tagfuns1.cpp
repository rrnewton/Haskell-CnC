// This is a simple harness to compile/test the code generated from tagfuns1.cnc

#include <tr1/tuple>

// #include "boost/tuple/tuple.hpp" // Included BEFORE cnc.h
#include <cnc/cnc.h>
#include <stdio.h>

// A dummy class definition until our C++ API is updated.
class tagfuns1_context;
namespace CnC {
  template<class T>
  class step_collection {
  public:
      step_collection(tagfuns1_context* ctxt) {
      }
  };
}
//----------------------------------------------------------------------------------------------------

typedef char Foo;
typedef char Bar;

// Next, we define the step type before including the generated header.
struct S
{
    template < class ctxt > 
    int execute( const cnctup::tuple< int, int > & tag, ctxt & c) const {
        printf("EXECUTING STEP %d %d\n", cnctup::get<0>(tag), cnctup::get<1>(tag));
        c.I1.put(33,44);
        // c.T1.put(55);
        return CnC::CNC_Success;
    }
};

#include "tagfuns1.h"

int main () {
    printf("Main starting\n");

    //std::tr1::tuple<int, int> mytup;
    cnctup::tuple<int, int> mytup;
    
    tagfuns1_context ctxt;

    cnctup::tuple<int, int> mytup2 = cnctup::make_tuple(9,99);

    ctxt.T1.put(mytup2);

    ctxt.wait();
    
    printf("Returned from context wait\n");
}
