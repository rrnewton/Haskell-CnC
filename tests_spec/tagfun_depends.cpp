
// This Test illustrates generation of a depends function.

#include<tagfun_depends.h>

template < class ctxt > 
int S::execute( const int & tag, ctxt & c) const {
    printf("Step(%d): Begun.\n", tag);
    double v;
    c.In.get(tag-1, v);
    c.In.get(tag, v);
    c.In.get(tag+1, v);
    printf("  Step(%d), gets of In succeeded\n", tag, tag-1);
    c.Out.put(tag, v);
    return CnC::CNC_Success;    
}

int main () {
    printf ("Running with tagfun checking.\n");
    tagfun_depends_context context;

    // Put some tags into the collection in a naive way:
    for(int i=1; i<10; i++) {
      context.T.put(i);
      context.In.put(i, 33);
    }
    context.In.put(0, 44);
    context.In.put(10, 44);

    context.wait();

    double fl;
    context.Out.get(3, fl);

    printf("Retrieve a single item, index 3: %lf\n", fl);
    return 0;
}
