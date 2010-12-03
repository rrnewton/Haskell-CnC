

#include<tagfun_depends.h>

template < class ctxt > 
int S::execute( const int & tag, ctxt & c) const {
    double v;
    c.In.get(tag-1, v);
    printf("Step exec with tag %d\n", tag);
    c.Out.put(tag, v);
    return CnC::CNC_Success;    
}

int main () {
    printf ("Running with tagfun checking.\n");
    tagfun_depends_context context;

    // Put some tags into the collection in a naive way:
    context.T.put(0);
    for(int i=1; i<10; i++) {
      context.T.put(i);
      context.In.put(i, 33);
    }

    context.wait();

    double fl;
    context.Out.get(3, fl);

    printf("Retrieve a single item, index 3: %lf\n", fl);
    return 0;
}
