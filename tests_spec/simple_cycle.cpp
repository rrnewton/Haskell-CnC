

#include<simple_cycle.h>

template < class ctxt > 
int S::execute( const int & tag, ctxt & c) const {
    printf("Step exec with tag %d\n", tag);

    c.I.put(tag + 1, (tag * 3.33)); 
    if (tag < 10) c.T.put(tag + 1); 

    return CnC::CNC_Success;    
}

int main () {
    simple_cycle_context context;

    context.T.put(0);
    context.wait();

    double fl;
    context.I.get(3, fl);
    printf("Retrieve a single item, index 3: %lf\n", fl);
    return 0;
}
