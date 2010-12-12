

int plus(int x, int y) { return x + y; }

#include<reduction_test.h>

template < class ctxt > 
int S1::execute( const int & tag, ctxt & c) const 
{
    printf("Step(%d) begun\n", tag);
    c.R1.put(0, tag);
    printf(" Step(%d) reducer put finished\n", tag);
    return CnC::CNC_Success;    
}

int main () 
{
    reduction_test_context context;
    for(int i=0; i<10; i++) context.T1.put(i);
    context.wait();
    int n;
    //    context.R1.done(0);
    // context.R1.all_done();
    context.R1.get(0, n);
    printf("Retrieve reducer result at 0: %d\n", n);
    return 0;
}
