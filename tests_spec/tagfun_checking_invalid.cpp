
// This version uses the same .cnc file but breaks the rules.

#include<tagfun_checking_valid.h>

template < class ctxt > 
int S1::execute( const int & tag, ctxt & c) const {
    printf("Step1 exec with tag %d\n", tag);
    c.T2.put(tag); 
    return CnC::CNC_Success;    
}

template < class ctxt > 
int S2::execute( const int & tag, ctxt & c) const {
    printf("Step2 exec with tag %d\n", tag);

//----------------------------------------
    printf("Step2 VIOLATES TAG FUNCTION HERE: +2 instead of +1:\n");
    c.I.put(tag + 2, (tag * 3.33)); 
//----------------------------------------

    return CnC::CNC_Success;    
}

int main () {
    printf ("Running with tagfun checking.\n");
    tagfun_checking_valid_context context;

    // Put some tags into the collection in a naive way:
    for(int i=0; i<5; i++)
       context.T1.put(i);

    context.wait();

    double fl;
    context.I.get(3, fl);
    printf("Retrieve a single item, index 3: %lf\n", fl);
    return 0;
}



