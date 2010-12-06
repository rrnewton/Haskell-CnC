

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
    // Obey the tag function, put tag + 1:
    c.I.put(tag + 1, (tag * 3.33)); 
    return CnC::CNC_Success;    
}

int main () {
    printf ("Running with tagfun checking.\n");
    tagfun_checking_valid_context context;

    // Put some tags into the collection in a naive way:
    for(int i=0; i<10; i++)
       context.T1.put(i);

    context.wait();

    double fl;
    context.I.get(3, fl);
    printf("Retrieve a single item, index 3: %lf\n", fl);
    return 0;
}
