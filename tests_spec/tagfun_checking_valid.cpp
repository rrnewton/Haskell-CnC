

#include<tagfun_checking_valid.h>

template < class ctxt > 
int S::execute( const int & tag, ctxt & c) const {
    printf("Step exec with tag %d\n", tag);

    // Obey the tag function, put tag + 1:
    c.I.put(tag + 1, (tag * 3.33)); 

    return CnC::CNC_Success;    
}

int main () {
    printf ("Running with tagfun checking.\n");
    tagfun_checking_valid_context context;

    // Put some tags into the collection in a naive way:
    for(int i=0; i<10; i++)
      context.T.put(i);

    context.wait();

    double fl;
    context.I.get(3, fl);

    printf("Retrieve a single item, index 3: %lf\n", fl);
    return 0;
}
