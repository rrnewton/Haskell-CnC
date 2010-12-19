


#include<two_cycles.h>

template < class ctxt > 
int S1::execute( const int & tag, ctxt & c) const {
    printf("Step1 exec with tag %d\n", tag);
    c.T2.put(tag + 1); 
    return CnC::CNC_Success;    
}

template < class ctxt > 
int S2::execute( const int & tag, ctxt & c) const {
    printf("Step2 exec with tag %d\n", tag);
    if (tag < 10) { 
      c.T1.put(tag + 1);      
      c.T3.put(tag + 1);
    }
    return CnC::CNC_Success;    
}

template < class ctxt > 
int S3::execute( const int & tag, ctxt & c) const {
    printf("Step3 exec with tag %d\n", tag);
    c.T4.put(tag + 1); 
    return CnC::CNC_Success;    
}

template < class ctxt > 
int S4::execute( const int & tag, ctxt & c) const {
    printf("Step4 exec with tag %d\n", tag);
    if (tag < 10) { 
      c.T3.put(tag + 1);
    }
    return CnC::CNC_Success;    
}




int main () {
    two_cycles_context context;
    context.T1.put(0);
    context.wait();
    printf("Wait completed.\n");
    return 0;
}
