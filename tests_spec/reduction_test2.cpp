

double plus (double x, double y) { return x + y; }
double times(double x, double y) { return x * y; }

#include<reduction_test2.h>

template < class ctxt > 
int S1::execute( const int & tag, ctxt & c) const 
{
    printf("S1(%d) begun\n", tag);
    for(int i=0; i<=tag; i++)
        //      c.R1.put(i, tag);
      c.R1.put(i, i);
    return CnC::CNC_Success;    
}

template < class ctxt > 
int S2::execute( const int & tag, ctxt & c) const 
{
    printf("S2(%d) begun\n", tag);
    double val[10];
    for(int i=0; i<=tag; i++)  c.R1.get(i, val[i]);
    for(int i=0; i<=tag; i++)  c.R2.put(i, val[i]);
    return CnC::CNC_Success;    
}

template < class ctxt > 
int S3::execute( const int & tag, ctxt & c) const 
{
    printf("S3(%d) begun\n", tag);
    double val;
    c.R2.get(tag, val);

    printf("Put into r3 %d, %lf\n", tag, val);

    c.R3.put(0, val);
    return CnC::CNC_Success;    
}

/*
Here is a quick check that the output is correct (6684686229).

let s1 = map sum $ transpose $ inits [0..9]
let s2 = map product $ transpose $ inits s1
let s3 = sum s2

s1: [0,9,16,21,24,25,24,21,16,9]
s2: [0,387420489,4294967296,1801088541,191102976,9765625,331776,9261,256,9]
s3 is 6684686229

*/

int main () 
{
    reduction_test2_context context;
    for(int i=0; i<10; i++) context.T1.put(i);
    context.wait();
    double tmp;

    printf("\nR1: ");
    for(int i=0; i<10; i++) { context.R1.get(i, tmp); printf("%lf ", tmp); }
    printf("\nR2: ");
    for(int i=0; i<10; i++) { context.R2.get(i, tmp); printf("%lf ", tmp); }
    context.R3.get(0, tmp);
    printf("\nR3[0] = %lf\n", tmp);

    return 0;
}
