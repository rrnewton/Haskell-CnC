
// Here we test some tag functions

tags <(int,int)>      T;
items<int, Foo>       I1;
items<(int,int), Bar> I2;
steps                 S;


// Here is a system of linear equations.
// T :: S(i+2, 2*j) -> I[3*j, i + 2*j + 4];

// TEMP, for the current prototype we require that the STEP be indexed by simple variables.  This guarantees
// TOTAL FUNCTIONS.  Otherwise, we could have situations where the step is restricted, say, to even numbers.
// If the domain of the step is restricted, then we have a whole additional class of errors. 
T :: S;

I1[i] -> S(i, j) -> I2[6*j, i + j + 2];
