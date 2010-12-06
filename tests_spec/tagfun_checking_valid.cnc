

tags <int>           T1;
tags <int>           T2;
items<int, double>   I; 
steps                S1;
steps                S2;

// T(i) :: S(i) -> I[i+1], maybe(T(i+1));
// T(i) :: S(i) -> I[i+1], T(maybe(i+1));
// T(i) :: S(i) -> I[i+1], maybe T(i+1);
// T(i) :: S(i) -> I[i+1], T(i+1);

env -> T1;
T1 :: S1;

S1(i) -> T2(i);

T2 :: S2;
S2(i) -> I[i+1];


