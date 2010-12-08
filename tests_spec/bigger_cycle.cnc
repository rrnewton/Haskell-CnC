
tags <int>           T1;
tags <int>           T2;
steps                S1;
steps                S2;

env -> T1;

T1 :: S1(i) -> T2(i);
T2 :: S2(i) -> T1(i+1);
