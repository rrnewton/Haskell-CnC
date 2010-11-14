

tags <int>           T;
items<int, double>   I; 
steps                S;

env -> T;
T(i) :: S(i) -> I[i+1];
