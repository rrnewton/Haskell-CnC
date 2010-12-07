

tags <int>           T;
items<int, double>   I; 
steps                S;

env -> T;
T :: S;

S(i) -> T(i+1), I[i+1];
