

tags <int>           T;
items<int, double>   In; 
items<int, double>   Out; 
steps                S;

env -> T;
T :: S;
In[i-1] -> S(i) -> Out[i];
