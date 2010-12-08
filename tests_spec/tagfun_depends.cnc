

tags <int>           T;
items<int, double>   In; 
items<int, double>   Out; 
steps                S;

env -> T, In;
T :: S;

In[i-1],
In[i], 
In[i+1] -> S(i) -> Out[i];

env <- Out;
