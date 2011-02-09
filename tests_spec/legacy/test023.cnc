//type for item, components
//item declared twice with same type but with different components

//Is correct: No

[double* item<int> : char a, int* b, short c, myType_t d, double e, unsigned& f, char* item, float tag];
[double* item<int> : char a, int* b, short c, myType_t d, double e, unsigned& f, char* item];
<int tag>;
env -> <tag>;
<tag> :: (step);
(step) -> [item];
[item] -> env;
