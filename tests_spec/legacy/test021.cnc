//type for item, components
//item declared twice with same components, but with different type

//Is correct: No

[double* item<int> : char a, int* b, short c, myType_t d, double e, unsigned& f, char* item, float tag];
[int item<int> : char a, int* b, short c, myType_t d, double e, unsigned& f, char* item, float tag];
<int tag>;
env -> <tag>;
<tag> :: (step);
(step) -> [item];
[item] -> env;
