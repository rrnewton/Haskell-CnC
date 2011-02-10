//type for item, components
//item declared twice with same type, with same components but with different component names

//Is correct: Yes

[double* item<int> : char a, int* b, short c, myType_t d, double e, unsigned& f, char* item, float tag];
[double* item<int> : char z, int* x, short v, myType_t b, double n, unsigned& m, char* i, float t];
<int tag>;
env -> <tag>;
<tag> :: (step);
(step) -> [item];
[item] -> env;
