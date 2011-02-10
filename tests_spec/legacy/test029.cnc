//tag components, not only ints

//Is correct: Yes
[int item<int>];
<int tag : char a, short b, unsigned c, int d, long e>;
env -> <tag>;
<tag> :: (step);
(step) -> [item];
[item] -> env;
