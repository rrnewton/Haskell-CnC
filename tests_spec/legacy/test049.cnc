//using tag components

//Is correct: Yes
[int item<int>];
<int tag : int a, int b, int c>;
env -> <tag>;
<tag : i, j, k> :: (step);
(step) -> [item];
[item] -> env;
