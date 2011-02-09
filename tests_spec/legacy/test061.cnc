//env -> env

//Is correct: No
[int item<int>];
<int tag>;
env -> <tag>;
<tag> :: (step);
(step) -> [item];
env, [item] -> env;
