//complicated relations
// env, env

//Is correct: No
[int item<int>];
[int data<int>];
<int tag>;
<tag> :: (step);
env, env -> [item], <tag>;
[item] -> (step) -> [item];
[item] -> env;
