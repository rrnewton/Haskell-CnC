//environment: env, Env, ENV

//Is correct: Yes
[int item<int> ];
<int tag>;
<tag> :: (step);
env -> <tag>;
Env -> [item];
[item] -> (step) -> [item];
[item] -> ENV;
