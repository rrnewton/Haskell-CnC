//items pruced only by env cannot be consumed only by env

//Is correct: No
[int item<int>];
[int data<int>];
<int tag>;
env -> <tag>;
<tag> :: (step);
(step) -> [item];
[item] -> env;

env -> [data] -> env;
