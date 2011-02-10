//items pruduced only by env cannot be consumed only by env

//Is correct: Yes, with a warning
[int item<int>];
[int data<int>];
<int tag>;
env -> <tag>;
<tag> :: (step);
(step) -> [item];
[item] -> env;

env -> [data];
[data] -> env;
