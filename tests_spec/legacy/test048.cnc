//different tag component declaration

//Is correct: No
[int item<int>];
<int tag : int a, int b, int c>;
env -> <tag>;
<tag : id> :: (step);
(step) -> [item];
[item] -> env;
