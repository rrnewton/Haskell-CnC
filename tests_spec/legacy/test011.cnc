//step declaration is allowed due to priorities, refcounting introduction

//Is correct: No
(step);
[int item<int>];
env -> <tag>;
<tag> :: (step);
(step) -> [item];
[item] -> env;
