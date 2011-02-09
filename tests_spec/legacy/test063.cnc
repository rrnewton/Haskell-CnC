//Chain with env

//Is correct: No?
[type item<int>: int n];
<int singleton>;
<singleton> :: (Start);
env -> <singleton>;
(Start) -> [item] -> env; //Why is this incorrect?
