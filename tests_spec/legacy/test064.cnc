//Chain with env

//Is correct: No?
[type item<int>: int n];
<int singleton>;
<singleton> :: (Start);
env -> <singleton>;
env -> [item] -> (Start); //Why is this incorrect?
(Start) -> [item];
[item] -> env; 
