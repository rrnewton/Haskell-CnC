//Priority functions
//Do not specify both priority and priority_func

//Is correct: No

(Work)priority_func=pf1, priority=10;
(Start)priority_func=pf2;
[type item<int>: int n];
<int singleton>;
<singleton> :: (Start);
<int worker>;
<worker> :: (Work);
env -> <singleton>;
(Start) -> [item];
(Start) -> <worker>;
(Work) -> [item];
[item] -> env; 
