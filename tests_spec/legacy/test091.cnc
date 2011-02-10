//Priority functions
//Multiple declarations

//Is correct: No

(Work);
(Work)priority_func=simplePriorityFunction;
(Start);
(Start)priority=100;
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
