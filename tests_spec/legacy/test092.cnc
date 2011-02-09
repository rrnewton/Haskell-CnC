//Priority functions
//Multiple declarations

//Is correct: No

(Work)priority_func=simplePriorityFunction;
(Work);
(Start)priority=100;
(Start);
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
