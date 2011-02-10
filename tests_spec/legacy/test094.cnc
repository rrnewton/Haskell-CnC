//Priority functions
//Multiple declarations

//Is correct: No

(Work)priority_func=simplePriorityFunction;
(Work)priority_func=simplePriorityFunction;
(Start)priority=100;
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
