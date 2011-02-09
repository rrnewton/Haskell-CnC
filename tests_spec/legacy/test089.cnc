//Priority functions
//Multiple declarations

//Is correct: No

(Work)priority_func=simplePriorityFunction,priority_func=simplePriorityFunction2;
(Start)priority=100,priority=200;
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
