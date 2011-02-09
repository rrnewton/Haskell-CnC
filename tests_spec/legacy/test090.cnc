//Priority functions
//Multiple declarations

//Is correct: Yes ?

(Work)priority_func=simplePriorityFunction,priority_func=simplePriorityFunction;
(Start)priority=100,priority=100;
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
