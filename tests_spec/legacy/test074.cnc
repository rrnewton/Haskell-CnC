//Priority functions

//Is correct: No

(Work)priority_func=simplePriorityFunction;
(Start)priority_func=arrayOfPointersToFunction[1];
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
