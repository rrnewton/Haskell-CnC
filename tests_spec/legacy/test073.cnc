//Priority functions

//Is correct: Yes

(Work)priority_func=simplePriorityFunction;
(Start)priority_func="another_simple";
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
