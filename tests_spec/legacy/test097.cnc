//Priority functions
//Multiple declarations commented

//Is correct: Yes

(Work)priority_func=simplePriorityFunction;
//(Work)priority_func=simplePriorityFunction2;
(Start)priority=100;
//(Start)priority=200;
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
