//Priority functions

//Is correct: Yes

(Work)priority_func=________________________;
(Start)priority_func="_";
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
