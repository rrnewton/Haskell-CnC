//Priority functions
//function name ends with '_'

//Is correct: Yes

(Work)priority_func=a_;
(Start)priority_func="b_";
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
