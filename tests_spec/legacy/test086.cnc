//Priority functions
//function name starts with '_'

//Is correct: Yes

(Work)priority_func=_a;
(Start)priority_func="_b";
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
