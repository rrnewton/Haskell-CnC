//Priority functions
//name of a priority function is the same as a name of a step

//Is correct: Yes

(Work)priority_func=Work;
(Start)priority_func=Start;
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
