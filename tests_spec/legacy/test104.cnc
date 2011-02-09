//Refcounting

//Is correct: No

(Work);
(Start);
[type item<int>: int n]priority_func=someFunc;
[type item2<int>: int n]priority=100;
<int singleton>;
<singleton> :: (Start);
<int worker>;
<worker> :: (Work);
env -> <singleton>;
(Start) -> [item];
(Start) -> <worker>;
[item] -> (Work) -> [item2];
[item2] -> env; 
