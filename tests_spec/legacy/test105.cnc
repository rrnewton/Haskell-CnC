//Refcounting

//Is correct: No

(Work);
(Start);
[type item<int>: int n];
[type item2<int>: int n];
<int singleton>priority=100;
<singleton> :: (Start);
<int worker>priority_func=someFunc;
<worker> :: (Work);
env -> <singleton>;
(Start) -> [item];
(Start) -> <worker>;
[item] -> (Work) -> [item2];
[item2] -> env; 
