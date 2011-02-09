//Empty names

//Is correct: No

(Work);
();
[type item<int>: int n];
[type item2<int>: int n];               
<int singleton>;
<singleton> :: (Start);
<int worker>;
<worker> :: (Work);
env -> <singleton>;
(Start) -> [item];
(Start) -> <worker>;
[item] -> (Work) -> [item2];
[item2] -> env; 
