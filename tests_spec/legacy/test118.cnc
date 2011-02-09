//Empty names

//Is correct: No

(Work);
(Start);
[type item<int>: int n];
[type item2<int>: int n];               
<int singleton>;
<singleton> :: (Start);
<int worker>;
<worker> :: (Work);
 -> <singleton>;
(Start) -> [item];
(Start) -> <worker>;
[item] -> (Work) -> [item2];
[item2] -> env; 
