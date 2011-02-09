//Priority functions

//Is correct: Yes

(Work)priority_func="templateFunction<double, my_type*, 2+2-2*2/(2<<1)>";
(Start)priority_func="templatized<int>";
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
