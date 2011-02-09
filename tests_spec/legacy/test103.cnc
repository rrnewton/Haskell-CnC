//Refcounting

//Is correct: No

(Work);
(Start);
[type item<int>: int n]refcount_func=someFunc<int>;
[type item2<int>: int n]refcount_func="anotherFunc<1,int,2+2>";
<int singleton>;
<singleton> :: (Start);
<int worker>;
<worker> :: (Work);
env -> <singleton>;
(Start) -> [item];
(Start) -> <worker>;
[item] -> (Work) -> [item2];
[item2] -> env; 
