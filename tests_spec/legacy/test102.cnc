//Refcounting

//Is correct: Yes

(Work);
(Start);
[type item<int>: int n]refcount_func=someFunc;
[type item2<int>: int n]refcount_func="anotherFunc";
<int singleton>;
<singleton> :: (Start);
<int worker>;
<worker> :: (Work);
env -> <singleton>;
(Start) -> [item];
(Start) -> <worker>;
[item] -> (Work) -> [item2];
[item2] -> env; 
