//Deallocation

//Is correct: Yes

(Work);
(Start);
[type item<int>: int n]dealloc_func=f1,refcount=100;
[type item2<int>: int n]dealloc_func=f2, refcount_func=aaa1;
<int singleton>;
<singleton> :: (Start);
<int worker>;
<worker> :: (Work);
env -> <singleton>;
(Start) -> [item];
(Start) -> <worker>;
[item] -> (Work) -> [item2];
[item2] -> env; 
