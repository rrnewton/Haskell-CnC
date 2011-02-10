//Refcounting

//Is correct: Yes?

(Work);
(Start);
[type item<int>: int n]refcount="-100";
[type item2<int>: int n]refcount=0;
<int singleton>;
<singleton> :: (Start);
<int worker>;
<worker> :: (Work);
env -> <singleton>;
(Start) -> [item];
(Start) -> <worker>;
[item] -> (Work) -> [item2];
[item2] -> env; 
