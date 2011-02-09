//Priority functions
//Different case

//Is correct: Yes

(Work)PrIoRiTy_FuNc=simplePriorityFunction;
(Start)prIORitY=100;
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
