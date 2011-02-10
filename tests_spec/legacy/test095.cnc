//Priority functions
//Arithmetics

//Is correct: Yes

(Work)priority="0+0-0*0/1";
(Start)priority="100+100";
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
