//Priority functions

//Is correct: No

// error says smth about line 90(!)

(Work)priority_func="100";
(Start)priority=0;
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
