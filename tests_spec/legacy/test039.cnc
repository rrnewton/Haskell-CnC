//complicated relations
// [] -> () <- []

//Is correct: No
[int item<int>];
[int data<int>];
<int tag>;
<tag> :: (step);
env -> <tag>, [item];
[item] -> (step) <- [item];
(step) -> [item];
[item] -> env;
