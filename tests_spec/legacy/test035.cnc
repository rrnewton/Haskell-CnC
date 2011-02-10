//complicated relations
// -> env <-

//Is correct: No
[int item<int>];
[int data<int>];
<int tag>;
<tag> :: (step);
env -> [item], <tag>;
[item] -> (step) -> [item], <tag>, [data];
[item] -> env <- [data];
