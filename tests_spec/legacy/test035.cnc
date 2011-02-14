//complicated relations
// -> env <-

//Is correct: No
// [2011.02.14] Is now.
[int item<int>];
[int data<int>];
<int tag>;
<tag> :: (step);
env -> [item], <tag>;
[item] -> (step) -> [item], <tag>, [data];
[item] -> env <- [data];
