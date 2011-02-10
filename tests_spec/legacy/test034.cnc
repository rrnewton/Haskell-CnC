//complicated relations
// <- env ->

//Is correct: No
[int item<int>];
<int tag>;
<tag> :: (step);
<tag> <- env -> [item];
[item] -> (step) -> [item], <tag>;
[item] -> env;
