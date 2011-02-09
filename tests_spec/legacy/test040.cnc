//step collection must have output

//Is correct: No
[int item<int>];
<int tag>;
env -> <tag>, [item];
<tag> :: (step);
[item] -> (step);
[item] -> env;
