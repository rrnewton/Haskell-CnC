//each produced item must be consumed

//Is correct: yes, with a warning
[int item<int>];
[int data<int>];
<int tag>;
env -> <tag>, [data];
<tag> :: (step);
[data] -> (step) -> [item], [data];
[data] -> env;
