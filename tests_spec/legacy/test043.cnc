//each tag must be prescribed or consumed

//Is correct: Yes, with a warning
[int item<int>];
<int tag>;
<int anotherTag>;
env -> <tag>, <anotherTag>;
<tag> :: (step);
[item] -> (step) -> [item];
[item] -> env;
