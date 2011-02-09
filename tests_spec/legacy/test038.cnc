//complicated relations
// () :: <> :: ()

//Is correct: No
[int item<int>];
[int data<int>];
<int tag>;
(anotherStep) :: <tag> :: (step);
env -> <tag>, [item];
[item] -> (step) -> [item];
[item] -> (anotherStep);
[item] -> env;
