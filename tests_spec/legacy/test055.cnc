//wrong producer relation

//Is correct: No
[int item<int>];
<int tag>;
env -> <tag>;

<tag> :: (step);
(step) -> [item];

<tag> :: (anotherStep);
(anotherStep) -> [item];

(step) -> (anotherStep);

[item] -> env;
