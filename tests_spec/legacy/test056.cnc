//wrong consumer relation

//Is correct: No
[int item<int>];
<int tag>;
env -> <tag>;

<tag> :: (step);
(step) -> [item];
<tag> -> (step);

[item] -> env;
