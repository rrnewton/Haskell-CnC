//no type specification is allowed in consumer relation

//Is correct: No
[int item<int>: int number];
<int tag: int a, int b>;
env -> <tag>;
<tag : x, y> :: (step : a, b);
(step : a, b) -> [int item: sum(a,b)];
[item] -> env;
