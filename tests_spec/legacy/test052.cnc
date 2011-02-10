//same components in prescription
//step produces an item with lots of components

//Is correct: Yes
[int item<int>: int i, int j, int k];
<int tag: int a, int b>;
env -> <tag>;
<tag : x, y> :: (step : a, b);
(step) -> [item: i,j,k];
[item] -> env;
