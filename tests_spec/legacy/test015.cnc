//not a connected graph specified

//Is correct: Yes ?
[int firstItem<int>];
<int firstTag>;

env -> <firstTag>;
<firstTag> :: (firstStep);
(firstStep) -> [firstItem];
[firstItem] -> env;

[int secondItem<int>];
<int secondTag>;

env -> <secondTag>;
<secondTag> :: (secondStep);
(secondStep) -> [secondItem];
[secondItem] -> env;
