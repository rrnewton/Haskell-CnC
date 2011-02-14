//complicated relations
// <- env ->

//Is correct: No
//  Dude, yes it is... what the heck would be wrong with it?
//  That is a pretty arbitrary rule.
// New compiler: allowed.
[int item<int>];
<int tag>;
<tag> :: (step);
<tag> <- env -> [item];
[item] -> (step) -> [item], <tag>;
[item] -> env;
