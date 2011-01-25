//a small correct cnc file
//relation () :: <>

// [2011.01.25] RRN: Update: in the the new compiler I am not supporting this backwards prescribe.

//Is correct: Yes
[int item<int>];
<int tag>;
env -> <tag>;
(step) :: <tag>;
(step) -> [item];
[item] -> env;
