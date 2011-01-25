//a small correct cnc file
//multiline comment


/* this
   comment
   is not
   allowed */

// [2011.01.25] RRN: Note, under the new compiler this is allowed.
//Is correct: YES
[int item<int>];
<int tag>;
env -> <tag>;
<tag> :: (step);
(step) -> [item];
[item] -> env;
