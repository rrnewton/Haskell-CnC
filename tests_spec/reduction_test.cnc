


tags<int>       T1;
steps           S1;
reductions<int> R1(plus);

// Here is a one-liner:
env -> T1 :: S1 -> R1 -> env;
