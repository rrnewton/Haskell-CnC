


tags<int>       T1;
steps           S1;
reductions<int,int> R1(plus, 0);

// Here is a one-liner:
env -> T1 :: S1 -> R1 -> env;
