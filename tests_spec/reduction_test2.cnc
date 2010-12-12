


tags<int>       T1;
steps           S1;
steps           S2;
steps           S3;
reductions<int, double> R1(plus, 0 );
reductions<int, double> R2(times, 1);
reductions<int, double> R3(plus, 0);

T1 :: S1, S2, S3;

env -> T1;
 S1 -> R1 -> 
 S2 -> R2 -> 
 S3 -> R3 -> env;
