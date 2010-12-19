
// Two cycles that communicate with one another.

tags <int>           T1;
tags <int>           T2;
tags <int>           T3;
tags <int>           T4;
steps                S1;
steps                S2;
steps                S3;
steps                S4;

// Cycle one:
env -> T1 :: S1 
    -> T2 :: S2 -> T1;

// Cycle two:
S2  -> T3 :: S3 
    -> T4 :: S4 -> T3;

// Add an extra edge to see what happens:
// S1 -> T3;

// [2010.12.15] As expected, the extra edge currently causes redundant checks like this:
// if ((c.done_flag1 && c.done_flag1))

// The `done` condition for cycle (S3-S4) should depend on the cycle (S1-S2).

// At the end you should see:
 // [autodone] S2: Node(s) done [S2,S1,T2,T1], deps met: [CGSteps env]
 // [autodone] S4: Node(s) done [S4,S3,T4,T3], deps met: [CGSteps S2]
