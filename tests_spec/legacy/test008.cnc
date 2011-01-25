// '-' is not allowed in names

//Is correct: No
[int employee<int>];
<int employee-identifier>;
env -> <employee-identifier>;
<employee-identifier> :: (step);
(step) -> [employee];
[employee] -> env;
