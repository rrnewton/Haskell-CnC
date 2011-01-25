// '-' is not allowed in names

//Is correct: No
[int employee<int>];
<int employeeID>;
env -> <employeeID>;
<employeeID> :: (compute-pay-for-month);
(compute-pay-for-month) -> [employee];
[employee] -> env;
