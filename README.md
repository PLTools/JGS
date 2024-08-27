# JGS
Java Generics Constraint Solver


#### Duplicate testing 

Essentially, it is a beforehand cut of branches of the search that could remove duplicates, 
and speedup implementation by not doing extra work. 
The `debug_var_implementation` works better, so called structural constraints are slower 
because of they are checked too often.

Future work optimization: Hash consing of the trees

