  $ export NOBENCH=1
# 14s (direct subtyping) for the 1st answer is a cringe!
# 26s (closure subtyping)
# I tried to rewrite query from using a variable to usage of Wildcards. Performance become worse!
  $ timeout 15 run_json2  -ct ../../../jsons_real/0.json java.util.NavigableSet.json -n 5
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.util.NavigableSet (? extends java.lang.Comparable ((T <: java.lang.Object ())))
  [124]
