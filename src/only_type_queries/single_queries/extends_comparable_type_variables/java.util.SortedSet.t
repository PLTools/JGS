# dune exec jsons/run_json2.exe -- -n 5 -ct jsons_real/0.json only_type_queries/single_queries/extends_comparable_type_variables/java.util.SortedSet.json
  $ export NOBENCH=1
# 14s for the 1st answer is a cringe!
  $ timeout 15 run_json2 -ct ../../../jsons_real/0.json java.util.SortedSet.json -n 5
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.util.SortedSet (? extends java.lang.Comparable ((T <: java.lang.Object ())))
  timeout
  [124]
