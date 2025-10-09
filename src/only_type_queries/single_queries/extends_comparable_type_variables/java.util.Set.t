  $ export NOBENCH=1
# 14s for the 1st answer is a cringe!
  $ timeout 15 run_json2 -ct ../../../jsons_real/0.json java.util.Set.json -n 5
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.util.Set (? extends java.lang.Comparable ((T <: java.lang.Object ())))
  [124]
