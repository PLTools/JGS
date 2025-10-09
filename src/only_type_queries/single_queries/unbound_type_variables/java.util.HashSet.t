  $ export NOBENCH=1
  $ timeout 15 run_json2 -ct ../../../jsons_real/0.json java.util.HashSet.json -n 2
  Table size: 40960
  Negatives bound are not yet supported
  Running generated query
  	     Processing: ? <-< java.util.HashSet ((E <: java.lang.Object ()))
    1)  java.util.HashSet<[(? extends java.lang.Object)]>
    2)  java.util.LinkedHashSet<[(? extends java.lang.Object)]>
