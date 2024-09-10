  $ export NOBENCH=1
# should give List<int> but it doesn't
  $ ./run_json.exe 2.json -n 2
  Table size: 4
  Running generated query
  	     Processing: ? <-< List (? extends java.lang.Object ())
    1)  List<[? Extends java.lang.Object]>
