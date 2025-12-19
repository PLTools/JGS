  $ export NOBENCH=1
# should give List<int> but it doesn't
  $ timeout 5 ./run_json.exe 2.json -n 1
  Table size: 4
  Running generated query
  	     Processing: ? <-< List (? extends java.lang.Object ())
    1)  List<[java.lang.Object]>
