  $ export NOBENCH=1
# should give List<int> but it doesn't
  $ ./run_json.exe 2.json -n 2
  
  Type variables mentioned in constraints: []
  
  Running generated query
  	     Processing: _.? <-< List (? extends java.lang.Object ())
    1)  null
    2)  List<[java.lang.Object]>
