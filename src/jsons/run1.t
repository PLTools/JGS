  $ export NOBENCH=1
  $ ./run_json.exe test1.json -n 5 -default
  
  Type variables mentioned in constraints: []
  
  Running generated query
  1.1 (?) < Object :
    1)  java.lang.Object
    2)  java.lang.Clonable
    3)  java.io.Serializable
    4)  IA
  ERROR: free variables inside class id
    5)  D<[_.??]>
  	     Processing: _.? <-< java.lang.Object ()
    1)  null
    2)  Array<java.lang.Object>
    3)  java.lang.Object
    4)  java.lang.Clonable
    5)  java.io.Serializable
