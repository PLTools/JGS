  $ export NOBENCH=1
  $ ./run_json.exe test3.json -n 10
  
  Type variables mentioned in constraints: []
  
  Running generated query
  	     Processing: _.? <-< java.lang.Object ()
    1)  null
    2)  Array<java.lang.Object>
    3)  java.lang.Object
    4)  java.lang.Clonable
    5)  String
    6)  java.io.Serializable
  ERROR: free variables inside class id
    7)  Collection<[_.??]>
  ERROR: free variables inside class id
    8)  Collection<[_.??]>
    9)  Collection<[null]>
  ERROR: free variables inside class id
   10)  Collection<[Array<_.??>]>
