  $ export NOBENCH=1
  $ ./run_json.exe 7.json -n 6 -v
  Add edge: java.lang.Object -> int
  Add edge: java.lang.Object -> ICollection
  Add edge: java.lang.Object -> list
  Add edge: ICollection -> list
  Add edge: java.lang.Object -> list
  Adding a class "int" with id  = 4
  Adding an interface ICollection with id = 5
  Adding a class "list" with id  = 7
  
  Type variables mentioned in constraints: []
  
  Running generated query
  	     Processing: _.? <-< ICollection (int ())
    1)  ICollection<[int]>
    2)  java.lang.Object
    3)  null
    4)  null
    5)  null
    6)  Array<java.lang.Object>
