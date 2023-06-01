  $ ./run_json.exe 7.json -n 6 -v
  Class   with id=1 was created
  Interface   with id=2 was created
  Interface   with id=3 was created
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
    0.1ms   1)  ICollection<[int]>
    0.6ms   2)  java.lang.Object
    0.4ms   3)  null
    2.5ms   4)  null
    0.4ms   5)  null
    1.8ms   6)  Array<java.lang.Object>
