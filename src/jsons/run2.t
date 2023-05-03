  $ ./run_json.exe test2.json -n 7
  Adding a class String with id  = 4
  Adding a class Int with id  = 5
  Adding a class List with id  = 6
  Running generated query
  
  [
    Var {id=_.34, index=_.35, upb=Class (6, [Wildcard (Some ((Extends, Class (1, []))))]), lwb=_.37};
    Null;
    Intersect ([Class (6, [Wildcard (Some ((Extends, Class (1, []))))]) | _.104]);
    Intersect ([_.106 [=/= Class (6, [Wildcard (Some ((Extends, Class (1, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (1, []))))]) | _.232]);
    Intersect ([_.106 [=/= Class (6, [Wildcard (Some ((Extends, Class (1, []))))])]; _.234 [=/= Class (6, [Wildcard (Some ((Extends, Class (1, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (1, []))))]) | _.317]);
    Intersect ([_.106 [=/= Class (6, [Wildcard (Some ((Extends, Class (1, []))))])]; _.234 [=/= Class (6, [Wildcard (Some ((Extends, Class (1, []))))])]; _.319 [=/= Class (6, [Wildcard (Some ((Extends, Class (1, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (1, []))))]) | _.348]);
    Intersect ([_.106 [=/= Class (6, [Wildcard (Some ((Extends, Class (1, []))))])]; _.234 [=/= Class (6, [Wildcard (Some ((Extends, Class (1, []))))])]; _.319 [=/= Class (6, [Wildcard (Some ((Extends, Class (1, []))))])]; _.350 [=/= Class (6, [Wildcard (Some ((Extends, Class (1, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (1, []))))]) | _.380])
  ]
