  $ ./run_json.exe test1.json -default -n 5
  1.1 (?) < Object : 
  [
    Var {id=_.34, index=_.35, upb=Class (1, []), lwb=_.37};
    Null;
    Intersect ([Class (1, []) | _.104]);
    Class (1, []);
    Array (Class (1, []))
  ]
  Running generated query
  
  [
    Var {id=_.34, index=_.35, upb=Class (5, []), lwb=_.37};
    Null;
    Intersect ([Class (5, []) | _.104]);
    Class (5, []);
    Intersect ([_.106 [=/= Class (5, [])]; Class (5, []) | _.232])
  ]
