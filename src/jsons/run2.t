  $ ./run_json.exe test2.json -n 7
  	 param_of_yojson: "{\"pname\":\"P1\",\"p_upper\":[[\"Class\",\"java.lang.Object\",[]]]}"
  Fallback: it's not  a type
  Running generated query
  
  [
    Var {id=_.34, index=_.35, upb=Class (6, [Wildcard (Some ((Extends, Class (4, []))))]), lwb=_.37};
    Null;
    Intersect ([Class (6, [Wildcard (Some ((Extends, Class (4, []))))]) | _.104]);
    Intersect ([_.106 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (4, []))))]) | _.232]);
    Intersect ([_.106 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.234 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (4, []))))]) | _.317]);
    Intersect ([_.106 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.234 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.319 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (4, []))))]) | _.345]);
    Intersect ([_.106 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.234 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.319 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; _.347 [=/= Class (6, [Wildcard (Some ((Extends, Class (4, []))))])]; Class (6, [Wildcard (Some ((Extends, Class (4, []))))]) | _.379])
  ]
