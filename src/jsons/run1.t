  $ ./run_json.exe test1.json
  [
    [ "I", { "iname": "A", "params": [], "supers": [] } ],
    [ "I", { "iname": "B", "params": [ [ "Class", "A", [] ] ], "supers": [] } ],
    [
      "C",
      {
        "cname": "D",
        "params": [ [ "Class", "Object", [] ] ],
        "super": [ "Class", "Object", [] ],
        "supers": []
      }
    ],
    [
      "C",
      {
        "cname": "E",
        "params": [
          [ "Class", "A", [] ],
          [ "Class", "D", [ [ "Type", [ "Class", "B", [] ] ] ] ]
        ],
        "super": [ "Class", "Object", [] ],
        "supers": []
      }
    ]
  ]
  1.1 (?) < Object : 
  [
    Class (1, []);
    Var {id=_.38, index=_.39, upb=Class (1, []), lwb=_.41};
    Interface (2, _.17);
    Null;
    Interface (3, _.17);
    Intersect ([Class (1, []) | _.94]);
    Array (Class (1, []));
    Intersect ([_.96 [=/= Class (1, [])]; Class (1, []) | _.183]);
    Intersect ([_.96 [=/= Class (1, [])]; _.185 [=/= Class (1, [])]; Class (1, []) | _.211]);
    Intersect ([_.96 [=/= Class (1, [])]; _.185 [=/= Class (1, [])]; _.213 [=/= Class (1, [])]; Class (1, []) | _.235])
  ]
