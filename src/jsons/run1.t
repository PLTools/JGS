  $ ./run_json.exe test1.json -default -n 5
  {
    "table": [
      [ "I", { "iname": "A", "iparams": [], "isupers": [] } ],
      [
        "I",
        {
          "iname": "B",
          "iparams": [],
          "isupers": [ [ "Interface", "A", [] ] ]
        }
      ],
      [
        "C",
        {
          "cname": "D",
          "params": [
            { "pname": "P1", "p_upper": [ [ "Class", "Object", [] ] ] }
          ],
          "super": [ "Class", "Object", [] ],
          "supers": []
        }
      ],
      [
        "C",
        {
          "cname": "E",
          "params": [
            { "pname": "P1", "p_upper": [] }, { "pname": "P2", "p_upper": [] }
          ],
          "super": [ "Class", "Object", [] ],
          "supers": []
        }
      ]
    ],
    "upper_bounds": [ [ "Class", "Object", [] ] ],
    "lower_bounds": [],
    "neg_upper_bounds": [],
    "neg_lower_bounds": []
  }
  Adding an interface A with id = 4
  Adding an interface B with id = 5
  Adding a class D with id  = 6
  Adding a class E with id  = 8
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
    Var {id=_.34, index=_.35, upb=Class (1, []), lwb=_.37};
    Null;
    Intersect ([Class (1, []) | _.104]);
    Class (1, []);
    Array (Class (1, []))
  ]
