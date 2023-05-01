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
    Class (1, []);
    Var {id=_.38, index=_.39, upb=Class (1, []), lwb=_.41};
    Interface (2, _.17);
    Null;
    Intersect ([Class (1, []) | _.94])
  ]
  Running generated query
  
  [
    Class (1, []);
    Var {id=_.38, index=_.39, upb=Class (1, []), lwb=_.41};
    Interface (2, _.17);
    Null;
    Intersect ([Class (1, []) | _.94])
  ]
