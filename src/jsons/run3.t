  $ ./run_json.exe test3.json -n 20
  {
    "table": [
      [
        "C",
        {
          "cname": "String",
          "params": [],
          "super": [ "Class", "Object", [] ],
          "supers": []
        }
      ],
      [
        "I",
        {
          "iname": "Collection",
          "iparams": [ { "pname": "E", "p_upper": [] } ],
          "isupers": []
        }
      ],
      [
        "C",
        {
          "cname": "AbstractCollection",
          "params": [
            { "pname": "E", "p_upper": [ [ "Class", "Object", [] ] ] }
          ],
          "super": [
            "Interface", "Collection", [ [ "Type", [ "Class", "E", [] ] ] ]
          ],
          "supers": []
        }
      ]
    ],
    "upper_bounds": [ [ "Class", "Object", [] ] ],
    "lower_bounds": [],
    "neg_upper_bounds": [],
    "neg_lower_bounds": []
  }
  Adding a class String with id  = 4
  Adding an interface Collection with id = 5
  Adding a class AbstractCollection with id  = 7
  Running generated query
  
  [
    Class (1, []);
    Var {id=_.38, index=_.39, upb=Class (1, []), lwb=_.41};
    Interface (2, _.17);
    Null;
    Intersect ([Class (1, []) | _.94]);
    Interface (3, _.17);
    Array (Class (1, []));
    Interface (5, _.17);
    Intersect ([_.96 [=/= Class (1, [])]; Class (1, []) | _.181]);
    Intersect ([_.96 [=/= Class (1, [])]; _.183 [=/= Class (1, [])]; Class (1, []) | _.196]);
    Intersect ([_.96 [=/= Class (1, [])]; _.183 [=/= Class (1, [])]; _.198 [=/= Class (1, [])]; Class (1, []) | _.214]);
    Intersect ([_.96 [=/= Class (1, [])]; _.183 [=/= Class (1, [])]; _.198 [=/= Class (1, [])]; _.216 [=/= Class (1, [])]; Class (1, []) | _.231]);
    Intersect ([_.96 [=/= Class (1, [])]; _.183 [=/= Class (1, [])]; _.198 [=/= Class (1, [])]; _.216 [=/= Class (1, [])]; _.233 [=/= Class (1, [])]; Class (1, []) | _.258]);
    Class (4, _.15);
    Intersect ([_.96 [=/= Class (1, [])]; _.183 [=/= Class (1, [])]; _.198 [=/= Class (1, [])]; _.216 [=/= Class (1, [])]; _.233 [=/= Class (1, [])]; _.260 [=/= Class (1, [])]; Class (1, []) | _.279]);
    Intersect ([_.96 [=/= Class (1, [])]; _.183 [=/= Class (1, [])]; _.198 [=/= Class (1, [])]; _.216 [=/= Class (1, [])]; _.233 [=/= Class (1, [])]; _.260 [=/= Class (1, [])]; _.281 [=/= Class (1, [])]; Class (1, []) | _.302]);
    Intersect ([_.96 [=/= Class (1, [])]; _.183 [=/= Class (1, [])]; _.198 [=/= Class (1, [])]; _.216 [=/= Class (1, [])]; _.233 [=/= Class (1, [])]; _.260 [=/= Class (1, [])]; _.281 [=/= Class (1, [])]; _.304 [=/= Class (1, [])]; Class (1, []) | _.321]);
    Intersect ([_.96 [=/= Class (1, [])]; _.183 [=/= Class (1, [])]; _.198 [=/= Class (1, [])]; _.216 [=/= Class (1, [])]; _.233 [=/= Class (1, [])]; _.260 [=/= Class (1, [])]; _.281 [=/= Class (1, [])]; _.304 [=/= Class (1, [])]; _.323 [=/= Class (1, [])]; Class (1, []) | _.337]);
    Intersect ([_.96 [=/= Class (1, [])]; _.183 [=/= Class (1, [])]; _.198 [=/= Class (1, [])]; _.216 [=/= Class (1, [])]; _.233 [=/= Class (1, [])]; _.260 [=/= Class (1, [])]; _.281 [=/= Class (1, [])]; _.304 [=/= Class (1, [])]; _.323 [=/= Class (1, [])]; _.339 [=/= Class (1, [])]; Class (1, []) | _.356]);
    Intersect ([_.96 [=/= Class (1, [])]; _.183 [=/= Class (1, [])]; _.198 [=/= Class (1, [])]; _.216 [=/= Class (1, [])]; _.233 [=/= Class (1, [])]; _.260 [=/= Class (1, [])]; _.281 [=/= Class (1, [])]; _.304 [=/= Class (1, [])]; _.323 [=/= Class (1, [])]; _.339 [=/= Class (1, [])]; _.358 [=/= Class (1, [])]; Class (1, []) | _.373])
  ]
