open CT_of_json

let with_file name f =
  let ch = open_out name in
  try
    f ch;
    close_out ch
  with e ->
    close_out ch;
    raise e

let wrap filename ?(upper_bounds = []) ?(lower_bounds = [])
    ?(neg_lower_bounds = []) ?(neg_upper_bounds = []) table =
  let query =
    { table; upper_bounds; lower_bounds; neg_lower_bounds; neg_upper_bounds }
  in
  with_file filename (fun ch ->
      Yojson.Safe.pretty_to_channel ch (yojson_of_query query))

let () =
  let table =
    [
      make_c "java.lang.Object" ~params:[] [];
      make_i "IA" ~params:[] [];
      make_i "B"
        ~params:[ make_param "P1" ~up:[ Class ("java.lang.Object", []) ] ]
        [ Interface ("IA", []) ];
      make_c "D"
        ~params:[ make_param "P1" ~up:[ Class ("java.lang.Object", []) ] ]
        ~sup:(Class ("java.lang.Object", []))
        [];
      C
        {
          cname = "E";
          params = [ make_param "P1"; make_param "P2" ];
          super = Some (Class ("java.lang.Object", []));
          supers = [];
        };
    ]
  in
  let upper_bounds = [ Class ("java.lang.Object", []) ] in

  let query =
    {
      table;
      upper_bounds;
      lower_bounds = [];
      neg_lower_bounds = [];
      neg_upper_bounds = [];
    }
  in

  with_file "test1.json" (fun ch ->
      Yojson.Safe.pretty_to_channel ch (yojson_of_query query))

let () =
  wrap "2.json"
    ~upper_bounds:
      [
        Class
          ("List", [ Wildcard (Some (Extends, Class ("java.lang.Object", []))) ]);
      ]
    [
      make_c "java.lang.Object" ~params:[] [];
      make_c "String" ~sup:(Class ("java.lang.Object", [])) [] ~params:[];
      make_c "Int" ~sup:(Class ("java.lang.Object", [])) [] ~params:[];
      C
        {
          cname = "List";
          params = [ make_param "P1" ~up:[ Class ("java.lang.Object", []) ] ];
          super = Some (Class ("java.lang.Object", []));
          supers = [];
        };
    ]

let () =
  let table =
    [
      make_c "String" ~params:[] ~sup:(Class ("java.lang.Object", [])) [];
      make_c "java.lang.Object" ~params:[] [];
      I
        {
          iname = "Collection";
          iparams = [ { pname = "E"; p_upper = [] } ];
          isupers = [];
        };
      make_c "AbstractCollection"
        ~params:[ make_param "E" ~up:[ Class ("java.lang.Object", []) ] ]
        ~sup:(Interface ("Collection", [ Class ("E", []) ]))
        [];
    ]
  in

  let query =
    {
      table;
      upper_bounds = [ Class ("java.lang.Object", []) ];
      lower_bounds = [];
      neg_lower_bounds = [];
      neg_upper_bounds = [];
    }
  in

  with_file "test3.json" (fun ch ->
      Yojson.Safe.pretty_to_channel ch (yojson_of_query query))

let () =
  wrap "5.json"
    ~upper_bounds:[ Class ("java.lang.Object", []) ]
    [
      make_c "java.lang.Object" ~params:[] [];
      make_c "java.util.List"
        ~params:[ make_param "E" ~up:[ Class ("java.lang.Object", []) ] ]
        ~sup:(Class ("java.lang.Object", []))
        [];
      make_c "java.util.string" ~params:[]
        ~sup:(Class ("java.util.List", [ Class ("java.util.string", []) ]))
        [];
    ]

let () =
  wrap "6closure.json"
    ~upper_bounds:[ Interface ("ia", []) ]
    [
      make_i "ia" ~params:[] [];
      make_i "ib" ~params:[] [ Interface ("ia", []) ];
      make_i "ic" ~params:[] [ Interface ("ib", []) ];
      make_i "id" ~params:[] [ Interface ("ic", []) ];
    ]
