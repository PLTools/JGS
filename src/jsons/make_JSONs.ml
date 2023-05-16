open CT_of_json

let with_file name f =
  let ch = open_out name in
  try
    f ch;
    close_out ch
  with e ->
    close_out ch;
    raise e

let () =
  let table =
    [
      make_c "java.lang.Object" ~params:[] [];
      I { iname = "A"; iparams = []; isupers = [] };
      I { iname = "B"; isupers = [ Interface ("A", []) ]; iparams = [] };
      C
        {
          cname = "D";
          params = [ make_param "P1" ~up:[ Class ("java.lang.Object", []) ] ];
          super = Some (Class ("java.lang.Object", []));
          supers = [];
        };
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
  let table =
    [
      make_c "java.lang.Object" ~params:[] [];
      C
        {
          cname = "String";
          params = [];
          super = Some (Class ("java.lang.Object", []));
          supers = [];
        };
      C
        {
          cname = "Int";
          params = [];
          super = Some (Class ("java.lang.Object", []));
          supers = [];
        };
      C
        {
          cname = "List";
          params = [ make_param "P1" ~up:[ Class ("java.lang.Object", []) ] ];
          super = Some (Class ("java.lang.Object", []));
          supers = [];
        };
    ]
  in
  let upper_bounds =
    [
      Class
        ("List", [ Wildcard (Some (Extends, Class ("java.lang.Object", []))) ]);
    ]
  in

  let query =
    {
      table;
      upper_bounds;
      lower_bounds = [];
      neg_lower_bounds = [];
      neg_upper_bounds = [];
    }
  in

  with_file "test2.json" (fun ch ->
      Yojson.Safe.pretty_to_channel ch (yojson_of_query query))

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
  let table =
    [
      make_c "String" ~params:[]
        ~sup:(Class ("java.util.List", [ Class ("java.util.string", []) ]))
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

  with_file "test5.json" (fun ch ->
      Yojson.Safe.pretty_to_channel ch (yojson_of_query query))
