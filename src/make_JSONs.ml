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
      I { iname = "A"; iparams = []; isupers = [] };
      I { iname = "B"; isupers = [ Interface ("A", []) ]; iparams = [] };
      C
        {
          cname = "D";
          params = [ make_param "P1" ~up:[ Class ("Object", []) ] ];
          super = Class ("Object", []);
          supers = [];
        };
      C
        {
          cname = "E";
          params = [ make_param "P1"; make_param "P2" ];
          super = Class ("Object", []);
          supers = [];
        };
    ]
  in
  let upper_bounds = [ Class ("Object", []) ] in

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
      C
        {
          cname = "String";
          params = [];
          super = Class ("Object", []);
          supers = [];
        };
      C
        {
          cname = "Int";
          params = [];
          super = Class ("Object", []);
          supers = [];
        };
      C
        {
          cname = "List";
          params = [ make_param "P1" ~up:[ Class ("Object", []) ] ];
          super = Class ("Object", []);
          supers = [];
        };
    ]
  in
  let upper_bounds =
    [ Class ("List", [ Wildcard (Some (Extends, Class ("Object", []))) ]) ]
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
