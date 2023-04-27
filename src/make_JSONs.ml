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
      I { iname = "A"; params = []; supers = [] };
      I { iname = "B"; params = [ Class ("A", []) ]; supers = [] };
      C
        {
          cname = "D";
          params = [ Class ("Object", []) ];
          super = Class ("Object", []);
          supers = [];
        };
      C
        {
          cname = "E";
          params = [ Class ("A", []); Class ("D", [ Type (Class ("B", [])) ]) ];
          super = Class ("Object", []);
          supers = [];
        };
    ]
  in

  with_file "test1.json" (fun ch ->
      Yojson.Safe.pretty_to_channel ch (yojson_of_table table))
