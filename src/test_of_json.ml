open CT_of_json

let%expect_test _ =
  Format.printf "%a\n%!"
    (Yojson.Safe.pretty_print ~std:true)
    (yojson_of_polarity Extends);
  [%expect {| [ "Extends" ] |}]

let%expect_test "test wildcards" =
  let pp t =
    Format.printf "%a\n%!"
      (Yojson.Safe.pretty_print ~std:true)
      (yojson_of_jtype t)
  in
  pp (Wildcard None);
  [%expect {| [ "Wildcard", null ] |}];
  pp (Wildcard (Some (Extends, Class ("Object", []))));
  [%expect {| [ "Wildcard", [ [ "Extends" ], [ "Class", "Object", [] ] ] ] |}];
  pp (Wildcard (Some (Super, Class ("Int", []))));
  [%expect {| [ "Wildcard", [ [ "Super" ], [ "Class", "Int", [] ] ] ] |}];

  pp (Wildcard None);
  [%expect {| [ "Wildcard", null ] |}]

let%expect_test _ =
  let pp t =
    let j = yojson_of_jtype t in
    Format.printf "%s\n%!" (Yojson.Safe.show j);
    Format.printf "%a\n%!" (Yojson.Safe.pretty_print ~std:true) j
  in
  let t = var "XXX" (Class ("Object", [])) in
  pp t;
  [%expect
    {|
    `List ([`String ("Var");
             `Assoc ([("id", `String ("XXX")); ("index", `Int (0));
                       ("upb",
                        `List ([`String ("Class"); `String ("Object"); `List (
                                 [])]));
                       ("lwb", `Null)])
             ])
    [
      "Var",
      { "id": "XXX", "index": 0, "upb": [ "Class", "Object", [] ], "lwb": null }
    ] |}];
  pp @@ var "XXX" (Class ("Object", []));
  [%expect
    {|
    `List ([`String ("Var");
             `Assoc ([("id", `String ("XXX")); ("index", `Int (0));
                       ("upb",
                        `List ([`String ("Class"); `String ("Object"); `List (
                                 [])]));
                       ("lwb", `Null)])
             ])
    [
      "Var",
      { "id": "XXX", "index": 0, "upb": [ "Class", "Object", [] ], "lwb": null }
    ] |}];
  pp @@ var "E" (Class ("java.lang.Object", []));
  [%expect
    {|
    `List ([`String ("Var");
             `Assoc ([("id", `String ("E")); ("index", `Int (0));
                       ("upb",
                        `List ([`String ("Class"); `String ("java.lang.Object");
                                 `List ([])]));
                       ("lwb", `Null)])
             ])
    [
      "Var",
      {
        "id": "E",
        "index": 0,
        "upb": [ "Class", "java.lang.Object", [] ],
        "lwb": null
      }
    ] |}]

let%expect_test _ =
  let j =
    Yojson.Safe.from_string
      {|
          [
            "Var",
            {
              "id": "E",
              "index": 0,
              "upb": [ "Class", "Object", [] ]
            }
          ]
|}
  in
  Format.printf "%s\n%!" (Yojson.Safe.show j);
  [%expect
    {|
    `List ([`String ("Var");
             `Assoc ([("id", `String ("E")); ("index", `Int (0));
                       ("upb",
                        `List ([`String ("Class"); `String ("Object"); `List (
                                 [])]))
                       ])
             ]) |}];
  let () =
    match jtype_of_yojson j with
    | exception Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (exc, j) ->
        Format.printf "%s\n%s\n%!" (Printexc.to_string exc) (Yojson.Safe.show j)
    | _ -> Format.printf "OK\n%!"
  in
  [%expect
    {|
    Failure("CT_of_json.ml.jtype_of_yojson: the following record elements were undefined: lwb")
    `List ([`String ("Var");
             `Assoc ([("id", `String ("E")); ("index", `Int (0));
                       ("upb",
                        `List ([`String ("Class"); `String ("Object"); `List (
                                 [])]))
                       ])
             ]) |}]

let%expect_test _ =
  let j =
    Yojson.Safe.from_string
      {|[ "Var",            {
          "id": "E",
          "index": 0,
          "upb": [ "Class", "java.lang.Object", [] ],
          "lwb": null
        }          ]
|}
  in
  Format.printf "%s\n%!" (Yojson.Safe.show j);
  [%expect
    {|
    `List ([`String ("Var");
             `Assoc ([("id", `String ("E")); ("index", `Int (0));
                       ("upb",
                        `List ([`String ("Class"); `String ("java.lang.Object");
                                 `List ([])]));
                       ("lwb", `Null)])
             ]) |}];
  let () =
    match param_of_yojson j with
    | exception Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (exc, j) ->
        Format.printf "%s\n%s\n%!" (Printexc.to_string exc) (Yojson.Safe.show j)
    | _ -> Format.printf "OK\n%!"
  in
  [%expect
    {|
    	 param_of_yojson: "[\"Var\",{\"id\":\"E\",\"index\":0,\"upb\":[\"Class\",\"java.lang.Object\",[]],\"lwb\":null}]"
    OK
    	 |}]

let%expect_test _ =
  let j =
    Yojson.Safe.from_string
      {|
  [
           "Var",
           {
              "id": "id",
              "upb": [ "Class", "java.lang.Object", [] ],
              "lwb": null
            }
        ]
|}
  in
  Format.printf "%s\n%!" (Yojson.Safe.show j);
  [%expect
    {|
    `List ([`String ("Var");
             `Assoc ([("id", `String ("id"));
                       ("upb",
                        `List ([`String ("Class"); `String ("java.lang.Object");
                                 `List ([])]));
                       ("lwb", `Null)])
             ]) |}];
  let () =
    match jtype_of_yojson j with
    | exception Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (exc, j) ->
        Format.printf "%s\n%s\n%!" (Printexc.to_string exc) (Yojson.Safe.show j)
    | _ -> Format.printf "OK\n%!"
  in
  [%expect
    {|
    Failure("CT_of_json.ml.jtype_of_yojson: the following record elements were undefined: index")
    `List ([`String ("Var");
             `Assoc ([("id", `String ("id"));
                       ("upb",
                        `List ([`String ("Class"); `String ("java.lang.Object");
                                 `List ([])]));
                       ("lwb", `Null)])
             ]) |}]

let%expect_test _ =
  let table =
    [
      I { iname = "A"; iparams = []; isupers = [] };
      I { iname = "B"; iparams = []; isupers = [ Interface ("A", []) ] };
      C
        {
          cname = "D";
          supers = [ Class ("Object", []) ];
          super = Some (Class ("Object", []));
          params = [];
        };
      C
        {
          cname = "E";
          supers = [ Class ("A", []); Class ("D", [ Class ("B", []) ]) ];
          super = Some (Class ("Object", []));
          params = [];
        };
    ]
  in
  Format.printf "%a\n%!"
    (Yojson.Safe.pretty_print ~std:true)
    (yojson_of_table table);

  [%expect
    {|
    [
      [ "I", { "iname": "A", "iparams": [], "isupers": [] } ],
      [
        "I",
        { "iname": "B", "iparams": [], "isupers": [ [ "Interface", "A", [] ] ] }
      ],
      [
        "C",
        {
          "cname": "D",
          "params": [],
          "super": [ "Class", "Object", [] ],
          "supers": [ [ "Class", "Object", [] ] ]
        }
      ],
      [
        "C",
        {
          "cname": "E",
          "params": [],
          "super": [ "Class", "Object", [] ],
          "supers": [
            [ "Class", "A", [] ],
            [ "Class", "D", [ [ "Type", [ "Class", "B", [] ] ] ] ]
          ]
        }
      ]
    ] |}]
