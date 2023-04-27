type polarity = JGS.polarity = Extends | Super
[@@deriving yojson_of, of_yojson]

type 'jtype targ = 'jtype JGS.targ =
  | Type of 'jtype
  | Wildcard of (polarity * 'jtype) option
[@@deriving yojson_of, of_yojson]

type class_id = string [@@deriving yojson_of, of_yojson]

type 'class_id jtype =
  (* array type *)
  | Array of 'class_id jtype
  (* class type *)
  | Class of (* NEED TO RETURN: id *) 'class_id * 'class_id jtype targ list
  (* interface type *)
  | Interface of (* NEED TO RETURN: id *) 'class_id * 'class_id jtype targ list
  (* type variable: *)
  | Var of {
      (* 1. identity *)
      id : (* NEED TO RETURN: id *) 'class_id;
      (* 2. index in declaration list *)
      index : 'class_id;
      (* 3. upper bound *)
      upb : 'class_id jtype;
      (* 4. lower bound *)
      lwb : 'class_id jtype option;
    }
    (* null type *)
  | Null
(* intersection type *)
(* | Intersect of 'class_id jtype list *)
[@@deriving yojson_of, of_yojson]

type cdecl = {
  cname : class_id;
  (* type parameters *)
  params : (* NEED TO RETURN: params *) class_id jtype list;
  (* supeclass *)
  super : class_id jtype;
  (* superinterfaces *)
  supers : class_id jtype list;
}
[@@deriving yojson_of, of_yojson]

type idecl = {
  iname : class_id;
  (* type parameters *)
  params : (* NEED TO RETURN: params *) class_id jtype list;
  (* superinterfaces *)
  supers : class_id jtype list;
}
[@@deriving yojson_of, of_yojson]

type decl = C of cdecl | I of idecl [@@deriving yojson_of, of_yojson]
type table = decl list [@@deriving yojson_of, of_yojson]

let%expect_test _ =
  Format.printf "%a\n%!"
    (Yojson.Safe.pretty_print ~std:true)
    (yojson_of_polarity Extends);
  [%expect {| [ "Extends" ] |}]

let%expect_test _ =
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
  Format.printf "%a\n%!"
    (Yojson.Safe.pretty_print ~std:true)
    (yojson_of_table table);

  [%expect {|
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
    ] |}]
