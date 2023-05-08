type polarity = JGS.polarity = Extends | Super
[@@deriving yojson_of, of_yojson]

let%expect_test _ =
  Format.printf "%a\n%!"
    (Yojson.Safe.pretty_print ~std:true)
    (yojson_of_polarity Extends);
  [%expect {| [ "Extends" ] |}]

type class_id = string [@@deriving yojson_of, of_yojson]

type 'jtype targ = Type of 'jtype | Wildcard of (polarity * 'jtype) option
[@@deriving yojson_of, of_yojson]

let targ_of_yojson (from_arg : Yojson.Safe.t -> 'jtype) (j : Yojson.Safe.t) =
  match j with
  | `List (`String "Var" :: _) ->
      (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
      Type (from_arg j)
  | _ -> targ_of_yojson from_arg j

type jtype =
  (* array type *)
  | Array of jtype
  (* class type *)
  | Class of (* NEED TO RETURN: id *) class_id * jtype targ list
  (* interface type *)
  | Interface of (* NEED TO RETURN: id *) class_id * jtype targ list
  (* type variable: *)
  | Var of {
      (* 1. identity *)
      id : (* NEED TO RETURN: id *) class_id;
      index : int;
      (* 3. upper bound *)
      upb : jtype;
      (* 4. lower bound *)
      lwb : jtype option; [@yojson.option]
    }
    (* null type *)
  | Null
[@@deriving yojson_of, of_yojson]

let var ?lwb id upb = Var { id; upb; lwb; index = 0 }

let%expect_test _ =
  let t = var "XXX" (Class ("Object", [])) in
  let j = yojson_of_jtype t in
  Format.printf "%s\n%a\n%!" (Yojson.Safe.show j)
    (Yojson.Safe.pretty_print ~std:true)
    j;
  [%expect
    {|
    `List ([`String ("Var");
             `Assoc ([("id", `String ("XXX")); ("index", `Int (0));
                       ("upb",
                        `List ([`String ("Class"); `String ("Object"); `List (
                                 [])]))
                       ])
             ])

    [
      "Var", { "id": "XXX", "index": 0, "upb": [ "Class", "Object", [] ] }
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
    match targ_of_yojson jtype_of_yojson j with
    | exception Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (exc, j) ->
        Format.printf "%s\n%s\n%!" (Printexc.to_string exc) (Yojson.Safe.show j)
    | _ -> Format.printf "OK\n%!"
  in
  [%expect {|
    OK |}]

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
    match targ_of_yojson jtype_of_yojson j with
    | exception Ppx_yojson_conv_lib__Yojson_conv.Of_yojson_error (exc, j) ->
        Format.printf "%s\n%s\n%!" (Printexc.to_string exc) (Yojson.Safe.show j)
    | _ -> Format.printf "OK\n%!"
  in
  [%expect
    {|
    Failure("CT_of_json.ml.jtype_of_yojson: unexpected variant constructor")
    `Null |}]

type param = { pname : class_id; p_upper : jtype list }
[@@deriving yojson_of, of_yojson]

let param_of_yojson j =
  match jtype_of_yojson j with
  | Var { id; upb; index = _ } -> { pname = id; p_upper = [ upb ] }
  | (exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error _) | _ ->
      param_of_yojson j

let make_param ?(up = []) pname = { pname; p_upper = up }

type cdecl = {
  cname : class_id;
  (* type parameters *)
  params : (* NEED TO RETURN: params *) param list;
  (* supeclass *)
  super : jtype option; [@yojson.option]
  (* superinterfaces *)
  supers : jtype list;
}
[@@deriving yojson_of, of_yojson]

type idecl = {
  iname : class_id;
  (* type parameters *)
  iparams : (* NEED TO RETURN: params *) param list;
  (* superinterfaces *)
  isupers : jtype list;
}
[@@deriving yojson_of, of_yojson]

type decl = C of cdecl | I of idecl [@@deriving yojson_of, of_yojson]
type table = decl list [@@deriving yojson_of, of_yojson]

let make_c cname ~params ?sup supers = C { cname; params; super = sup; supers }

type query = {
  table : table;
  upper_bounds : jtype list; [@default []]
  lower_bounds : jtype list; [@default []]
  neg_upper_bounds : jtype list; [@default []]
  neg_lower_bounds : jtype list; [@default []]
}
[@@deriving yojson_of, of_yojson]

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
          supers = [ Class ("A", []); Class ("D", [ Type (Class ("B", [])) ]) ];
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

let make_sample_ct () =
  let open MutableTypeTable in
  (module SampleCT () : SAMPLE_CLASSTABLE)

let failwiths fmt = Format.kasprintf failwith fmt

type config = { mutable verbose : int }

let config = { verbose = 0 }
let set_verbose () = config.verbose <- 1

let log fmt =
  if config.verbose > 0 then Format.kasprintf (Format.eprintf "%s\n%!") fmt
  else Format.ikfprintf (fun _ -> ()) Format.err_formatter fmt

[@@@ocaml.warnerror "-26"]

module G = struct
  include Graph.Imperative.Digraph.Concrete (struct
    type t = string

    let hash = Stdlib.Hashtbl.hash
    let compare = (Stdlib.compare : string -> string -> int)
    let equal = String.equal
  end)

  let add_edge g start fin =
    if not (String.equal start fin) then
      let () = log "Add edge: %s -> %s" start fin in
      add_edge g start fin
end

module TopSort = Graph.Topological.Make (G)

let populate_graph on_decl table =
  (* log "%s %d" __FILE__ __LINE__; *)
  let iter () =
    let g = G.create ~size:(List.length table) () in
    let decl_of_name = Hashtbl.create (List.length table) in
    let wrap iname isupers =
      (* log "wrap '%s' with %d supers " iname (List.length isupers); *)
      isupers
      |> Stdlib.List.iter (function
           | Interface (name, _) | Class (name, _) -> G.add_edge g name iname
           | _ ->
               Format.eprintf "Missgin case?\n%!";
               ())
    in
    let rec traverse_typ dest = function
      | Class (name, _) | Interface (name, _) -> G.add_edge g name dest
      | Var { upb; _ } -> traverse_typ dest upb
      | _ -> ()
    in
    let traverse_param dest { p_upper; _ } =
      p_upper |> Stdlib.List.iter (traverse_typ dest)
    in
    table
    |> Stdlib.List.iter (function
         | I { iname; isupers; iparams } as d ->
             (* log "%s %d" __FILE__ __LINE__; *)
             G.add_vertex g iname;
             Hashtbl.add decl_of_name iname d;
             iparams |> Stdlib.List.iter (traverse_param iname);
             wrap iname isupers
         | C { cname; supers; super; params } as d ->
             (* log "%s %d" __FILE__ __LINE__; *)
             G.add_vertex g cname;
             Hashtbl.add decl_of_name cname d;
             params |> Stdlib.List.iter (traverse_param cname);
             Stdlib.Option.iter (fun x -> wrap cname [ x ]) super;
             wrap cname supers);

    (* log "Graph hash %d vertexes and %d edges. %s %d" (G.nb_vertex g)
       (G.nb_edges g) __FILE__ __LINE__; *)
    g
    |> TopSort.iter (fun name ->
           (* log "%s %d" __FILE__ __LINE__; *)
           (* log "Running on '%s'" name; *)
           match Hashtbl.find decl_of_name name with
           | exception Not_found ->
               Format.eprintf
                 "  The type %S is not found (Bad JSON?). Ignored.\n%!" name
           | x -> on_decl x)
    (* log "%s %d" __FILE__ __LINE__ *)
  in

  iter ()

let make_classtable table =
  let classes : (string, int) Hashtbl.t =
    Hashtbl.create (List.length table + 1)
  in
  let ifaces : (string, int) Hashtbl.t =
    Hashtbl.create (List.length table + 1)
  in
  let params_hash : (string, int) Hashtbl.t =
    Hashtbl.create (List.length table + 1)
  in
  let cur_name = ref ("", -42) in
  let is_current name = name = fst !cur_name in

  let ((module CT : MutableTypeTable.SAMPLE_CLASSTABLE) as ct) =
    make_sample_ct ()
  in

  let () =
    match CT.object_t with
    | Class (class_id, _) -> Hashtbl.add classes "java.lang.Object" class_id
    | _ -> assert false
  in

  let id_of_name name =
    if name = fst !cur_name then `Defined (snd !cur_name)
    else
      match Hashtbl.find classes name with
      | x -> `Defined x
      | exception Not_found -> (
          match Hashtbl.find ifaces name with
          | x -> `Defined x
          | exception Not_found -> (
              match Hashtbl.find params_hash name with
              | x -> `Param x
              | exception Not_found ->
                  failwiths "The type names '%s' is not found" name))
  in

  let rec on_decl = function
    | C { cname; params; super; supers } ->
        log "Running on class %s..." cname;
        Hashtbl.clear params_hash;
        Stdlib.List.iteri
          (fun i { pname; _ } -> Hashtbl.add params_hash pname i)
          params;
        let cid =
          CT.make_class_fix
            ~params:(fun cur_id ->
              cur_name := (cname, cur_id);
              Stdlib.List.iteri
                (fun i p -> Hashtbl.add params_hash p.pname i)
                params;
              List.mapi on_param params)
            (fun _ ->
              match super with
              | None -> CT.object_t
              | Some super -> on_typ super)
            (fun _ -> List.map on_typ supers)
        in
        log "Adding a class %s with id  = %d" cname cid;
        Hashtbl.add classes cname cid
    | I { iname; iparams; isupers } ->
        if true then (
          log "Running on interface %s..." iname;
          Hashtbl.clear params_hash;
          let iid =
            CT.make_interface_fix
              (fun cur_id ->
                cur_name := (iname, cur_id);
                Stdlib.List.iteri
                  (fun i p -> Hashtbl.add params_hash p.pname i)
                  iparams;
                List.mapi on_param iparams)
              (fun _ -> List.map on_typ isupers)
          in
          log "Adding an interface %s with id = %d" iname iid;
          Hashtbl.add ifaces iname iid)
        else
          Format.eprintf
            "The interface '%s' seems to be not declared. Skipping.\n%!" iname
  and on_param idx { pname; p_upper } : JGS.jtype =
    let upper_bounds = p_upper |> List.map on_typ in
    (* TODO: read  again if params could have upper bound *)
    CT.object_t
    (* assert false *)
  and on_arg : _ -> JGS.jtype JGS.targ = function
    | Type t -> JGS.Type (on_typ t)
    | Wildcard None -> JGS.Wildcard None
    | Wildcard (Some (kind, typ)) -> JGS.Wildcard (Some (kind, on_typ typ))
  and on_typ = function
    | Class (name, args) when is_current name ->
        JGS.Class (snd !cur_name, List.map on_arg args)
    | Interface (name, args) when is_current name ->
        JGS.Class (snd !cur_name, List.map on_arg args)
    | Class (name, args) -> (
        match Hashtbl.find params_hash name with
        | param_id ->
            let () =
              if args <> [] then
                failwith "Type variables with arguments do not happen in Java"
            in
            CT.make_tvar param_id CT.object_t
        | exception Not_found -> (
            match Hashtbl.find classes name with
            | id -> JGS.Class (id, List.map on_arg args)
            | exception Not_found ->
                failwith (Printf.sprintf "Class '%s' was not yet declared" name)
            ))
    | Interface (name, args) -> (
        match Hashtbl.find params_hash name with
        | param_id ->
            let () =
              if args <> [] then
                failwith "Type variables with arguments do not happen in Java"
            in
            CT.make_tvar param_id CT.object_t
        | exception Not_found -> (
            match Hashtbl.find ifaces name with
            | id -> JGS.Interface (id, List.map on_arg args)
            | exception Not_found ->
                failwith
                  (Printf.sprintf "Interface '%s' was not yet declared" name)))
    | Var { id; upb; lwb; index = _ } ->
        (* log "Looking for param %s " id; *)
        let class_id = Hashtbl.find params_hash id in
        JGS.(
          Var
            {
              id = class_id;
              index = class_id;
              upb = on_typ upb;
              lwb = Stdlib.Option.map on_typ lwb;
            })
    | _ -> assert false
  in
  populate_graph on_decl table;
  ( ct,
    fun name ->
      match Hashtbl.find classes name with
      | cid -> cid
      | exception Not_found -> (
          match Hashtbl.find ifaces name with
          | id -> id
          | exception Not_found ->
              failwiths "Can't find '%s' neither in classes not in interfaces"
                name) )

let make_query j =
  let { table; neg_upper_bounds; neg_lower_bounds; upper_bounds; lower_bounds }
      =
    query_of_yojson j
  in
  let ((module CT) as ct), id_by_name = make_classtable table in
  let rec on_typ : _ -> JGS.jtype = function
    | Class (name, args) -> (
        match id_by_name name with
        | cid -> JGS.Class (cid, args |> List.map (fun arg -> on_arg arg))
        | exception Not_found -> failwiths "Can't find class name '%s'" name)
    | _ -> assert false
  and on_arg : _ -> _ JGS.targ = function
    (* TODO: wildcards are not used, fix that later *)
    | Type t -> JGS.Type (on_typ t)
    | Wildcard None -> JGS.Wildcard None
    | Wildcard (Some (kind, typ)) -> JGS.Wildcard (Some (kind, on_typ typ))
  in

  if neg_lower_bounds <> [] || neg_upper_bounds <> [] then
    Format.eprintf "Negatives bound are not yet supported\n%!";
  let goal is_subtype targ_inj answer_typ =
    let open OCanren in
    let init = success in

    let init =
      List.fold_left
        (fun acc b ->
          acc &&& is_subtype answer_typ (targ_inj (on_typ b)) !!true)
        init upper_bounds
    in
    List.fold_left
      (fun acc b -> acc &&& is_subtype (targ_inj (on_typ b)) answer_typ !!true)
      init lower_bounds
  in
  (ct, goal)
