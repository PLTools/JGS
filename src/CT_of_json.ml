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

type param = { pname : class_id; p_upper : class_id jtype list }
[@@deriving yojson_of, of_yojson]

let make_param ?(up = []) pname = { pname; p_upper = up }

type cdecl = {
  cname : class_id;
  (* type parameters *)
  params : (* NEED TO RETURN: params *) param list;
  (* supeclass *)
  super : class_id jtype;
  (* superinterfaces *)
  supers : class_id jtype list;
}
[@@deriving yojson_of, of_yojson]

type idecl = {
  iname : class_id;
  (* type parameters *)
  iparams : (* NEED TO RETURN: params *) param list;
  (* superinterfaces *)
  isupers : class_id jtype list;
}
[@@deriving yojson_of, of_yojson]

type decl = C of cdecl | I of idecl [@@deriving yojson_of, of_yojson]
type table = decl list [@@deriving yojson_of, of_yojson]

type query = {
  table : table;
  upper_bounds : class_id jtype list;
  lower_bounds : class_id jtype list;
  neg_upper_bounds : class_id jtype list;
  neg_lower_bounds : class_id jtype list;
}
[@@deriving yojson_of, of_yojson]

let%expect_test _ =
  Format.printf "%a\n%!"
    (Yojson.Safe.pretty_print ~std:true)
    (yojson_of_polarity Extends);
  [%expect {| [ "Extends" ] |}]

let%expect_test _ =
  let table =
    [
      I { iname = "A"; iparams = []; isupers = [] };
      I { iname = "B"; iparams = []; isupers = [ Interface ("A", []) ] };
      C
        {
          cname = "D";
          supers = [ Class ("Object", []) ];
          super = Class ("Object", []);
          params = [];
        };
      C
        {
          cname = "E";
          supers = [ Class ("A", []); Class ("D", [ Type (Class ("B", [])) ]) ];
          super = Class ("Object", []);
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

let log fmt =
  if true then Format.kasprintf (Format.eprintf "%s\n%!") fmt
  else Format.ikfprintf (fun _ppf -> ()) Format.err_formatter fmt

[@@@ocaml.warnerror "-26"]

let make_classtable table =
  let classes = Hashtbl.create (List.length table + 1) in
  let ifaces = Hashtbl.create (List.length table + 1) in

  let ((module CT : MutableTypeTable.SAMPLE_CLASSTABLE) as ct) =
    make_sample_ct ()
  in

  let () =
    match CT.object_t with
    | Class (class_id, _) -> Hashtbl.add classes "Object" class_id
    | _ -> assert false
  in

  let prepare_lookup cur_id cur_name : _ =
   fun name -> if name = cur_name then cur_id else Hashtbl.find classes name
   (* TODO: Lookup for interfaces too  *)
  in
  let rec on_decl = function
    | C { cname; params; super; supers } ->
        let params_hash = Hashtbl.create (List.length params) in
        let cid =
          CT.make_class_fix
            ~params:(fun cur_id ->
              List.mapi
                (fun i p ->
                  let typ = on_param (prepare_lookup cur_id cname) i p in
                  Hashtbl.add params_hash p.pname typ;
                  typ)
                params)
            (fun _ -> on_typ super)
            (fun _ -> List.map on_typ supers)
        in
        log "Adding a class %s with id  = %d" cname cid;
        Hashtbl.add classes cname cid
    | I { iname; iparams; isupers } ->
        let iid =
          CT.make_interface_fix
            (fun cur_id ->
              List.mapi (on_param (prepare_lookup cur_id iname)) iparams)
            (fun _ -> List.map on_typ isupers)
        in
        log "Adding an interface %s with id = %d" iname iid;
        Hashtbl.add ifaces iname iid
  and on_param id_of_name idx { pname; p_upper } =
    let upper_bounds = p_upper |> List.map on_typ in
    (* TODO: read  again if params could have upper bound *)
    CT.object_t
    (* assert false *)
  and on_arg = function Type _ -> assert false | Wildcard _ -> assert false
  and on_typ = function
    | Class (name, args) -> (
        match Hashtbl.find classes name with
        | id -> JGS.Class (id, List.map on_arg args)
        | exception Not_found ->
            failwith (Printf.sprintf "Class '%s' was not yet declared" name))
    | Interface (name, args) -> (
        match Hashtbl.find ifaces name with
        | id -> JGS.Interface (id, List.map on_arg args)
        | exception Not_found ->
            failwith (Printf.sprintf "Interface '%s' was not yet declared" name)
        )
    | _ -> assert false
  in

  table |> Stdlib.List.iter on_decl;
  ( ct,
    fun name ->
      match Hashtbl.find classes name with
      | cid -> cid
      | exception Not_found -> (
          match Hashtbl.find ifaces name with
          | id -> id
          | exception Not_found ->
              failwith "Can't find '%s' neither in classes not in interfaces")
  )

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
    (* -> failwith "Wildcards are not yet implemented" *)
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
