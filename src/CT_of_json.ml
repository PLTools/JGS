open Stdlib

let failwiths fmt = Format.kasprintf failwith fmt

type polarity = JGS.polarity = Extends | Super
[@@deriving yojson_of, of_yojson]

type class_id = string [@@deriving yojson_of, of_yojson]
(*
   let targ_of_yojson (from_arg : Yojson.Safe.t -> 'jtype) (j : Yojson.Safe.t) =
     (* Format.printf "\ttarg_of_yosjon: %S\n%!" (Yojson.Safe.to_string j); *)
     match j with
     | `List (`String "Var" :: _) ->
         (* Format.printf "%s %d\n%!" __FILE__ __LINE__; *)
         Type (from_arg j)
     | `List
         [
           `String "Wildcard";
           `Assoc [ ("first", `String "Extends"); ("seconds", typ) ];
         ] ->
         Wildcard (Some (Extends, from_arg typ))
     | `List [] ->
         (* Format.printf "\t%s %d\n%!" __FILE__ __LINE__; *)
         targ_of_yojson from_arg j
     | _ -> targ_of_yojson from_arg j *)

type jtype =
  (* array type *)
  | Array of jtype
  (* class type *)
  | Class of (* NEED TO RETURN: id *) class_id * jtype list
  (* interface type *)
  | Interface of (* NEED TO RETURN: id *) class_id * jtype list
  (* type variable: *)
  | Var of {
      (* 1. identity *)
      id : (* NEED TO RETURN: id *) class_id;
      index : int;
      (* 3. upper bound *)
      upb : jtype;
      (* 4. lower bound *)
      lwb : jtype option;
    }
    (* null type *)
  | Null
  | Wildcard of (polarity * jtype) option
  | Intersect of jtype list
  | Primitive of string
[@@deriving yojson_of, of_yojson]

let jtype_of_yojson =
  let rec helper j : jtype =
    (* Format.printf "\t@[jtype_of_yojson helper:@ %s@]\n\n%!"
       (Yojson.Safe.pretty_to_string j); *)
    match j with
    | `List [ `String "Class"; `String name; `List xs ] ->
        Class (name, List.map helper xs)
    | `List [ `String "Interface"; `String name; `List xs ] ->
        Interface (name, List.map helper xs)
    | `List [ `String "Type"; lst ] -> helper lst
    | `List [ `String "Array"; t ] -> Array (helper t)
    | `List
        [
          `String "Wildcard";
          `Assoc [ ("first", `String "Extends"); ("second", t) ];
        ] ->
        Wildcard (Some (Extends, helper t))
    | `List
        [
          `String "Wildcard";
          `Assoc [ ("first", `String "Super"); ("second", t) ];
        ] ->
        Wildcard (Some (Super, helper t))
    | `List
        [
          `String "Var";
          `Assoc
            [
              ("id", `String id);
              ("index", `Int index);
              ("upb", upb);
              ("lwb", `Null);
            ];
        ] ->
        Var { id; index; upb = helper upb; lwb = None }
    | `List [ `String "Intersect"; `List xs ] -> Intersect (List.map helper xs)
    | t -> jtype_of_yojson t
  in
  helper

let var ?lwb id upb = Var { id; upb; lwb; index = 0 }

type param = { pname : class_id; p_upper : jtype list }
[@@deriving yojson_of, of_yojson]

let param_of_yojson j =
  (* Format.printf "\t param_of_yojson: %S\n%!" (Yojson.Safe.to_string j); *)
  match jtype_of_yojson j with
  | Var { id; upb; index = _; _ } -> { pname = id; p_upper = [ upb ] }
  | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error _ ->
      Format.printf "Fallback: it's not  a type\n%!";
      param_of_yojson j
  | _ ->
      Format.printf "Fallback from type to param\n%!";
      param_of_yojson j

let make_param ?(up = []) pname = { pname; p_upper = up }

type cdecl = {
  cname : class_id;
  (* type parameters *)
  params : (* NEED TO RETURN: params *) param list;
  (* supeclass *)
  super : jtype option;
  (* superinterfaces *)
  supers : jtype list;
}
[@@deriving yojson_of, of_yojson]

let cdecl_of_yojson j =
  (* Format.printf "\t cdecl_of_yojson: %S\n%!" (Yojson.Safe.to_string j); *)
  cdecl_of_yojson j

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

module SS = Set.Make (String)

let collect_used_typenames =
  let rec helper acc = function
    | Interface (_, args) | Class (_, args) -> List.fold_left helper acc args
    | Array t -> helper acc t
    | Primitive _ | Null -> acc
    | Wildcard None -> acc
    | Wildcard (Some (_, t)) -> helper acc t
    | Intersect xs -> List.fold_left helper acc xs
    | Var { upb; lwb; id; _ } ->
        let acc = SS.add id acc in
        let acc = match lwb with Some t -> helper acc t | None -> acc in
        helper acc upb
  in

  helper SS.empty

let collect_varnames =
  let rec helper acc = function
    | Interface (name, args) | Class (name, args) ->
        List.fold_left helper (SS.add name acc) args
    | Array t -> helper acc t
    | Primitive _ | Null -> acc
    | Wildcard None -> acc
    | Wildcard (Some (_, t)) -> helper acc t
    | Intersect xs -> List.fold_left helper acc xs
    | Var { upb; lwb; id } ->
        SS.union
          (match lwb with None -> SS.empty | Some t -> helper SS.empty t)
          (helper (SS.add id acc) upb)
  in

  helper SS.empty

  let make_sample_ct () =
    let open MutableTypeTable in
    (module SampleCT () : SAMPLE_CLASSTABLE)

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
      (* let () = log "Add edge: %s -> %s" start fin in *)
      add_edge g start fin
end

module TopSort = Graph.Topological.Make (G)

let populate_graph on_decl table =
  (* log "%s %d" __FILE__ __LINE__; *)
  let iter () =
    let g = G.create ~size:(List.length table) () in
    let decl_of_name = Hashtbl.create (List.length table) in

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
             G.add_vertex g iname;
             Hashtbl.add decl_of_name iname d;
             iparams |> Stdlib.List.iter (traverse_param iname);
             let used_typenames =
               List.fold_left
                 (fun acc p -> SS.union acc (collect_used_typenames p))
                 SS.empty isupers
             in
             SS.iter (fun name -> G.add_edge g name iname) used_typenames
         | C { cname; supers; super; params } as d ->
             (* log "Adding a class %s to graph. %s %d" cname __FILE__ __LINE__; *)
             G.add_vertex g cname;
             assert (
               match Hashtbl.find decl_of_name cname with
               | exception Not_found -> true
               | C
                   {
                     cname = "org.intellij.lang.annotations.PrintFormatPattern";
                     _;
                   }
               | C { cname = "org.intellij.lang.annotations.JdkConstants"; _ }
                 ->
                   true
               | _ ->
                   Format.eprintf "Already present: '%s'\n%!" cname;
                   false);
             Hashtbl.add decl_of_name cname d;
             params |> Stdlib.List.iter (traverse_param cname);
             (* TODO: should we lookup references in params? *)
             let used_typenames =
               let acc =
                 Stdlib.Option.fold ~none:SS.empty ~some:collect_used_typenames
                   super
               in
               List.fold_left
                 (fun acc p -> SS.union acc (collect_used_typenames p))
                 acc supers
             in
             SS.iter (fun name -> G.add_edge g name cname) used_typenames);

    (* log "Graph hash %d vertexes and %d edges. %s %d" (G.nb_vertex g)
       (G.nb_edges g) __FILE__ __LINE__; *)
    TopSort.iter
      (fun name ->
        match Hashtbl.find decl_of_name name with
        | exception Not_found ->
            Format.eprintf
              "  The type %S is not found (Bad JSON?). Ignored.\n%!" name
        | x -> on_decl x)
      g
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

  let return x = Some x in
  let unwrap scru ?(on_error = fun () -> failwith "Can't recover from error") sk
      =
    match scru with Some t -> sk t | None -> on_error ()
  in
  let rec on_decl = function
    | C { cname; params; super; supers } ->
        (* log "Running on class %s..." cname; *)
        Hashtbl.clear params_hash;
        Stdlib.List.iteri
          (fun i { pname; _ } -> Hashtbl.add params_hash pname i)
          params;
        let cid =
          CT.make_class_fix
            ~params:(fun cur_id ->
              cur_name := (cname, cur_id);
              (* Stdlib.List.iteri
                 (fun i p -> Hashtbl.add params_hash p.pname i)
                 params; *)
              List.mapi on_param params)
            (fun cur_id ->
              cur_name := (cname, cur_id);
              match super with
              | None -> CT.object_t
              | Some super ->
                  unwrap (on_typ super) Fun.id ~on_error:(fun () -> CT.object_t))
            (fun cur_id ->
              cur_name := (cname, cur_id);
              Stdlib.List.filter_map on_typ supers)
        in
        (* log "Adding a class %s with id  = %d" cname cid; *)
        Hashtbl.add classes cname cid
    | I { iname; iparams; isupers } ->
        if true then (
          (* log "Running on interface %s..." iname; *)
          Hashtbl.clear params_hash;
          Stdlib.List.iteri
            (fun i { pname; _ } -> Hashtbl.add params_hash pname i)
            iparams;
          let iid =
            CT.make_interface_fix
              (fun cur_id ->
                cur_name := (iname, cur_id);
                (* Stdlib.List.iteri
                   (fun i p -> Hashtbl.add params_hash p.pname i)
                   iparams; *)
                List.mapi on_param iparams)
              (fun cur_id ->
                cur_name := (iname, cur_id);
                Stdlib.List.filter_map on_typ isupers)
          in
          (* log "Adding an interface %s with id = %d" iname iid; *)
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
    | Wildcard None -> JGS.Wildcard None
    | Wildcard (Some (kind, typ)) ->
        unwrap (on_typ typ) (fun x -> JGS.Wildcard (Some (kind, x)))
    | t -> (
        match on_typ t with
        | Some t -> JGS.Type t
        | None ->
            failwiths "Can't recover from error in on_typ: \n%a\n"
              Yojson.Safe.pp (yojson_of_jtype t))
  and on_typ : _ -> JGS.jtype option = function
    | Class (name, args) when is_current name ->
        return @@ JGS.Class (snd !cur_name, List.map on_arg args)
    | Interface (name, args) when is_current name ->
        return @@ JGS.Class (snd !cur_name, List.map on_arg args)
    | Class (name, args) -> (
        match Hashtbl.find params_hash name with
        | param_id ->
            let () =
              if args <> [] then
                failwith "Type variables with arguments do not happen in Java"
            in
            return @@ CT.make_tvar param_id CT.object_t
        | exception Not_found -> (
            match Hashtbl.find classes name with
            | id -> return @@ JGS.Class (id, List.map on_arg args)
            | exception Not_found ->
                Format.eprintf
                  "   The name %S is not class or parameter. Substituting \
                   object. (cur_name = %s)\n\n"
                  name (fst !cur_name);
                Some CT.object_t))
    | Interface (name, args) -> (
        match Hashtbl.find params_hash name with
        | param_id ->
            let () =
              if args <> [] then
                failwith "Type variables with arguments do not happen in Java"
            in
            return @@ CT.make_tvar param_id CT.object_t
        | exception Not_found -> (
            match Hashtbl.find ifaces name with
            | id -> return @@ JGS.Interface (id, List.map on_arg args)
            | exception Not_found ->
                Format.eprintf
                  "  The name %S is not interface or parameter. Substituting \
                   object  (cur_name = %s)\n"
                  name (fst !cur_name);
                Some CT.object_t))
    | Array typ -> Option.map CT.array_t (on_typ typ)
    | Var { id; upb; lwb; index = _ } -> (
        (* log "Looking for param %s " id; *)
        match Hashtbl.find params_hash id with
        | exception Not_found ->
            Format.eprintf "Possibly undeclared param '%s' in the class '%s'\n"
              id (fst !cur_name);
            return CT.object_t
        | class_id ->
            return
            @@ JGS.(
                 Var
                   {
                     id = class_id;
                     index = class_id;
                     upb = unwrap (on_typ upb) Fun.id (* Fix HERE *);
                     lwb =
                       Option.bind lwb (fun x -> unwrap (on_typ x) Option.some);
                   }))
    | Intersect _ ->
        Format.eprintf "Intersections are not supported. Substituting Object\n";
        return CT.object_t
    | Primitive s -> return (CT.primitive_t s)
    | t ->
        Format.eprintf "%a\n%!" Yojson.Safe.pp (yojson_of_jtype t);
        failwith "unsuported case"
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
        | cid -> JGS.Class (cid, List.map on_arg args)
        | exception Not_found -> failwiths "Can't find class name '%s'" name)
    | Interface (name, args) -> (
        match id_by_name name with
        | cid -> JGS.Interface (cid, List.map on_arg args)
        | exception Not_found -> failwiths "Can't find class name '%s'" name)
    | Var { id; upb; lwb; _ } ->
        JGS.Var
          { id = 0; upb = on_typ upb; lwb = Option.map on_typ lwb; index = -42 }
    | t ->
        Format.eprintf "%a\n%!" Yojson.Safe.pp (yojson_of_jtype t);
        failwith "unsuported case"
  and on_arg : _ -> _ JGS.targ = function
    (* TODO: wildcards are not used, fix that later *)
    | Wildcard None -> JGS.Wildcard None
    | Wildcard (Some (kind, typ)) -> JGS.Wildcard (Some (kind, on_typ typ))
    | t -> JGS.Type (on_typ t)
  in

  if neg_lower_bounds <> [] || neg_upper_bounds <> [] then
    Format.eprintf "Negatives bound are not yet supported\n%!";

  (*

         let relationalize_typ acc typ = function
               | Class (name, args) -> (
             match id_by_name name with
             | cid -> JGS.Class (cid, List.map on_arg args)
             | exception Not_found -> failwiths "Can't find class name '%s'" name)
         | Interface (name, args) -> (
             match id_by_name name with
             | cid -> JGS.Interface (cid, List.map on_arg args)
             | exception Not_found -> failwiths "Can't find class name '%s'" name)
         | Var { id; upb; lwb; _ } ->
             JGS.Var
               { id = 0; upb = on_typ upb; lwb = Option.map on_typ lwb; index = -42 }
         | t ->
             Format.eprintf "%a\n%!" Yojson.Safe.pp (yojson_of_jtype t);
             failwith "unsuported case"
         in
         let relationalize_arg acc arg =
     | Wildcard None -> JGS.Wildcard None
         | Wildcard (Some (kind, typ)) -> JGS.Wildcard (Some (kind, on_typ typ))
         | t -> JGS.Type (on_typ t)
         in *)
  let varnames =
    let acc =
      List.fold_left
        (fun acc x -> SS.union acc (collect_used_typenames x))
        SS.empty upper_bounds
    in
    let acc =
      List.fold_left
        (fun acc x -> SS.union acc (collect_used_typenames x))
        acc lower_bounds
    in
    acc
  in

  let () =
    Format.printf "\nType variables mentioned in constraints: [";
    SS.to_seq varnames |> Seq.iter (Format.printf " %s ");
    Format.printf "]\n\n%!"
  in

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
