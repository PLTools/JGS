open Stdlib

let failwiths fmt = Format.kasprintf failwith fmt

type polarity = JGS.polarity = Extends | Super
[@@deriving yojson_of, of_yojson]

type class_id = string [@@deriving yojson_of, of_yojson]

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

let rec pp_jtype ppf = function
  | Array t -> Format.fprintf ppf "Array<%a>" pp_jtype t
  | Null -> Format.fprintf ppf "null"
  | Wildcard None -> Format.fprintf ppf "?"
  | Wildcard (Some (Super, t)) -> Format.fprintf ppf "? super %a" pp_jtype t
  | Wildcard (Some (Extends, t)) -> Format.fprintf ppf "? extends %a" pp_jtype t
  | Interface (name, args) | Class (name, args) ->
      Format.fprintf ppf "%s (%a)" name
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf " ")
           pp_jtype)
        args
  | Var { id; lwb = None; upb; index = _ } ->
      Format.fprintf ppf "(%s <: %a)" id pp_jtype upb
  | Var { id; lwb = Some lwb; upb; index = _ } ->
      Format.fprintf ppf "(%a <: %s <: %a)" pp_jtype lwb id pp_jtype upb
  | Primitive name -> Format.fprintf ppf "%s" name
  | Intersect xs ->
      Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.fprintf ppf " & ")
        pp_jtype ppf xs

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
      (* Format.printf "Fallback: it's not  a type\n%!"; *)
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
let make_i iname ~params isupers = I { iname; iparams = params; isupers }

type query = {
  table : table;
  upper_bounds : jtype list; [@default []]
  lower_bounds : jtype list; [@default []]
  neg_upper_bounds : jtype list; [@default []]
  neg_lower_bounds : jtype list; [@defau lt []]
}
[@@deriving yojson_of, of_yojson]

module SS = Set.Make (String)

let collect_used_typenames =
  let rec helper acc = function
    | Interface (name, args) | Class (name, args) ->
        List.fold_left helper (SS.add name acc) args
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
    | Interface (_, args) | Class (_, args) -> List.fold_left helper acc args
    | Array t -> helper acc t
    | Primitive _ | Null -> acc
    | Wildcard None -> acc
    | Wildcard (Some (_, t)) -> helper acc t
    | Intersect xs -> List.fold_left helper acc xs
    | Var { upb; lwb; id; _ } ->
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

  let add_vertex g v =
    (* let () = log "Add vertex: %s" v in *)
    add_vertex g v

  let add_edge g start fin =
    if not (String.equal start fin) then
      let () = log "Add edge: %s -> %s%!" start fin in
      add_edge g start fin
end

let add_parent_edges add_edge cname =
  let names = Str.split (Str.regexp "\\$") cname in
  match names with
  | [] -> failwith "should not happen"
  | [ _ ] -> ()
  | parents ->
      let _ =
        List.fold_left
          (fun acc name ->
            add_edge acc cname;
            acc ^ "$" ^ name)
          (List.hd parents) (List.tl parents)
      in
      ()

let%expect_test _ =
  add_parent_edges (Printf.printf "%s -> %s\n") "a$b$c";
  [%expect {|
    a -> a$b$c
    a$b -> a$b$c |}]

module TopSort = Graph.Topological.Make (G)

let populate_graph on_decl table =
  (* log "%s %d" __FILE__ __LINE__; *)
  let iter () =
    let g = G.create ~size:(List.length table) () in
    let decl_of_name = Hashtbl.create (List.length table) in

    let params_hash = Hashtbl.create 20 in
    let add_params =
      List.iter (fun { pname; _ } -> Hashtbl.add params_hash pname ())
    in
    let is_a_param name =
      try
        let () = Hashtbl.find params_hash name in
        true
      with Not_found -> false
    in
    let rec traverse_typ dest = function
      | Class (name, _) | Interface (name, _) ->
          if is_a_param name then () else G.add_edge g name dest
      | Var { upb; _ } -> traverse_typ dest upb
      | _ -> ()
    in

    let traverse_param dest { p_upper; _ } =
      Stdlib.List.iter (traverse_typ dest) p_upper
    in
    table
    |> Stdlib.List.iter (fun decl ->
           Hashtbl.clear params_hash;
           match decl with
           | I { iname; isupers; iparams } as d ->
               G.add_vertex g iname;
               Hashtbl.add decl_of_name iname d;
               add_params iparams;

               iparams |> Stdlib.List.iter (traverse_param iname);
               let used_typenames =
                 List.fold_left
                   (fun acc p -> SS.union acc (collect_used_typenames p))
                   SS.empty isupers
               in
               SS.iter
                 (fun name ->
                   if is_a_param name then () else G.add_edge g name iname)
                 used_typenames
           | C { cname; supers; super; params } as d ->
               log "Adding a class %s to graph. %s %d" cname __FILE__ __LINE__;
               G.add_vertex g cname;
               assert (
                 match Hashtbl.find decl_of_name cname with
                 | exception Not_found -> true
                 | C
                     {
                       cname =
                         "org.intellij.lang.annotations.PrintFormatPattern";
                       _;
                     }
                 | C { cname = "org.intellij.lang.annotations.JdkConstants"; _ }
                   ->
                     true
                 | _ ->
                     Format.eprintf "Already present: '%s'\n%!" cname;
                     false);
               Hashtbl.add decl_of_name cname d;
               add_params params;
               let () =
                 (* If it is an inner class, we should add dependencies to parents  *)
                 add_parent_edges (G.add_edge g) cname
               in

               params |> Stdlib.List.iter (traverse_param cname);
               (* TODO: should we lookup references in params? *)
               let used_typenames =
                 let acc =
                   Stdlib.Option.fold ~none:SS.empty
                     ~some:(fun t ->
                       (* Format.printf "Superclass = %s\n%!"
                          (Yojson.Safe.pretty_to_string (yojson_of_jtype t)); *)
                       collect_used_typenames t)
                     super
                 in
                 List.fold_left
                   (fun acc p -> SS.union acc (collect_used_typenames p))
                   acc supers
                 |> SS.elements
               in
               let __ () =
                 if
                   cname
                   = "net.bytebuddy.pool.TypePool$Default$AnnotationRegistrant$ForTypeVariable$WithIndex$DoubleIndexed"
                 then
                   let () =
                     Format.printf "Used typenames for %S: %s\n%!" cname
                       (String.concat " " used_typenames)
                   in
                   (* Format.printf "%s\n%!"
                      (Yojson.Safe.pretty_to_string (yojson_of_decl d)); *)
                   ()
               in
               List.iter
                 (fun name ->
                   if is_a_param name then () else G.add_edge g name cname)
                 used_typenames);

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

exception Id_not_found of int
exception Name_not_found of class_id

type var_info = { vi_id : int; vi_index : int }

let var_info ~id vi_index = { vi_id = id; vi_index }

let make_classtable table =
  let ((module CT : MutableTypeTable.SAMPLE_CLASSTABLE) as ct) =
    make_sample_ct ()
  in
  let classes : (string, int) Hashtbl.t =
    Hashtbl.create (List.length table + 1)
  in
  let ifaces : (string, int) Hashtbl.t =
    Hashtbl.create (List.length table + 1)
  in
  let params_hash : (string, var_info) Hashtbl.t =
    Hashtbl.create (List.length table + 1)
  in
  let name_of_id_hash = Hashtbl.create 10000 in
  let cur_name = ref ("", -42) in
  let is_current name = name = fst !cur_name in

  let () =
    match CT.object_t with
    | Class (id, _) ->
        Hashtbl.add classes "java.lang.Object" id;
        Hashtbl.add name_of_id_hash id "java.lang.Object"
    | _ -> assert false
  in
  let () =
    let full_name = "java.lang.Clonable" in
    match CT.cloneable_t with
    | Interface (id, _) ->
        Hashtbl.add classes full_name id;
        Hashtbl.add name_of_id_hash id full_name
    | _ -> assert false
  in
  let () =
    let full_name = "java.io.Serializable" in
    match CT.serializable_t with
    | Interface (id, _) ->
        Hashtbl.add classes full_name id;
        Hashtbl.add name_of_id_hash id full_name
    | _ -> assert false
  in
  let return x = Some x in
  let unwrap scru ?(on_error = fun () -> failwith "Can't recover from error") sk
      =
    match scru with Some t -> sk t | None -> on_error ()
  in
  let rec on_decl = function
    | C { cname = "java.lang.Object"; _ } -> ()
    | C { cname; params; super; supers } ->
        (* log "Running on class %s..." cname; *)
        Hashtbl.clear params_hash;
        (* Stdlib.List.iteri
           (fun i { pname; _ } ->
             let id = CT.new_var () in
             log "Creating new Var with name = %S, id = %d, index = %d" pname id
               i;
             Hashtbl.add params_hash pname (var_info ~id i))
           params; *)
        let cid =
          CT.make_class_fix
            ~params:(fun cur_id ->
              (* log "  make_class_fix %S. params" cname; *)
              cur_name := (cname, cur_id);
              List.mapi on_param params)
            (fun cur_id ->
              (* log "  make_class_fix %S. superclass" cname; *)
              cur_name := (cname, cur_id);
              match super with
              | None -> CT.object_t
              | Some super ->
                  unwrap (on_typ super) Fun.id ~on_error:(fun () -> CT.object_t))
            (fun cur_id ->
              (* log "  make_class_fix %S. superinterfaces" cname; *)
              cur_name := (cname, cur_id);
              Stdlib.List.filter_map on_typ supers)
        in
        log "Adding a class %S with id  = %d" cname cid;
        Hashtbl.add name_of_id_hash cid cname;
        Hashtbl.add classes cname cid
    | I { iname; iparams; isupers } ->
        if true then (
          (* log "Running on interface %s..." iname; *)
          Hashtbl.clear params_hash;
          (* Stdlib.List.iteri
             (fun i { pname; _ } ->
               let id = CT.new_var () in
               log "Creating new Var with name = %S, id = %d, index = %d" pname
                 id i;
               Hashtbl.add params_hash pname (var_info ~id i))
             iparams; *)
          let iid =
            CT.make_interface_fix
              (fun cur_id ->
                cur_name := (iname, cur_id);
                List.mapi on_param iparams)
              (fun cur_id ->
                cur_name := (iname, cur_id);
                Stdlib.List.filter_map on_typ isupers)
          in
          log "Adding an interface %s with id = %d" iname iid;
          Hashtbl.add name_of_id_hash iid iname;
          Hashtbl.add ifaces iname iid)
        else
          Format.eprintf
            "The interface '%s' seems to be not declared. Skipping.\n%!" iname
  and on_param idx { pname; p_upper } : JGS.jtype =
    let upper_bounds =
      p_upper |> List.map on_typ
      |> List.map (function
           | None -> failwith "Can't interpet a parameter"
           | Some p -> p)
    in
    let new_id = CT.new_var () in
    Hashtbl.add params_hash pname (var_info ~id:new_id idx);
    let upb =
      match upper_bounds with
      | [] -> CT.object_t
      | [ x ] -> x
      | xs -> JGS.Intersect xs
    in
    JGS.Var { id = new_id; index = idx; lwb = None; upb }
  (* CT.make_tvar idx (List.hd upper_bounds) *)
  (* upper_bounds *)
  (* CT.object_t *)
  (* This works only if we have in the upper bound an Object *)
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
        | { vi_id; vi_index } ->
            let () =
              if args <> [] then
                failwith "Type variables with arguments do not happen in Java"
            in
            log "on_typ: Got a parameter with index = %d and id = %d" vi_index
              vi_id;
            return @@ CT.make_tvar vi_index CT.object_t
        | exception Not_found -> (
            match Hashtbl.find classes name with
            | id ->
                (* log "Building a class id=%d, name = %s" id name; *)
                return @@ JGS.Class (id, List.map on_arg args)
            | exception Not_found ->
                Format.eprintf
                  "   The name %S is not class or parameter. Substituting \
                   object. (cur_name = %s)\n\
                   %!"
                  name (fst !cur_name);
                Some CT.object_t))
    | Interface (name, args) -> (
        match Hashtbl.find params_hash name with
        | { vi_id; vi_index } ->
            let () =
              if args <> [] then
                failwith "Type variables with arguments do not happen in Java"
            in
            log "on_typ: Got a parameter with index = %d and id = %d" vi_index
              vi_id;
            return @@ CT.make_tvar vi_index CT.object_t
        | exception Not_found -> (
            match Hashtbl.find ifaces name with
            | id ->
                (* log "Building an interface %d" id; *)
                return @@ JGS.Interface (id, List.map on_arg args)
            | exception Not_found ->
                Format.eprintf
                  "  The name %S is not interface or parameter. Substituting \
                   object  (cur_name = %s)\n\
                   %!"
                  name (fst !cur_name);
                Some CT.object_t))
    | Array typ -> Option.map CT.array_t (on_typ typ)
    | Var { id; upb; lwb; index } -> (
        (* log "Looking for param %s " id; *)
        match Hashtbl.find params_hash id with
        | exception Not_found ->
            Format.eprintf
              "Possibly undeclared param '%s' in the class '%s'\n%!" id
              (fst !cur_name);
            return CT.object_t
        | { vi_id; vi_index } ->
            if index <> vi_index then
              Format.eprintf
                "WARNING: Possible issue with indexes in variable declaration \
                 site and isage site. %d <> %d\n\
                 %!"
                index vi_index;
            return
            @@ JGS.(
                 Var
                   {
                     id = vi_id;
                     index = vi_index;
                     upb = unwrap (on_typ upb) Fun.id (* Fix HERE *);
                     lwb =
                       Option.bind lwb (fun x -> unwrap (on_typ x) Option.some);
                   }))
    | Intersect _ ->
        Format.eprintf
          "Intersections are not supported. Substituting Object\n%!";
        return CT.object_t
    | Primitive s -> return (CT.primitive_t s)
    | t ->
        Format.eprintf "%a\n%!" Yojson.Safe.pp (yojson_of_jtype t);
        failwith "unsuported case"
  in
  populate_graph on_decl table;
  let name_of_id id =
    match Hashtbl.find name_of_id_hash id with
    | exception Not_found -> raise (Id_not_found id)
    | s -> s
  in
  let id_of_name name =
    match Hashtbl.find classes name with
    | cid -> cid
    | exception Not_found -> (
        match Hashtbl.find ifaces name with
        | id -> id
        | exception Not_found -> raise (Name_not_found name))
  in
  (ct, id_of_name, name_of_id)

type var_desc = Queried | Named of string

let pp_var_desc ppf = function
  | Queried -> Format.fprintf ppf "_.?"
  | Named s -> Format.fprintf ppf "_.%s" s

type result_query =
  (JGS.HO.jtype_injected ->
  JGS.HO.jtype_injected ->
  (* bool OCanren.ilogic -> *)
  OCanren.goal) ->
  (JGS.HO.jtype_injected -> JGS.HO.jtype_injected) ->
  JGS.HO.jtype_injected ->
  OCanren.goal

let make_query j : _ * result_query * _ =
  let { table; neg_upper_bounds; neg_lower_bounds; upper_bounds; lower_bounds }
      =
    query_of_yojson j
  in
  let ((module CT) as ct), id_of_name, name_of_id = make_classtable table in

  if neg_lower_bounds <> [] || neg_upper_bounds <> [] then
    Format.eprintf "Negatives bound are not yet supported\n%!";

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
    |> SS.filter (fun name ->
           match id_of_name name with
           | _ -> false
           | exception Name_not_found _ -> true)
  in

  let () =
    Format.printf "\nType variables mentioned in constraints: [";
    SS.to_seq varnames |> Seq.iter (Format.printf " %s ");
    Format.printf "]\n\n%!"
  in

  (* TODO: negative bound too *)
  let upper_bounds, lower_bounds =
    let ( ++ ) (a, b) (c, d) = (a @ c, b @ d) in
    let fold :
        jtype -> ((var_desc * jtype) list * (var_desc * jtype) list) * jtype =
      let empty = ([], []) in
      let rec helper : _ -> _ * _ = function
        | Var { id; upb; lwb; _ } as t ->
            let ans1, upb = helper upb in
            let acc = ((Named id, upb) :: fst ans1, snd ans1) in
            let ans2 =
              match lwb with
              | None -> ([], [])
              | Some t ->
                  let ans, lwb = helper t in
                  (fst ans, (Named id, lwb) :: snd ans)
            in
            (acc ++ ans2, t)
        | Interface (name, args) ->
            let acc, new_args =
              List.fold_right
                (fun x (acc, args) ->
                  let acc0, x = helper x in
                  (acc0 ++ acc, x :: args))
                args
                (([], []), [])
            in
            (acc, Interface (name, new_args))
        | Class (name, args) ->
            let acc, new_args =
              List.fold_right
                (fun x (acc, args) ->
                  let acc0, x = helper x in
                  (acc0 ++ acc, x :: args))
                args
                (([], []), [])
            in
            (acc, Class (name, new_args))
        | Wildcard None as v -> (empty, v)
        | Wildcard (Some (pol, t)) ->
            let acc0, t = helper t in
            (acc0, Wildcard (Some (pol, t)))
        | t ->
            Format.eprintf "%s\n%!"
              (Yojson.Safe.pretty_to_string (yojson_of_jtype t));
            assert false
      in
      fun typ -> helper typ
    in

    let upper_rez =
      List.fold_left
        (fun acc t ->
          let rez, t = fold t in
          acc ++ ([ (Queried, t) ], []) ++ rez)
        ([], []) upper_bounds
    in
    let lower_rez =
      List.fold_left
        (fun acc t ->
          let rez, t = fold t in
          acc ++ ([ (Queried, t) ], []) ++ rez)
        ([], []) lower_bounds
    in
    upper_rez ++ lower_rez
  in
  let goal is_subtype targ_inj answer_typ =
    let make_vars names k =
      let storage = Hashtbl.create 23 in
      let rec helper = function
        | [] ->
            k (fun s ->
                try Hashtbl.find storage s
                with Not_found ->
                  failwiths
                    "Logic variable for Var named %s should be defined \
                     beforehand, but doesn't. "
                    s)
        | h :: tl ->
            OCanren.Fresh.one (fun q ->
                Hashtbl.add storage h q;
                helper tl)
      in
      helper names
    in
    make_vars
      (SS.to_seq varnames |> List.of_seq)
      (fun lookup ->
        let open OCanren in
        let rec on_typ : _ -> JGS.HO.jtype_injected = function
          | Class (name, args) -> (
              match id_of_name name with
              | cid -> JGS_Helpers.class_ !!cid (Std.list on_arg args)
              | exception Not_found ->
                  failwiths "Can't find class name '%s'" name)
          | Interface (name, args) -> (
              match id_of_name name with
              | cid -> JGS_Helpers.interface !!cid (Std.list on_arg args)
              | exception Not_found ->
                  failwiths "Can't find class name '%s'" name)
          | Var { id; _ } -> lookup id
          | t ->
              Format.eprintf "%a\n%!" Yojson.Safe.pp (yojson_of_jtype t);
              failwith "unsuported case"
        and on_arg : _ -> _ JGS.HO.targ_injected = function
          (* TODO: wildcards are not used, fix that later *)
          | Wildcard None -> JGS_Helpers.wildcard (Std.none ())
          | Wildcard (Some (kind, typ)) ->
              let pol : JGS.HO.polarity_injected =
                match kind with
                | Super -> !!JGS.HO.Super
                | Extends -> !!JGS.HO.Extends
              in
              JGS_Helpers.wildcard (Std.some !!(pol, on_typ typ))
          | t ->
              let __ () =
                Format.printf "t = %a\n%!"
                  (Yojson.Safe.pretty_print ~std:false)
                  (yojson_of_jtype t)
              in
              JGS_Helpers.type_ (on_typ t)
        in

        let open OCanren in
        let ask_var = function Queried -> answer_typ | Named v -> lookup v in
        let wrap pos (name, b) =
          Format.printf "\t%sProcessing: %a <-< %a\n%!"
            (if pos then "     " else " NOT ")
            pp_var_desc name pp_jtype b;
          is_subtype (ask_var name) (targ_inj (on_typ b))
          (* TODO: There were bool here to switch positive/negative *)
        in
        let pos_upper_goals = List.map (wrap true) upper_bounds in
        let pos_lower_goals = List.map (wrap true) lower_bounds in
        let neg_upper_goals = [] (* List.map (wrap false) upper_bounds  *) in
        let neg_lower_goals =
          (* List.map (wrap false) lower_bounds *)
          []
        in
        let all_goals =
          List.concat
            [
              pos_upper_goals; pos_lower_goals; neg_upper_goals; neg_lower_goals;
            ]
        in
        match all_goals with
        | [] ->
            Format.printf "The are not bounds. Exit\n%!";
            exit 1
        | _ -> List.fold_right ( &&& ) all_goals success)
  in

  (ct, goal, name_of_id)
