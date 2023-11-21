open OCanren
open JGS
open JGS_Helpers

(**************************************************************************************************)
(*************************** Functional-relational fuctor parameter *******************************)
(**************************************************************************************************)

let need_table_dynamic_specialisation = ref true

module type SAMPLE_CLASSTABLE = sig
  include CLASSTABLE

  val make_class :
    ?name:string ->
    int Jtype.ground list ->
    int Jtype.ground ->
    int Jtype.ground list ->
    int

  val make_tvar : ?name:string -> int -> int Jtype.ground -> int Jtype.ground

  val make_interface :
    ?name:string -> int Jtype.ground list -> int Jtype.ground list -> int

  val make_class_fix :
    ?name:string ->
    params:(int -> int Jtype.ground list) ->
    (int -> int Jtype.ground) ->
    (int -> int Jtype.ground list) ->
    int

  val make_interface_fix :
    ?name:string ->
    (int -> int Jtype.ground list) ->
    (int -> int Jtype.ground list) ->
    int
  (** [make_interface_fix make_params make_superinterfaces] creates a new interface in open recursion style *)

  val pp_targ : Format.formatter -> int logic Jtype.logic Targ.logic -> unit
  val pp_jtyp : Format.formatter -> int logic Jtype.logic -> unit

  module Ground : sig
    val decl_by_id : int -> int Decl.ground
    val object_t : int Jtype.ground
    val cloneable_t : int Jtype.ground
    val serializable_t : int Jtype.ground
    val array_t : int Jtype.ground -> int Jtype.ground
    val primitive_t : string -> int Jtype.ground
    val new_var : unit -> int
  end
end

module SampleCT () : SAMPLE_CLASSTABLE = struct
  let new_id =
    let n = ref 1 in
    fun () ->
      let i = !n in
      incr n;
      i

  module M = Map.Make (Int)

  (* let make_tvar index upb = Var { id = new_id (); index; upb; lwb = None } *)
  let m = ref M.empty
  let table_was_changed = ref true

  (* let make_params params = Stdlib.List.mapi (fun i p -> make_tvar i p) params *)
  type cdecl = {
    params : int Jtype.ground list;
    super : int Jtype.ground;
    supers : int Jtype.ground list;
  }

  let add_class { params; super; supers } =
    let id = new_id () in
    let d = Decl.C { params; super; supers } in
    m := M.add id d !m;
    table_was_changed := true;
    id

  type idecl = {
    params : int Jtype.ground list;
    supers : int Jtype.ground list;
  }

  let add_interface { params; supers } =
    let id = new_id () in
    let d = Decl.I { params; supers } in
    m := M.add id d !m;
    table_was_changed := true;
    id

  let decl_by_id id = M.find id !m

  let get_superclass_by_id sub_id super_id =
    let open Stdlib in
    List.find_map (fun (id, decl) ->
        if id = sub_id then
          let supers =
            match decl with
            | Decl.C { super; supers; _ } -> super :: supers
            | I { supers; _ } -> supers
          in
          List.find_map
            (fun super ->
              match super with
              | (Jtype.Class (id, _) | Interface (id, _)) when id = super_id ->
                  Some super
              | _ -> None)
            supers
        else None)
    @@ M.bindings !m

  let add_class_fix (c : int -> cdecl) =
    let id = new_id () in
    let { params; super; supers } = c id in
    let d = Decl.C { params; super; supers } in
    m := M.add id d !m;
    table_was_changed := true;
    id

  let add_interface_fix (i : int -> idecl) =
    let id = new_id () in
    let { params; supers } = i id in
    let d = Decl.I { params; supers } in
    m := M.add id d !m;
    table_was_changed := true;
    id

  let make_tvar ?name:_ index upb =
    let id = new_id () in
    (* Printf.printf "Create variable %s with id = %d\n%!"
       (Stdlib.Option.value name ~default:"")
       id; *)
    Jtype.Var { id; index = Std.Nat.of_int index; upb; lwb = None }

  let padding = -35

  let make_class ?name:_ params super supers =
    let id = add_class { params; super; supers } in
    (* Printf.printf "%*s with id=%d was created\n%!" padding
       ("Class     " ^ Stdlib.Option.value name ~default:"")
       id; *)
    id

  let make_interface ?name:_ params supers =
    let id = add_interface { params; supers } in
    (* Printf.printf "%*s with id=%d was created\n%!" padding
       ("Interface " ^ Stdlib.Option.value name ~default:"")
       id; *)
    id

  let make_class_fix ?name:_ ~params super supers =
    let id =
      add_class_fix (fun id ->
          let params = params id in
          { params; super = super id; supers = supers id })
    in
    (* Printf.printf "%*s with id=%d was created\n%!" padding
       ("Class     " ^ Stdlib.Option.value name ~default:"")
       id; *)
    id

  let make_interface_fix ?name:_ params supers =
    let id =
      add_interface_fix (fun id ->
          let params = params id in
          { params; supers = supers id })
    in
    (* Printf.printf "%*s with id=%d was created\n%!" padding
       ("Interface " ^ Stdlib.Option.value name ~default:"")
       id; *)
    id

  let top_id = 0
  let top = Jtype.Class (top_id, [])

  module Ground = struct
    let decl_by_id = decl_by_id

    let object_t =
      let id = make_class ~name:"java.lang.Object" [] top [] in
      assert (id = 1);
      Jtype.Class (id, [])

    let cloneable_t =
      let id = make_interface ~name:"java.lang.Cloneable" [] [] in
      assert (id = 2);
      Jtype.Interface (id, [])

    let serializable_t =
      let id = make_interface ~name:"java.io.Serializable" [] [] in
      assert (id = 3);
      Jtype.Interface (id, [])

    let array_t param =
      let id = make_class [] top [] in
      Jtype.Class (id, [ Targ.Type param ])

    let primitive_t =
      let h = Hashtbl.create 13 in
      fun name ->
        let id =
          match Hashtbl.find h name with
          | exception Not_found ->
              let id = make_class [] top [] in
              Hashtbl.add h name id;
              id
          | id -> id
        in
        Jtype.Class (id, [])

    let new_var = new_id
  end

  let pp_jtyp = JGS_Helpers.pp_jtyp_logic (fun _ -> "?")
  let pp_targ = JGS_Helpers.pp_targ_logic (fun _ -> "?")

  type disj_args = {
    decl_by_id : (int ilogic * int ilogic Decl.injected) list lazy_t;
    get_superclass_by_id : (int * int * int ilogic Jtype.injected) list lazy_t;
    subclass_map : (int * int ilogic Jtype.injected) list M.t lazy_t;
  }

  let get_supers = function
    | Decl.C { super; supers; _ } -> super :: supers
    | I { supers = []; _ } -> [ Ground.object_t ]
    | I { supers; _ } -> supers

  let get_jtype_kind = function
    | Decl.C _ -> Jtype_kind.Class
    | I _ -> Jtype_kind.Interface

  let update_disj_args =
    let decl_by_id_disjs_args = ref (lazy []) in
    let get_superclass_by_id_disjs_args = ref (lazy []) in
    let subclass_map = ref (lazy M.empty) in
    fun () ->
      if !table_was_changed then (
        let bindings = M.bindings !m in
        decl_by_id_disjs_args :=
          lazy (Stdlib.List.map (fun (k, v) -> (!!k, decl_inj v)) bindings);
        get_superclass_by_id_disjs_args :=
          lazy
            (Stdlib.List.concat_map
               (fun (sub_id, decl) ->
                 let supers = get_supers decl in
                 Stdlib.List.filter_map
                   (fun super ->
                     match super with
                     | (Jtype.Class (super_id, _) | Interface (super_id, _))
                       when super_id <> top_id ->
                         (* Printf.printf "sub_id: %d, super_id: %d\n" sub_id
                            super_id; *)
                         Some (sub_id, super_id, jtype_inj super)
                     | _ -> None)
                   supers)
               bindings);
        subclass_map :=
          lazy
            (Stdlib.List.fold_left
               (fun map (sub_id, decl) ->
                 let supers = get_supers decl in
                 Stdlib.List.fold_left
                   (fun map super ->
                     match super with
                     | (Jtype.Class (super_id, _) | Interface (super_id, _))
                       when super_id <> top_id ->
                         M.update super_id
                           (function
                             | Some l -> Some ((sub_id, jtype_inj super) :: l)
                             | None -> Some [ (sub_id, jtype_inj super) ])
                           map
                     | _ -> map)
                   map supers)
               M.empty bindings);
        table_was_changed := false);
      {
        decl_by_id = !decl_by_id_disjs_args;
        get_superclass_by_id = !get_superclass_by_id_disjs_args;
        subclass_map = !subclass_map;
      }

  let get_decl_by_id_disjs_args () = Lazy.force (update_disj_args ()).decl_by_id

  let get_superclass_by_id_disjs_args () =
    Lazy.force (update_disj_args ()).get_superclass_by_id

  let get_subclass_map () = Lazy.force (update_disj_args ()).subclass_map

  let decl_by_id : int ilogic -> int ilogic Decl.injected -> goal =
    let decl_by_id_ground id rez =
      (* TODO: Kakadu: should we memoize already injected values? *)
      match M.find id !m with
      | d -> rez === decl_inj d
      | exception Not_found -> failwith (Printf.sprintf "Not_found: id = %d" id)
    in
    let decl_by_id_free id rez =
      let disj_args = get_decl_by_id_disjs_args () in
      let on_element (k, v) = id === k &&& (rez === v) in
      (* Generating list of the size of class table is bad.
         Not doing that could easily five 2x speedup *)
      let rec loop : _ -> goal = function
        | [] -> failure
        | h :: tl -> OCanren.disj (on_element h) (delay (fun () -> loop tl))
      in
      loop disj_args
    in
    fun id rez ->
      debug_var id (Fun.flip OCanren.reify) (function
        | [ Value id ] when id = top_id -> failure
        | [ Value id ] -> decl_by_id_ground id rez
        | _ -> decl_by_id_free id rez)

  let get_superclass_by_id :
      int ilogic ->
      Jtype_kind.injected ->
      int ilogic ->
      Jtype_kind.injected ->
      int ilogic Jtype.injected Std.Option.injected ->
      goal =
    let get_superclass_by_id_ground_ground sub_id sub_kind super_id super_kind
        rez =
      (* We find all parents of type `sub_id` *)
      match M.find_opt sub_id !m with
      | None -> failure
      | Some decl -> (
          let supers = get_supers decl in
          let sub_expected_kind = !!(get_jtype_kind decl) in

          (* We find the parent of type `sub_id` that id is `super_id` *)
          match
            Stdlib.List.find_opt
              (function
                | (Jtype.Class (id, _) | Interface (id, _)) when id = super_id
                  ->
                    true
                | _ -> false)
              supers
          with
          | None -> failure
          (* We unify variable `rez` with only one declaration of parent that id is `super_id`.
             And we recognize the kind of the parent. *)
          | Some (Jtype.Class _ as t) ->
              sub_kind === sub_expected_kind
              &&& (super_kind === Jtype_kind.class_ ())
              &&& (rez === jtype_inj t)
          | Some (Jtype.Interface _ as t) ->
              sub_kind === sub_expected_kind
              &&& (super_kind === Jtype_kind.interface ())
              &&& (rez === jtype_inj t)
          | _ -> failure)
    in
    let get_superclass_by_id_ground_free sub_id sub_kind super_id_val super_kind
        rez =
      (* We find all parents of type `sub_id` *)
      match M.find_opt sub_id !m with
      | None -> failure
      (* We unify variable `rez` with all declarations of parents of type `sub_id`.
             And we recognize the kind of the parents. *)
      | Some decl ->
          let sub_expected_kind = !!(get_jtype_kind decl) in
          let rec loop : _ -> goal = function
            | [] -> failure
            | (Jtype.Class (super_id, _) as t) :: tl ->
                OCanren.disj
                  (sub_kind === sub_expected_kind
                  &&& (super_id_val === !!super_id)
                  &&& (super_kind === Jtype_kind.class_ ())
                  &&& (rez === jtype_inj t))
                  (delay (fun () -> loop tl))
            | (Interface (super_id, _) as t) :: tl ->
                OCanren.disj
                  (sub_kind === sub_expected_kind
                  &&& (super_id_val === !!super_id)
                  &&& (super_kind === Jtype_kind.interface ())
                  &&& (rez === jtype_inj t))
                  (delay (fun () -> loop tl))
            | _ :: tl -> delay (fun () -> loop tl)
          in
          loop (get_supers decl)
    in
    let get_superclass_by_id_free_ground sub_id_val sub_kind super_id super_kind
        rez =
      let subclass_map = get_subclass_map () in
      (* We find all children of type `sub_id` in precalculated map of children.
         For each type this map contains list of children ids and declaration of type `super_id` *)
      match M.find_opt super_id subclass_map with
      | None -> failure
      | Some filtered_by_super ->
          let rec loop : _ -> goal = function
            | [] -> failure
            (* For each child we unify variable `rez` with declaration of `super_id`
               and `sub_id_val` with the id found.
               And we recognize the kind of the super. *)
            | (sub, super) :: tl ->
                let sub_expected_kind = !!(get_jtype_kind @@ M.find sub !m) in
                OCanren.disj
                  (sub_id_val === !!sub
                  &&& (sub_kind === sub_expected_kind)
                  &&& (rez === super)
                  &&& conde
                        [
                          super === Jtype.class_ __ __
                          &&& (super_kind === Jtype_kind.class_ ());
                          super === Jtype.interface __ __
                          &&& (super_kind === Jtype_kind.interface ());
                        ])
                  (delay (fun () -> loop tl))
          in
          loop filtered_by_super
    in
    let get_superclass_by_id_free_free sub_id_val sub_kind super_id_val
        super_kind rez =
      let disj_args = get_superclass_by_id_disjs_args () in

      (* Printf.printf "%s generated %d disjuncts\n" __FUNCTION__
         (List.length disj_args); *)

      (* Generating list of the size of class table is bad.
         Not doing that could easily five 2x speedup *)
      let rec loop : _ -> goal = function
        | [] -> failure
        | (sub, sup, super) :: tl ->
            let sub_expected_kind = !!(get_jtype_kind @@ M.find sub !m) in
            let sup_expected_kind = !!(get_jtype_kind @@ M.find sup !m) in
            OCanren.disj
              (sub_id_val === !!sub &&& (super_id_val === !!sup)
              &&& (sub_kind === sub_expected_kind)
              &&& (super_kind === sup_expected_kind)
              &&& (rez === super))
              (delay (fun () -> loop tl))
      in
      loop disj_args
    in
    fun sub_id_val sub_kind super_id_val super_kind some_rez ->
      (* debug_var sub_id_val (Fun.flip OCanren.reify) (fun sub_id ->
             debug_var sub_kind (Fun.flip Jtype_kind.reify) (fun sub_kind ->
                 debug_var super_id_val (Fun.flip OCanren.reify) (fun super_id ->
                     debug_var super_kind (Fun.flip Jtype_kind.reify)
                       (fun super_kind ->
                         let pp_kind = function
                           | [ Value Jtype_kind.Class ] -> "Class"
                           | [ Value Jtype_kind.Interface ] -> "Interface"
                           | _ -> "Var"
                         in
                         let pp_id = function
                           | [ Value id ] -> string_of_int id
                           | _ -> "Var"
                         in
                         Printf.printf
                           "debug_var -- sub_id: %s, sub_kind: %s, super_id: %s, \
                            super_kind: %s\n"
                           (pp_id sub_id) (pp_kind sub_kind) (pp_id super_id)
                           (pp_kind super_kind);
                         success))))
         &&& *)
      fresh rez
        (super_id_val =/= !!top_id)
        (some_rez === Std.some rez)
        (debug_var sub_id_val (Fun.flip OCanren.reify) (fun sub_id_reified ->
             debug_var super_id_val (Fun.flip OCanren.reify)
               (fun super_id_reified ->
                 match (sub_id_reified, super_id_reified) with
                 (* If ids of subclass and superclass are ground *)
                 | [ Value sub_id ], [ Value super_id ] ->
                     get_superclass_by_id_ground_ground sub_id sub_kind super_id
                       super_kind rez
                 (* If id of subclass is ground only *)
                 | [ Value sub_id ], _ ->
                     get_superclass_by_id_ground_free sub_id sub_kind
                       super_id_val super_kind rez
                 (* If id of superclass is ground only *)
                 | _, [ Value super_id ] ->
                     get_superclass_by_id_free_ground sub_id_val sub_kind
                       super_id super_kind rez
                 (* General case: if ids of sub and super classes are free *)
                 | _, _ ->
                     get_superclass_by_id_free_free sub_id_val sub_kind
                       super_id_val super_kind rez)))

  let object_t = jtype_inj Ground.object_t
  let cloneable_t = jtype_inj Ground.cloneable_t
  let serializable_t = jtype_inj Ground.serializable_t
  let new_var () = !!(new_id ())
end
