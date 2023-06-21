open OCanren
open JGS
open JGS_Helpers

(**************************************************************************************************)
(*************************** Functional-relational fuctor parameter *******************************)
(**************************************************************************************************)

module type SAMPLE_CLASSTABLE = sig
  val decl_by_id : int -> decl
  val get_superclass : int -> int -> jtype option
  val is_root_interface : int -> bool
  val object_t : jtype
  val array_t : jtype -> jtype
  val primitive_t : string -> jtype
  val cloneable_t : jtype
  val serializable_t : jtype
  val new_var : unit -> int
  val make_class : jtype list -> jtype -> jtype list -> int
  val make_tvar : int -> jtype -> jtype
  val make_interface : jtype list -> jtype list -> int

  val make_class_fix :
    params:(int -> jtype list) -> (int -> jtype) -> (int -> jtype list) -> int

  val make_interface_fix : (int -> jtype list) -> (int -> jtype list) -> int

  module HO : sig
    val decl_by_id : (Std.Nat.injected -> goal) -> HO.decl_injected -> goal

    val get_superclass :
      (Std.Nat.injected -> goal) ->
      (Std.Nat.injected -> goal) ->
      HO.jtype_injected Std.Option.injected ->
      goal

    val is_root_interface :
      (Std.Nat.injected -> goal) -> bool OCanren.ilogic -> goal

    val object_t : HO.jtype_injected -> goal
    val cloneable_t : HO.jtype_injected -> goal
    val serializable_t : HO.jtype_injected -> goal
    val new_var : (GT.unit ilogic -> goal) -> Std.Nat.injected -> goal
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

  let make_tvar index upb = Var { id = new_id (); index; upb; lwb = None }
  let m = ref M.empty
  let table_was_changed = ref true
  let make_params params = Stdlib.List.mapi (fun i p -> make_tvar i p) params

  let add_class (c : cdecl) =
    let id = new_id () in
    let d = C { c with params = (* make_params *) c.params } in
    m := M.add id d !m;
    table_was_changed := true;
    id

  let add_interface (i : idecl) =
    let id = new_id () in
    let d = I { i with params = (* make_params *) i.params } in
    m := M.add id d !m;
    table_was_changed := true;
    id

  let decl_by_id id = M.find id !m

  let get_superclass sub_id super_id =
    let open Stdlib in
    List.find_map (fun (id, decl) ->
        if id = sub_id then
          let supers =
            match decl with
            | C { super; supers; _ } -> super :: supers
            | I { supers; _ } -> supers
          in
          List.find_map
            (fun super ->
              match super with
              | (Class (id, _) | Interface (id, _)) when id = super_id ->
                  Some super
              | _ -> None)
            supers
        else None)
    @@ M.bindings !m

  let is_root_interface id =
    match M.find_opt id !m with
    | Some (I { supers = []; _ }) -> true
    | _ -> false

  let add_class_fix (c : int -> cdecl) =
    let id = new_id () in
    let c = c id in
    let d = C { c with params = c.params } in
    m := M.add id d !m;
    table_was_changed := true;
    id

  let add_interface_fix (i : int -> idecl) =
    let id = new_id () in
    let iface = i id in
    let d = I { iface with params = iface.params } in
    m := M.add id d !m;
    table_was_changed := true;
    id

  let make_tvar index upb = Var { id = new_id (); index; upb; lwb = None }

  let make_class params super supers =
    let id = add_class { params; super; supers } in
    (* Printf.printf "Class   with id=%d was created\n%!" id; *)
    id

  let make_interface params supers =
    let id = add_interface { params; supers } in
    (* Printf.printf "Interface   with id=%d was created\n%!" id; *)
    id

  let make_class_fix ~params super supers =
    add_class_fix (fun id ->
        let params = params id in
        { params; super = super id; supers = supers id })

  let make_interface_fix params supers =
    add_interface_fix (fun id ->
        let params = params id in
        { params; supers = supers id })

  let top = Class (0, [])

  let object_t =
    let id = make_class [] top [] in
    assert (id = 1);
    Class (id, [])

  let cloneable_t =
    let id = make_interface [] [] in
    assert (id = 2);
    Interface (id, [])

  let serializable_t =
    let id = make_interface [] [] in
    assert (id = 3);
    Interface (id, [])

  let array_t param =
    let id = make_class [] top [] in
    Class (id, [ Type param ])

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
      Class (id, [])

  let new_var = new_id

  module HO = struct
    type disj_args = {
      decl_by_id : (Std.Nat.groundi * HO.decl_injected) list lazy_t;
      is_direct_subclass :
        (Std.Nat.groundi * Std.Nat.groundi * HO.jtype_injected) list lazy_t;
      is_root_interface : Std.Nat.groundi list lazy_t;
    }

    let update_disj_args =
      let decl_by_id_disjs_args = ref (lazy []) in
      let is_direct_subclass_disjs_args = ref (lazy []) in
      let is_root_interface_disj_args = ref (lazy []) in
      fun () ->
        if !table_was_changed then (
          let bindings = M.bindings !m in
          decl_by_id_disjs_args :=
            lazy
              (Stdlib.List.map (fun (k, v) -> (Std.nat k, decl_inj v)) bindings);
          is_direct_subclass_disjs_args :=
            lazy
              (Stdlib.List.concat_map
                 (fun (sub_id, decl) ->
                   let supers =
                     match decl with
                     | C { super; supers; _ } -> super :: supers
                     | I { supers; _ } -> supers
                   in
                   Stdlib.List.filter_map
                     (fun super ->
                       match super with
                       | Class (super_id, _) | Interface (super_id, _) ->
                           Some
                             (Std.nat sub_id, Std.nat super_id, jtype_inj super)
                       | _ -> None)
                     supers)
                 bindings);
          is_root_interface_disj_args :=
            lazy
              (Stdlib.List.filter_map
                 (fun (k, v) ->
                   match v with
                   | I { supers = []; _ } -> Some (Std.nat k)
                   | _ -> None)
                 bindings);
          table_was_changed := false);
        {
          decl_by_id = !decl_by_id_disjs_args;
          is_direct_subclass = !is_direct_subclass_disjs_args;
          is_root_interface = !is_root_interface_disj_args;
        }

    let get_decl_by_id_disjs_args () =
      Lazy.force (update_disj_args ()).decl_by_id

    let get_is_direct_subclass_disjs_args () =
      Lazy.force (update_disj_args ()).is_direct_subclass

    let get_is_root_interface_disjs_args () =
      Lazy.force (update_disj_args ()).is_root_interface

    let decl_by_id id rez =
      fresh id_val (id id_val)
        (let disj_args = get_decl_by_id_disjs_args () in
         let disjs =
           Stdlib.List.map
             (fun (k, v) -> id_val === k &&& (rez === v))
             disj_args
         in
         match disjs with [] -> failure | _ -> conde disjs)

    let get_superclass :
        (Std.Nat.injected -> goal) ->
        (Std.Nat.injected -> goal) ->
        HO.jtype_injected Std.Option.injected ->
        goal =
     fun sub_id super_id some_rez ->
      fresh
        (sub_id_val super_id_val rez)
        (sub_id sub_id_val) (super_id super_id_val)
        (some_rez === Std.some rez)
        (let disj_args = get_is_direct_subclass_disjs_args () in
         let disjs =
           Stdlib.List.map
             (fun (sub, sup, super) ->
               sub_id_val === sub &&& (super_id_val === sup) &&& (rez === super))
             disj_args
         in
         match disjs with [] -> failure | _ -> conde disjs)

    let is_root_interface :
        (Std.Nat.injected -> goal) -> bool OCanren.ilogic -> goal =
     fun id rez ->
      fresh id_val (id id_val) (rez === !!true)
        (let disj_args = get_is_root_interface_disjs_args () in
         let disjs = Stdlib.List.map (( === ) id_val) disj_args in
         match disjs with [] -> failure | _ -> conde disjs)

    let object_t x = x === jtype_inj object_t
    let cloneable_t x = x === jtype_inj cloneable_t
    let serializable_t x = x === jtype_inj serializable_t
    let new_var _ x = x === Std.nat (new_id ())
  end
end
