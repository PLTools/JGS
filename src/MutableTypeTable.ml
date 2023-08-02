open OCanren
open JGS
open JGS_Helpers

(**************************************************************************************************)
(*************************** Functional-relational fuctor parameter *******************************)
(**************************************************************************************************)

module type SAMPLE_CLASSTABLE = sig
  val decl_by_id : int -> decl
  val get_superclass : int -> int -> jtype option
  val object_t : jtype
  val array_t : jtype -> jtype
  val primitive_t : string -> jtype
  val cloneable_t : jtype
  val serializable_t : jtype
  val new_var : unit -> int
  val make_class : ?name:string -> jtype list -> jtype -> jtype list -> int
  val make_tvar : ?name:string -> int -> jtype -> jtype
  val make_interface : ?name:string -> jtype list -> jtype list -> int

  val make_class_fix :
    ?name:string ->
    params:(int -> jtype list) ->
    (int -> jtype) ->
    (int -> jtype list) ->
    int

  val make_interface_fix :
    ?name:string -> (int -> jtype list) -> (int -> jtype list) -> int
  (** [make_interface_fix make_params make_superinterfaces] creates a new interface in open recursion style *)

  val pp_targ : Format.formatter -> HO.jtype_logic HO.targ_logic -> unit
  val pp_jtyp : Format.formatter -> HO.jtype_logic -> unit

  module HO : sig
    val decl_by_id_fo : int ilogic -> HO.decl_injected -> OCanren.goal
    val decl_by_id : (int ilogic -> goal) -> HO.decl_injected -> goal

    val get_superclass_fo :
      ?from:int ->
      int ilogic ->
      int ilogic ->
      HO.jtype_injected Std.Option.injected ->
      goal

    val get_superclass :
      (int ilogic -> goal) ->
      (int ilogic -> goal) ->
      HO.jtype_injected Std.Option.injected ->
      goal

    val object_t_ho : HO.jtype_injected -> goal
    val object_t : HO.jtype_injected
    val cloneable_t_ho : HO.jtype_injected -> goal
    val cloneable_t : HO.jtype_injected
    val serializable_t_ho : HO.jtype_injected -> goal
    val serializable_t : HO.jtype_injected
    val new_var : (GT.unit ilogic -> goal) -> int ilogic -> goal
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

  let make_tvar ?name:_ index upb =
    let id = new_id () in
    (* Printf.printf "Create variable %s with id = %d\n%!"
       (Stdlib.Option.value name ~default:"")
       id; *)
    Var { id; index; upb; lwb = None }

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
  let top = Class (top_id, [])

  let object_t =
    let id = make_class ~name:"java.lang.Object" [] top [] in
    assert (id = 1);
    Class (id, [])

  let cloneable_t =
    let id = make_interface ~name:"java.lang.Cloneable" [] [] in
    assert (id = 2);
    Interface (id, [])

  let serializable_t =
    let id = make_interface ~name:"java.io.Serializable" [] [] in
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
  let pp_jtyp = JGS_Helpers.pp_jtyp_logic (fun _ -> "?")
  let pp_targ = JGS_Helpers.pp_targ_logic (fun _ -> "?")

  module HO = struct
    type disj_args = {
      decl_by_id : (int ilogic * HO.decl_injected) list lazy_t;
      get_superclass : (int ilogic * int ilogic * HO.jtype_injected) list lazy_t;
    }

    let update_disj_args =
      let decl_by_id_disjs_args = ref (lazy []) in
      let get_superclass_disjs_args = ref (lazy []) in
      fun () ->
        if !table_was_changed then (
          let bindings = M.bindings !m in
          decl_by_id_disjs_args :=
            lazy (Stdlib.List.map (fun (k, v) -> (!!k, decl_inj v)) bindings);
          get_superclass_disjs_args :=
            lazy
              (Stdlib.List.concat_map
                 (fun (sub_id, decl) ->
                   let supers =
                     match decl with
                     | C { super; supers; _ } -> super :: supers
                     | I { supers = []; _ } -> [ object_t ]
                     | I { supers; _ } -> supers
                   in
                   Stdlib.List.filter_map
                     (fun super ->
                       match super with
                       | (Class (super_id, _) | Interface (super_id, _))
                         when super_id <> top_id ->
                           (* Printf.printf "sub_id: %d, super_id: %d\n" sub_id
                              super_id; *)
                           Some (!!sub_id, !!super_id, jtype_inj super)
                       | _ -> None)
                     supers)
                 bindings);
          table_was_changed := false);
        {
          decl_by_id = !decl_by_id_disjs_args;
          get_superclass = !get_superclass_disjs_args;
        }

    let get_decl_by_id_disjs_args () =
      Lazy.force (update_disj_args ()).decl_by_id

    let get_superclass_disjs_args () =
      Lazy.force (update_disj_args ()).get_superclass

    let decl_by_id_fo : int ilogic -> HO.decl_injected -> goal =
     fun id rez ->
      debug_var id (Fun.flip OCanren.reify) (function
        | [ Value id ] when id = top_id -> failure
        | [ Value id ] -> (
            (* TODO: Kakadu: should we memoize already injected values? *)
            match M.find id !m with
            | d -> rez === decl_inj d
            | exception Not_found ->
                failwith (Printf.sprintf "Not_found: id = %d" id))
        | _ -> (
            let disj_args = get_decl_by_id_disjs_args () in
            let on_element (k, v) = id === k &&& (rez === v) in
            let optimized_by_kakadu = true in
            (* The same trick which happens in the function [get_superclass] below *)
            if optimized_by_kakadu then
              (* Generating list of the size of class table is bad.
                 Not doing that could easily five 2x speedup *)
              let rec loop : _ -> goal = function
                | [] -> failure
                | h :: tl ->
                    OCanren.disj (on_element h) (delay (fun () -> loop tl))
              in
              loop disj_args
            else
              let disjs = Stdlib.List.map on_element disj_args in
              match disjs with [] -> failure | _ -> conde disjs))

    let decl_by_id : (int ilogic -> goal) -> HO.decl_injected -> goal =
     fun id d -> fresh id_val (id id_val) (decl_by_id_fo id_val d)

    let get_superclass_fo :
        ?from:int ->
        int ilogic ->
        int ilogic ->
        HO.jtype_injected Std.Option.injected ->
        goal =
     fun ?(from = 0) sub_id_val super_id_val some_rez st ->
      let () =
        if JGS_stats.config.trace_get_superclass then
          Format.printf
            "get_superclass(%d): sub_id_val = %a, super_id_val = %a, some_rez \
             = %a\n\
             %!"
            from
            (GT.fmt OCanren.logic (GT.fmt GT.int))
            (OCanren.reify_in_state st OCanren.reify sub_id_val)
            (GT.fmt OCanren.logic (GT.fmt GT.int))
            (OCanren.reify_in_state st OCanren.reify super_id_val)
            (GT.fmt Std.Option.logic pp_jtyp)
            (OCanren.reify_in_state st
               (Std.Option.reify JGS.HO.jtype_reify)
               some_rez)
      in
      st
      |> fresh rez
           (some_rez === Std.some rez)
           (let disj_args = get_superclass_disjs_args () in

            (* Printf.printf "%s generated %d disjuncts\n" __FUNCTION__
               (List.length disj_args); *)
            let optimized_by_kakadu = true in
            if optimized_by_kakadu then
              (* Generating list of the size of class table is bad.
                 Not doing that could easily five 2x speedup *)
              let rec loop : _ -> goal = function
                | [] -> failure
                | (sub, sup, super) :: tl ->
                    OCanren.disj
                      (sub_id_val === sub &&& (super_id_val === sup)
                     &&& (rez === super))
                      (delay (fun () -> loop tl))
              in
              loop disj_args
            else
              (* Peter's implementation *)
              let disjs =
                Stdlib.List.map
                  (fun (sub, sup, super) ->
                    sub_id_val === sub &&& (super_id_val === sup)
                    &&& (rez === super))
                  disj_args
              in
              match disjs with [] -> failure | _ -> conde disjs)

    let get_superclass :
        (int ilogic -> goal) ->
        (int ilogic -> goal) ->
        HO.jtype_injected Std.Option.injected ->
        goal =
     fun hoa hob jtyp ->
      fresh (a b) (hoa a) (hob b) (get_superclass_fo a b jtyp)

    let object_t = jtype_inj object_t
    let object_t_ho : HO.jtype_injected -> goal = fun x -> x === object_t
    let cloneable_t = jtype_inj cloneable_t
    let cloneable_t_ho : HO.jtype_injected -> goal = fun x -> x === cloneable_t
    let serializable_t = jtype_inj serializable_t

    let serializable_t_ho : HO.jtype_injected -> goal =
     fun x -> x === serializable_t

    let new_var _ x = x === !!(new_id ())
  end
end
