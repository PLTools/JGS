open OCanren
open JGS
open JGS_Helpers

(**************************************************************************************************)
(*************************** Functional-relational fuctor parameter *******************************)
(**************************************************************************************************)

module type SAMPLE_CLASSTABLE = sig
  val decl_by_id : int -> decl
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
    val decl_by_id :
      (OCanren__.Nat.injected -> goal) -> HO.decl_injected -> goal

    val object_t : HO.jtype_injected -> goal
    val cloneable_t : HO.jtype_injected -> goal
    val serializable_t : HO.jtype_injected -> goal
    val new_var : (GT.unit ilogic -> goal) -> OCanren__.Nat.injected -> goal
  end
end

module SampleCT () : SAMPLE_CLASSTABLE = struct
  let new_id =
    let n = ref 1 in
    fun () ->
      let i = !n in
      incr n;
      i

  module M = Map.Make (struct
    type t = int

    let compare = compare
  end)

  let make_tvar index upb = Var { id = new_id (); index; upb; lwb = None }
  let m = ref M.empty
  let make_params params = Stdlib.List.mapi (fun i p -> make_tvar i p) params

  let add_class, add_interface, decl_by_id, decl_by_id_rel =
    ( (fun (c : cdecl) ->
        let id = new_id () in
        let d = C { c with params = (* make_params *) c.params } in
        m := M.add id d !m;
        id),
      (fun (i : idecl) ->
        let id = new_id () in
        let d = I { i with params = (* make_params *) i.params } in
        m := M.add id d !m;
        id),
      (fun id -> M.find id !m),
      fun id rez ->
        fresh id_val (id id_val)
          (let disjs =
             Stdlib.List.map (fun (k, v) ->
                 fresh () (id_val === Std.nat k) (rez === decl_inj v))
             @@ M.bindings !m
           in
           match disjs with [] -> failure | _ -> conde disjs) )

  let add_class_fix (c : int -> cdecl) =
    let id = new_id () in
    let c = c id in
    let d = C { c with params = c.params } in
    m := M.add id d !m;
    id

  let add_interface_fix (i : int -> idecl) =
    let id = new_id () in
    let iface = i id in
    let d = I { iface with params = iface.params } in
    m := M.add id d !m;
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
    let decl_by_id = decl_by_id_rel
    let top = Class (-1, [])
    let object_t x = x === jtype_inj object_t
    let cloneable_t x = x === jtype_inj cloneable_t
    let serializable_t x = x === jtype_inj serializable_t
    let new_var _ x = x === Std.nat (new_id ())
  end
end
