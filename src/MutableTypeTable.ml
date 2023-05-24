open OCanren
open JGS
open JGS_Helpers

(**************************************************************************************************)
(*************************** Functional-relational fuctor parameter *******************************)
(**************************************************************************************************)

module SampleCT () = struct
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

  let add_class, add_interface, decl_by_id, decl_by_id_rel =
    let m = ref M.empty in
    ( (fun c ->
        let id = new_id () in
        let d = C c in
        m := M.add id d !m;
        id),
      (fun i ->
        let id = new_id () in
        let d = I i in
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

  let make_tvar index upb = Var { id = new_id (); index; upb; lwb = None }
  let make_class params super supers = add_class { params; super; supers }
  let make_interface params supers = add_interface { params; supers }
  let top = Class (0, [])

  let object_t =
    let id = make_class [] top [] in
    Class (id, [])

  let cloneable_t =
    let id = make_interface [] [] in
    Interface (id, [])

  let serializable_t =
    let id = make_interface [] [] in
    Interface (id, [])

  let new_var = new_id

  module HO = struct
    let decl_by_id = decl_by_id_rel
    let object_t x = x === jtype_inj object_t
    let cloneable_t x = x === jtype_inj cloneable_t
    let serializable_t x = x === jtype_inj serializable_t
    let new_var _ x = x === Std.nat (new_id ())
  end
end
