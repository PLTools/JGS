open OCanren
open OCanren.Std
open JGS

module type SCT = MutableTypeTable.SAMPLE_CLASSTABLE

let is_correct_type (module CT : SCT) ~closure_subtyping t =
  let decl_by_id id decl = CT.HO.decl_by_id (( === ) id) decl in
  conde
    [
      (* Array: always allow *)
      fresh elems (t === !!(HO.Array elems));
      (* Class: should be metioned in class declarations with the same arguments amount *)
      fresh
        (id actual_params expected_params super supers length)
        (t === !!(HO.Class (id, actual_params)))
        (decl_by_id id !!HO.(C !!{ params = expected_params; super; supers }))
        (List.lengtho expected_params length)
        (List.lengtho actual_params length);
      (* Interface: should be metioned in interface declarations with the same arguments amount *)
      fresh
        (id actual_params expected_params supers length)
        (t === !!(HO.Interface (id, actual_params)))
        (decl_by_id id !!HO.(I !!{ params = expected_params; supers }))
        (List.lengtho expected_params length)
        (List.lengtho actual_params length);
      (* Variable: lower bound should be subtype of upper bound *)
      fresh (id index upb lwb)
        (t === !!(HO.Var { id; index; upb; lwb = some lwb }))
        (upb =/= lwb)
        (closure_subtyping lwb upb);
      (* Varaible without lover bound: always allow *)
      fresh (id index upb) (t === !!(HO.Var { id; index; upb; lwb = none () }));
      (* Null: always allow *)
      t === !!HO.Null;
      (* Intersect: always allow *)
      fresh args (t === !!(HO.Intersect args));
    ]

let ( -<- ) (module CT : SCT) ~direct_subtyping ~closure_subtyping
    ~is_correct_type ta tb =
  fresh ()
    (direct_subtyping
       (fun a b rez -> fresh () (rez === !!true) (closure_subtyping a b))
       ta tb !!true)
    (is_correct_type ta) (is_correct_type tb)

let rec ( <-< ) ~direct_subtyping ta tb =
  conde
    [
      direct_subtyping ta tb;
      fresh ti (tb =/= ti) (ta =/= ti) (ta =/= tb) (direct_subtyping ta ti)
        (( <-< ) ~direct_subtyping ti tb);
    ]

let make_closure (module CT : SCT) direct_subtyping =
  let rec is_correct t =
    is_correct_type (module CT) ~closure_subtyping:closure t
  and direct ta tb =
    ( -<- )
      (module CT)
      ~direct_subtyping ~closure_subtyping:closure ~is_correct_type:is_correct
      ta tb
  and closure ta tb = ( <-< ) ~direct_subtyping:direct ta tb in
  (is_correct, direct, closure)
