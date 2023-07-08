open OCanren
open OCanren.Std
open JGS.HO

module type SCT = MutableTypeTable.SAMPLE_CLASSTABLE

type closure = {
  is_correct_type : jtype_injected -> goal;
  direct_subtyping : jtype_injected -> jtype_injected -> goal;
  closure : jtype_injected -> jtype_injected -> goal;
}

let rec list_same_length : _ Std.List.injected -> _ Std.List.injected -> goal =
 fun xs ys ->
  conde
    [
      fresh (h1 h2 tl1 tl2)
        (xs === Std.List.cons h1 tl1)
        (ys === Std.List.cons h2 tl2)
        (list_same_length tl1 tl2);
      xs === Std.nil () &&& (ys === Std.nil ());
    ]

let is_correct_type (module CT : SCT) ~closure_subtyping t =
  let decl_by_id id decl = CT.HO.decl_by_id (( === ) id) decl in
  conde
    [
      (* Array: always allow *)
      fresh elems (t === !!(Array elems));
      (* Class: should be metioned in class declarations with the same arguments amount *)
      fresh
        (id actual_params expected_params super supers length)
        (t === !!(Class (id, actual_params)))
        (decl_by_id id !!(C !!{ params = expected_params; super; supers }))
        (* TODO (Kakadu): write a relation same_length *)
        (* (List.lengtho expected_params length)
           (List.lengtho actual_params length) *)
        (list_same_length expected_params actual_params);
      (* Interface: should be metioned in interface declarations with the same arguments amount *)
      fresh
        (id actual_params expected_params supers length)
        (t === !!(Interface (id, actual_params)))
        (decl_by_id id !!(I !!{ params = expected_params; supers }))
        (List.lengtho expected_params length)
        (List.lengtho actual_params length);
      (* Variable: lower bound should be subtype of upper bound *)
      fresh (id index upb lwb)
        (t === !!(Var { id; index; upb; lwb = some lwb }))
        (upb =/= lwb)
        (closure_subtyping lwb upb);
      (* Varaible without lover bound: always allow *)
      fresh (id index upb) (t === !!(Var { id; index; upb; lwb = none () }));
      (* Null: always allow *)
      t === !!Null;
      (* Intersect: always allow *)
      fresh args (t === !!(Intersect args));
    ]

let ( -<- ) (module CT : SCT) ~direct_subtyping ~closure_subtyping
    ~is_correct_type ta tb =
  fresh ()
    (direct_subtyping
       (fun a b rez -> fresh () (rez === !!true) (closure_subtyping a b))
       ta tb !!true)
    (is_correct_type ta) (is_correct_type tb)

let rec ( <-< ) ~direct_subtyping ta tb st =
  st
  |> fresh ()
       (JGS_Helpers.only_classes_interfaces_and_arrays ta)
       (JGS_Helpers.only_classes_interfaces_and_arrays tb)
       (conde
          [
            direct_subtyping ta tb;
            fresh ti (tb =/= ti) (ta =/= ti) (ta =/= tb)
              (JGS_Helpers.only_classes_interfaces_and_arrays ti)
              (direct_subtyping ti tb)
              (( <-< ) ~direct_subtyping ta ti);
          ])

let rec ( <=< ) ~direct_subtyping ta tb =
  fresh ()
    (JGS_Helpers.only_classes_interfaces_and_arrays ta)
    (JGS_Helpers.only_classes_interfaces_and_arrays tb)
    (conde
       [
         direct_subtyping ta tb;
         fresh ti (tb =/= ti) (ta =/= ti) (ta =/= tb)
           (JGS_Helpers.only_classes_interfaces_and_arrays ti)
           (direct_subtyping ta ti)
           (( <=< ) ~direct_subtyping ti tb);
       ])

let make_closure_subtyping (module CT : SCT) direct_subtyping =
  let rec is_correct t =
    is_correct_type (module CT) ~closure_subtyping:closure t
  and direct ta tb =
    ( -<- )
      (module CT)
      ~direct_subtyping ~closure_subtyping:closure ~is_correct_type:is_correct
      ta tb
  and closure ta tb = ( <-< ) ~direct_subtyping:direct ta tb in
  { is_correct_type = is_correct; direct_subtyping = direct; closure }

let make_closure_supertyping (module CT : SCT) direct_subtyping =
  let rec is_correct t =
    is_correct_type (module CT) ~closure_subtyping:closure t
  and direct ta tb =
    ( -<- )
      (module CT)
      ~direct_subtyping ~closure_subtyping:closure ~is_correct_type:is_correct
      ta tb
  and closure ta tb st =
    let () =
      let open JGS_stats in
      let ta_is_g = ref false in
      let tb_is_g = ref false in
      OCanren.is_ground ta st (fun b -> ta_is_g := b);
      OCanren.is_ground tb st (fun b -> tb_is_g := b);
      set_fish stats
        ((if !ta_is_g then fun ((a, b), c) -> ((a, b + 1), c)
          else fun ((a, b), c) -> ((a, b + 1), c))
           (get_fish stats));
      set_fish stats
        ((if !tb_is_g then fun (c, (a, b)) -> (c, (a, b + 1))
          else fun (c, (a, b)) -> (c, (a, b + 1)))
           (get_fish stats))
    in
    let _ = failwith "WTF" in
    ( <=< ) ~direct_subtyping:direct ta tb st
  in
  { is_correct_type = is_correct; direct_subtyping = direct; closure }
