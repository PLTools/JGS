open OCanren
open OCanren.Std
open JGS.HO

let need_dynamic_closure = ref true

module type SCT = Mutable_type_table.SAMPLE_CLASSTABLE

type closure_type = Subtyping | Supertyping

type closure = {
  is_correct_type :
    closure_type:closure_type -> ?constr:goal -> jtype_injected -> goal;
  direct_subtyping :
    closure_type:closure_type ->
    ?constr:goal ->
    jtype_injected ->
    jtype_injected ->
    goal;
  closure :
    closure_type:closure_type ->
    ?constr:goal ->
    jtype_injected ->
    jtype_injected ->
    goal;
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
        (id actual_params expected_params super supers)
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

let rec ( <-< ) ~direct_subtyping ~constr ta tb st =
  if JGS_stats.config.trace_closure_subtyping then
    Format.printf "Closure.(<-<): ta = %a, tb = %a\n%!"
      (JGS_Helpers.pp_jtyp_logic ([%show: GT.int OCanren.logic] ()))
      (OCanren.reify_in_state st jtype_reify ta)
      (JGS_Helpers.pp_jtyp_logic ([%show: GT.int OCanren.logic] ()))
      (OCanren.reify_in_state st jtype_reify tb);
  st
  |> fresh () constr
       (JGS_Helpers.only_classes_interfaces_and_arrays ta)
       (JGS_Helpers.only_classes_interfaces_and_arrays tb)
       (conde
          [
            direct_subtyping ta tb;
            fresh ti (tb =/= ti) (ta =/= ti) (ta =/= tb)
              (JGS_Helpers.only_classes_interfaces_and_arrays ti)
              (direct_subtyping ti tb)
              (( <-< ) ~direct_subtyping ~constr ta ti);
          ])

let rec ( <=< ) ~direct_subtyping ~constr ta tb =
  fresh () constr
    (JGS_Helpers.only_classes_interfaces_and_arrays ta)
    (JGS_Helpers.only_classes_interfaces_and_arrays tb)
    (conde
       [
         direct_subtyping ta tb;
         fresh ti (tb =/= ti) (ta =/= ti) (ta =/= tb)
           (JGS_Helpers.only_classes_interfaces_and_arrays ti)
           (direct_subtyping ta ti)
           (( <=< ) ~direct_subtyping ~constr ti tb);
       ])

let ( <~< ) ~direct_subtyping ~constr ta tb =
  debug_var ta (Fun.flip JGS.HO.jtype_reify) (fun reified_ta ->
      debug_var tb (Fun.flip JGS.HO.jtype_reify) (fun reified_tb ->
          match (reified_ta, reified_tb) with
          | [ Value _ ], _ -> ( <=< ) ~direct_subtyping ~constr ta tb
          | _ -> ( <-< ) ~direct_subtyping ~constr ta tb))

let make_closure_by_closure_template closure_template (module CT : SCT)
    direct_subtyping =
  let rec is_correct ~closure_type ?(constr = success) t =
    is_correct_type
      (module CT)
      ~closure_subtyping:(closure ~closure_type ~constr)
      t
  and direct ~closure_type ?(constr = success) ta tb =
    ( -<- )
      (module CT)
      ~direct_subtyping
      ~closure_subtyping:(closure ~closure_type ~constr)
      ~is_correct_type:(is_correct ~closure_type) ta tb
  and closure ~closure_type ?(constr = success) ta tb st =
    closure_template closure_type
      ~direct_subtyping:(direct ~closure_type ~constr)
      ~constr ta tb st
  in
  { is_correct_type = is_correct; direct_subtyping = direct; closure }

let make_closure (module CT : SCT) =
  if !need_dynamic_closure then
    make_closure_by_closure_template (fun _ -> ( <~< )) (module CT)
  else
    make_closure_by_closure_template
      (function Subtyping -> ( <-< ) | Supertyping -> ( <=< ))
      (module CT)
