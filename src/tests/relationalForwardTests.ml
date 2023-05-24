open OCanren
open JGS_lib.JGS
open JGS_lib.MutableTypeTable
open JGS_lib.JGS_Helpers

let _ =
  let module SampleCT = SampleCT () in
  let module V = FO.Verifier (SampleCT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  let run_bool query =
    [%show: GT.bool OCanren.logic GT.list] ()
    @@ Stream.take ~n:(-1)
    @@ run q query (fun q -> q#reify Std.Bool.reify)
  in

  Printf.printf " 1 Object[] < Object (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array SampleCT.object_t)
           (jtype_inj @@ SampleCT.object_t)
           q);

  Printf.printf " 2 Object[] < Cloneable (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array SampleCT.object_t)
           (jtype_inj @@ SampleCT.cloneable_t)
           q);

  Printf.printf " 3 Object[] < Serializable (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array SampleCT.object_t)
           (jtype_inj @@ SampleCT.serializable_t)
           q);

  Printf.printf " 4 Object < Object[] (false): %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ SampleCT.object_t)
           (jtype_inj @@ Array SampleCT.object_t)
           q);

  Printf.printf " 5 Cloneable < Object[] (false):%s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ SampleCT.cloneable_t)
           (jtype_inj @@ Array SampleCT.object_t)
           q);

  Printf.printf " 6 Serializable < Object[] (false): %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ SampleCT.serializable_t)
           (jtype_inj @@ Array SampleCT.object_t)
           q);

  Printf.printf " 7 Object[][] < Serializable[] (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array (Array SampleCT.object_t))
           (jtype_inj @@ Array SampleCT.serializable_t)
           q);

  (* class A {...} *)
  let class_a = SampleCT.make_class [] SampleCT.object_t [] in

  (* class B extends A {...} *)
  let class_b = SampleCT.make_class [] (Class (class_a, [])) [] in
  Printf.printf " 8 B < A (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Class (class_b, []))
           (jtype_inj @@ Class (class_a, []))
           q);

  (* interface IA {...} *)
  let intf_a = SampleCT.make_interface [] [] in

  (* class C extends A implements IA {...} *)
  let class_c =
    SampleCT.make_class [] (Class (class_a, [])) [ Interface (intf_a, []) ]
  in
  Printf.printf " 9 C < A (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Class (class_c, []))
           (jtype_inj @@ Class (class_a, []))
           q);

  Printf.printf "10 C < IA (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Class (class_c, []))
           (jtype_inj @@ Interface (intf_a, []))
           q);

  (* interface IB extends IA {...} *)
  let intf_b = SampleCT.make_interface [] [ Interface (intf_a, []) ] in
  Printf.printf "11 IB < IA (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Interface (intf_b, []))
           (jtype_inj @@ Interface (intf_a, []))
           q);

  (* class D<X> {...} *)
  let class_d =
    SampleCT.make_class [ SampleCT.object_t ] SampleCT.object_t []
  in

  (* class E<X, Y> {...} *)
  let class_e =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      SampleCT.object_t []
  in

  (* class F<X, Y> extends E<D<Y>, X> {...} *)
  let class_f =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      (Class
         ( class_e,
           [
             Type
               (Class
                  (class_d, [ Type (SampleCT.make_tvar 1 SampleCT.object_t) ]));
             Type (SampleCT.make_tvar 0 SampleCT.object_t);
           ] ))
      []
  in
  Printf.printf "12 F<A, B> < E<D<B>, A> (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj
           @@ Class
                ( class_f,
                  [ Type (Class (class_a, [])); Type (Class (class_b, [])) ] ))
           (jtype_inj
           @@ Class
                ( class_e,
                  [
                    Type (Class (class_d, [ Type (Class (class_b, [])) ]));
                    Type (Class (class_a, []));
                  ] ))
           q)
