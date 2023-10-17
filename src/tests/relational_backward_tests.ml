open OCanren
open JGS
open Mutable_type_table
open JGS_Helpers
open Test_printer

let _ =
  let module SampleCT = SampleCT () in
  let module V = Verifier (SampleCT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  run_jtype ~n:(-1) ~msg:"1.1 (?) -<- Object" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) q SampleCT.object_t !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"1.2 Object[] -<- (?)" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) (Jtype.array SampleCT.object_t) q !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"2 (?) -<- Cloneable" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) q SampleCT.cloneable_t !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"3 (?) -<- Serializable" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) q SampleCT.serializable_t !!true));
  sep ();

  run_jtype ~n:10 ~msg:"4.1 (?) -<- Object[]" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) q (Jtype.array SampleCT.object_t) !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"4.2 Object -<- (?)" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) SampleCT.object_t q !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"5 Cloneable -<- (?)" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) SampleCT.cloneable_t q !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"6 Serializable -<- (?)" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) SampleCT.serializable_t q !!true));
  sep ();

  run_jtype ~n:10 ~msg:"7.1 (?) -<- Serializable[]" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) q (Jtype.array SampleCT.serializable_t) !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"7.2 Object[][] -<- (?)" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) (Jtype.array (Jtype.array SampleCT.object_t)) q !!true));
  sep ();

  (* class A {...} *)
  let class_a = SampleCT.make_class [] SampleCT.Ground.object_t [] in

  (* class B extends A {...} *)
  let class_b = SampleCT.make_class [] (Class (class_a, [])) [] in

  Printf.printf "Class A: %d\n\n" class_a;

  Printf.printf "Class B: %d\n\n" class_b;

  run_jtype ~n:(-1) ~msg:"8.1 (?) -<- A" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) q (jtype_inj @@ Class (class_a, [])) !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"8.2 B -<- (?)" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) (jtype_inj @@ Class (class_b, [])) q !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"8.3 (?) -<- B" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) q (jtype_inj @@ Class (class_b, [])) !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"8.4 A -<- (?)" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) (jtype_inj @@ Class (class_a, [])) q !!true));
  sep ();

  (* interface IA {...} *)
  let intf_a = SampleCT.make_interface [] [] in

  (* class C extends A implements IA {...} *)
  let class_c =
    SampleCT.make_class [] (Class (class_a, [])) [ Interface (intf_a, []) ]
  in

  Printf.printf "Interface A: %d\n\n" intf_a;

  Printf.printf "Class C: %d\n\n" class_c;

  run_jtype ~n:(-1) ~msg:"9 C -<- (?)" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) (jtype_inj @@ Class (class_c, [])) q !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"10.1 (?) -<- IA" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) q (jtype_inj @@ Interface (intf_a, [])) !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"10.2 IA -<- (?)" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) (jtype_inj @@ Interface (intf_a, [])) q !!true));
  sep ();

  (* interface IB extends IA {...} *)
  let intf_b = SampleCT.make_interface [] [ Interface (intf_a, []) ] in

  Printf.printf "Interface B: %d\n\n" intf_b;

  run_jtype ~n:(-1) ~msg:"11 IB -<- (?)" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) (jtype_inj @@ Interface (intf_b, [])) q !!true));
  sep ();

  (* class D<X> {...} *)
  let class_d =
    SampleCT.make_class [ SampleCT.Ground.object_t ] SampleCT.Ground.object_t []
  in

  (* class E<X, Y> {...} *)
  let class_e =
    SampleCT.make_class
      [ SampleCT.Ground.object_t; SampleCT.Ground.object_t ]
      SampleCT.Ground.object_t []
  in

  (* class F<X, Y> extends E<D<Y>, X> {...} *)
  let class_f =
    SampleCT.make_class
      [ SampleCT.Ground.object_t; SampleCT.Ground.object_t ]
      (Class
         ( class_e,
           [
             Type
               (Class
                  ( class_d,
                    [ Type (SampleCT.make_tvar 1 SampleCT.Ground.object_t) ] ));
             Type (SampleCT.make_tvar 0 SampleCT.Ground.object_t);
           ] ))
      []
  in
  Printf.printf "Class D<X>: %d\n\n" class_d;
  Printf.printf "Class E<X, Y>: %d\n\n" class_e;
  Printf.printf "Class F<X, Y>: %d\n\n" class_f;

  let f_ab =
    Jtype.Class
      ( class_f,
        [ Targ.Type (Jtype.Class (class_a, [])); Type (Class (class_b, [])) ] )
  in

  let e_d_b_a =
    Jtype.Class
      ( class_e,
        [
          Targ.Type
            (Jtype.Class (class_d, [ Targ.Type (Jtype.Class (class_b, [])) ]));
          Type (Class (class_a, []));
        ] )
  in

  run_jtype ~n:(-1) ~msg:"12.1 (?) -<- E<D<B>, A>" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) q (jtype_inj @@ e_d_b_a) !!true));
  sep ();

  run_jtype ~n:(-1) ~msg:"12.2 F<A, B> -<- (?)" (fun q ->
      fresh ()
        (only_classes_interfaces_and_arrays q)
        (( -<- ) (jtype_inj @@ f_ab) q !!true))
