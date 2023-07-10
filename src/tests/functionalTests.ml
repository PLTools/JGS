open OCanren
open OCanren.Std
open JGS
open MutableTypeTable

let _ =
  let module SampleCT = SampleCT () in
  let module V = Verifier (SampleCT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  Printf.printf " 1 Object[] < Object (true) : %b\n"
    (Array SampleCT.object_t -<- SampleCT.object_t);
  Printf.printf " 2 Object[] < Cloneable (true) : %b\n"
    (Array SampleCT.object_t -<- SampleCT.cloneable_t);
  Printf.printf " 3 Object[] < Serializable (true) : %b\n"
    (Array SampleCT.object_t -<- SampleCT.serializable_t);

  Printf.printf " 4 Object < Object[] (false): %b\n"
    (SampleCT.object_t -<- Array SampleCT.object_t);
  Printf.printf " 5 Cloneable < Object[] (false): %b\n"
    (SampleCT.cloneable_t -<- Array SampleCT.object_t);
  Printf.printf " 6 Serializable < Object[] (false): %b\n"
    (SampleCT.serializable_t -<- Array SampleCT.object_t);

  Printf.printf " 7 Object[][] < Serializable[] (true) : %b\n"
    (Array (Array SampleCT.object_t) -<- Array SampleCT.serializable_t);

  (* class A {...} *)
  let class_a = SampleCT.make_class [] SampleCT.object_t [] in

  (* class B extends A {...} *)
  let class_b = SampleCT.make_class [] (Class (class_a, [])) [] in
  Printf.printf " 8 B < A (true) : %b\n"
    (Class (class_b, []) -<- Class (class_a, []));

  (* interface IA {...} *)
  let intf_a = SampleCT.make_interface [] [] in

  (* class C extends A implements IA {...} *)
  let class_c =
    SampleCT.make_class [] (Class (class_a, [])) [ Interface (intf_a, []) ]
  in
  Printf.printf " 9 C < A (true) : %b\n"
    (Class (class_c, []) -<- Class (class_a, []));
  Printf.printf "10 C < IA (true) : %b\n"
    (Class (class_c, []) -<- Interface (intf_a, []));

  (* interface IB extends IA {...} *)
  let intf_b = SampleCT.make_interface [] [ Interface (intf_a, []) ] in
  Printf.printf "11 IB < IA (true) : %b\n"
    (Interface (intf_b, []) -<- Interface (intf_a, []));

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
  Printf.printf "12 F<A, B> < E<D<B>, A> (true) : %b\n"
    (Class (class_f, [ Type (Class (class_a, [])); Type (Class (class_b, [])) ])
    -<- Class
          ( class_e,
            [
              Type (Class (class_d, [ Type (Class (class_b, [])) ]));
              Type (Class (class_a, []));
            ] ));

  let class_collection =
    SampleCT.make_class [ SampleCT.object_t ] (Class (1 (* object *), [])) []
  in

  Printf.printf "13 Collection<A> < Collection< VB extends Object> : %b\n"
    (Class (class_collection, [ Type (Class (class_a, [])) ])
    -<- Class
          ( class_collection,
            [
              Type
                (Var
                   { id = 101; lwb = None; index = 1; upb = SampleCT.object_t });
            ] ));

  Printf.printf "14 Collection<VA extends Object> < Collection< Object > : %b\n"
    (Class
       ( class_collection,
         [
           Type
             (Var { id = 100; lwb = None; index = 0; upb = SampleCT.object_t });
         ] )
    -<- Class (class_collection, [ Type (Class (1 (* object *), [])) ]))
