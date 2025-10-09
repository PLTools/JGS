open OCanren
open JGS
open Mutable_type_table
open JGS_Helpers

let enabled_tests : (unit -> unit) list ref = ref []
let extend f = enabled_tests := f :: !enabled_tests

let _ =
  let module SampleCT = SampleCT () in
  let module V = FO.Verifier (SampleCT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  let run_jtype ?(n = -1) query =
    let pp_list f l =
      Printf.sprintf "\n[\n  %s\n]%!"
      @@ String.concat ";\n  " @@ Stdlib.List.map f l
    in
    pp_list pp_ljtype @@ Stream.take ~n
    @@ run q query (fun q -> q#reify HO.jtype_reify)
  in

  let test1_1 () =
    Printf.printf "1.1 (?) < Object : %s\n"
    @@ run_jtype ~n:3 (fun q ->
           fresh ()
             (only_classes_interfaces_and_arrays q)
             (( -<- ) q (jtype_inj @@ SampleCT.object_t) !!true));

    sep ()
  in
  let test1_2 () =
    Printf.printf "1.2 Object[] < (?) : %s\n"
    @@ run_jtype ~n:(-1) (fun q ->
           ( -<- ) (jtype_inj @@ Array SampleCT.object_t) q !!true);
    sep ()
  in
  let test2 () =
    Printf.printf "2 (?) < Cloneable : %s\n"
    @@ run_jtype ~n:3 (fun q ->
           fresh ()
             (only_classes_interfaces_and_arrays q)
             (( -<- ) q (jtype_inj @@ SampleCT.cloneable_t) !!true));
    sep ()
  in
  let test3 () =
    Printf.printf "3 (?) < Serializable : %s\n"
    @@ run_jtype ~n:10 (fun q ->
           ( -<- ) q (jtype_inj @@ SampleCT.serializable_t) !!true);
    sep ()
  in
  let test4_1 () =
    Printf.printf "4.1 (?) < Object[] : %s\n"
    @@ run_jtype ~n:10 (fun q ->
           fresh ()
             (only_classes_interfaces_and_arrays q)
             (( -<- ) q (jtype_inj @@ Array SampleCT.object_t) !!true));
    sep ()
  in
  let test4_2 () =
    Printf.printf "4.2 Object < (?) : %s\n"
    @@ run_jtype ~n:(-1) (fun q ->
           ( -<- ) (jtype_inj @@ SampleCT.object_t) q !!true);
    sep ()
  in

  let test5 () =
    Printf.printf "5 Cloneable < (?): %s\n"
    @@ run_jtype ~n:(-1) (fun q ->
           ( -<- ) (jtype_inj @@ SampleCT.cloneable_t) q !!true);
    sep ()
  in

  let test6 () =
    Printf.printf "6 Serializable < (?) : %s\n"
    @@ run_jtype ~n:(-1) (fun q ->
           ( -<- ) (jtype_inj @@ SampleCT.serializable_t) q !!true);
    sep ()
  in

  let test7_1 () =
    Printf.printf "7.1 (?) < Serializable[] : %s\n"
    @@ run_jtype ~n:10 (fun q ->
           fresh ()
             (only_classes_interfaces_and_arrays q)
             (( -<- ) q (jtype_inj @@ Array SampleCT.serializable_t) !!true));
    sep ()
  in

  let test7_2 () =
    Printf.printf "7.2 Object[][] < (?) : %s\n"
    @@ run_jtype ~n:(-1) (fun q ->
           ( -<- ) (jtype_inj @@ Array (Array SampleCT.object_t)) q !!true);
    sep ()
  in
  (* class A {...} *)
  let class_a = SampleCT.make_class [] SampleCT.object_t [] in

  (* class B extends A {...} *)
  let class_b = SampleCT.make_class [] (Class (class_a, [])) [] in

  Printf.printf "Class A: %d\n\n" class_a;

  Printf.printf "Class B: %d\n\n" class_b;

  let test8_1 () =
    Printf.printf "8.1 (?) < A : %s\n"
    @@ run_jtype ~n:3 (fun q ->
           fresh ()
             (only_classes_interfaces_and_arrays q)
             (( -<- ) q (jtype_inj @@ Class (class_a, [])) !!true));
    sep ()
  in

  let test8_2 () =
    Printf.printf "8.2 B < (?) : %s\n"
    @@ run_jtype ~n:(-1) (fun q ->
           ( -<- ) (jtype_inj @@ Class (class_b, [])) q !!true);
    sep ()
  in

  let test8_3 () =
    Printf.printf "8.3 (?) < B : %s\n"
    @@ run_jtype ~n:2 (fun q ->
           fresh ()
             (only_classes_interfaces_and_arrays q)
             (( -<- ) q (jtype_inj @@ Class (class_b, [])) !!true));
    sep ()
  in

  let test8_4 () =
    Printf.printf "8.4 A < (?) : %s\n"
    @@ run_jtype ~n:(-1) (fun q ->
           ( -<- ) (jtype_inj @@ Class (class_a, [])) q !!true);
    sep ()
  in

  (* interface IA {...} *)
  let intf_a = SampleCT.make_interface [] [] in

  (* class C extends A implements IA {...} *)
  let class_c =
    SampleCT.make_class [] (Class (class_a, [])) [ Interface (intf_a, []) ]
  in

  Printf.printf "Interface A: %d\n\n" intf_a;

  Printf.printf "Class C: %d\n\n" class_c;

  let test9 () =
    Printf.printf "9 C < (?) : %s\n"
    @@ run_jtype ~n:(-1) (fun q ->
           ( -<- ) (jtype_inj @@ Class (class_c, [])) q !!true);
    sep ()
  in

  let test10_1 () =
    Printf.printf "10.1 (?) < IA : %s\n"
    @@ run_jtype ~n:3 (fun q ->
           fresh ()
             (only_classes_interfaces_and_arrays q)
             (( -<- ) q (jtype_inj @@ Interface (intf_a, [])) !!true));
    sep ()
  in

  let test10_2 () =
    Printf.printf "10.2 C < (?) : %s\n"
    @@ run_jtype ~n:(-1) (fun q ->
           ( -<- ) (jtype_inj @@ Class (class_c, [])) q !!true);
    sep ()
  in

  (* interface IB extends IA {...} *)
  let intf_b = SampleCT.make_interface [] [ Interface (intf_a, []) ] in

  Printf.printf "Interface B: %d\n\n" intf_b;

  let test11 () =
    Printf.printf "11 IB < (?) : %s\n"
    @@ run_jtype ~n:(-1) (fun q ->
           ( -<- ) (jtype_inj @@ Interface (intf_b, [])) q !!true);
    sep ()
  in
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
  Printf.printf "Class D<X>: %d\n\n" class_d;
  Printf.printf "Class E<X, Y>: %d\n\n" class_e;
  Printf.printf "Class F<X, Y>: %d\n\n" class_f;

  let f_ab =
    Class (class_f, [ Type (Class (class_a, [])); Type (Class (class_b, [])) ])
  in

  let e_d_b_a =
    Class
      ( class_e,
        [
          Type (Class (class_d, [ Type (Class (class_b, [])) ]));
          Type (Class (class_a, []));
        ] )
  in

  let test12_1 () =
    Printf.printf "12.1 (?) < E<D<B>, A> : %s\n"
    @@ run_jtype ~n:2 (fun q ->
           fresh ()
             (only_classes_interfaces_and_arrays q)
             (( -<- ) q (jtype_inj @@ e_d_b_a) !!true));
    sep ()
  in

  let test12_2 () =
    Printf.printf "12.2 (? - is class) < E<D<B>, A> : %s\n"
    @@ run_jtype ~n:1 (fun q ->
           fresh (a b)
             (q === !!(HO.Class (a, b)))
             (( -<- ) q (jtype_inj @@ e_d_b_a) !!true));
    sep ()
  in

  let test12_3 () =
    Printf.printf "12.3 F<A, B> < (?) : %s\n"
    @@ run_jtype ~n:(-1) (fun q -> ( -<- ) (jtype_inj @@ f_ab) q !!true)
  in

  let mk name f = ("-" ^ name, Arg.Unit (fun () -> extend f), "") in
  let all_args =
    [
      mk "1_1" test1_1;
      mk "1_2" test1_2;
      mk "2" test2;
      mk "3" test3;
      mk "4_1" test4_1;
      mk "4_2" test4_2;
      mk "5" test5;
      mk "6" test6;
      mk "7_1" test7_1;
      mk "7_2" test7_2;
      mk "8_1" test8_1;
      mk "8_2" test8_2;
      mk "8_3" test8_3;
      mk "8_4" test8_4;
      mk "9" test9;
      mk "10_1" test10_1;
      mk "10_2" test10_2;
      mk "11" test11;
      mk "12_1" test12_1;
      mk "12_2" test12_2;
      mk "12_3" test12_3;
    ]
    |> List.rev
  in
  Arg.parse all_args
    (function
      | "all" ->
          Stdlib.List.iter
            (function _, Arg.Unit f, _ -> f () | _ -> ())
            all_args
      | s -> Printf.eprintf "What to do with '%s'\n%!" s)
    " "

let () = Stdlib.List.iter (fun f -> f ()) !enabled_tests
