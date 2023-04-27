open OCanren
open OCanren.Std
open JGS
open JGS_Helpers
open MutableTypeTable

let _ =
  let module SampleCT = SampleCT () in
  let module V = FO.Verifier (SampleCT) in
  let rec is_correct_type t =
    conde
      [
        fresh (id index upb lwb)
          (t === !!(HO.Var { id; index; upb; lwb = some lwb }))
          (lwb -<- upb);
        fresh (id index upb) (t === !!(HO.Var { id; index; upb; lwb = none () }));
        ( wc @@ fun id ->
          wc @@ fun index ->
          wc @@ fun upb ->
          wc @@ fun lwb -> t =/= !!(HO.Var { id; index; upb; lwb }) );
      ]
  and ( <-< ) ta tb b =
    fresh () (b === !!true)
      (conde [ ta === tb; fresh ti (tb -<- ti) ((ti <-< ta) b) ])
  and ( -<- ) ta tb =
    fresh ()
      (V.( -<- ) ( <-< ) ta tb !!true)
      (is_correct_type ta) (is_correct_type tb)
  in

  let _run_jtype ?(n = -1) query =
    let pp_list f l =
      Printf.sprintf "\n[\n  %s\n]%!"
      @@ String.concat ";\n  " @@ Stdlib.List.map f l
    in
    pp_list pp_ljtype @@ Stream.take ~n
    @@ run q query (fun q -> q#reify HO.jtype_reify)
  in

  let run_jtypes ?(n = -1) query =
    let pp_list f l =
      Printf.sprintf "\n[\n  %s\n]%!"
      @@ String.concat ";\n  " @@ Stdlib.List.map f l
    in
    pp_list (GT.show Std.List.logic pp_ljtype)
    @@ Stream.take ~n
    @@ run q query (fun q -> q#reify (Std.List.reify HO.jtype_reify))
  in

  let class_a = SampleCT.make_class [] SampleCT.object_t [] in
  let a = Class (class_a, []) in
  Printf.printf "Class A: %d\n\n" class_a;

  let class_a1 = SampleCT.make_class [] SampleCT.object_t [] in
  let _a1 = Class (class_a1, []) in
  Printf.printf "Class A1: %d\n\n" class_a1;

  let class_b = SampleCT.make_class [] a [] in
  let b = Class (class_b, []) in
  Printf.printf "Class B: %d\n\n" class_b;

  let class_c = SampleCT.make_class [] b [] in
  let c = Class (class_c, []) in
  Printf.printf "Class C: %d\n\n" class_c;

  Printf.printf "%s\n"
  @@ run_jtypes ~n:1 (fun q ->
         fresh (super sub t1)
           (super === jtype_inj a)
           (sub === jtype_inj c)
           (q === Std.list Fun.id [ super; t1; sub ])
           (t1 -<- super) (sub -<- t1))
