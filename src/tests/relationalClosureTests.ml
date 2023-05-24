open OCanren
open OCanren.Std
open JGS_lib.JGS
open JGS_lib.JGS_Helpers
open JGS_lib.MutableTypeTable

let _ =
  let module SampleCT = SampleCT () in
  let module V = FO.Verifier (SampleCT) in
  let rec ( <-< ) ta tb b =
    conde [ (ta -<- tb) b; fresh ti ((tb -<- ti) b) ((ti <-< ta) b) ]
  and ( -<- ) ta tb b = V.( -<- ) ( <-< ) ta tb b in

  let run_jtype ?(n = -1) query =
    let pp_list f l =
      Printf.sprintf "\n[\n  %s\n]%!"
      @@ String.concat ";\n  " @@ Stdlib.List.map f l
    in
    pp_list pp_ljtype @@ Stream.take ~n
    @@ run q query (fun q -> q#reify HO.jtype_reify)
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
  let _c = Class (class_c, []) in
  Printf.printf "Class C: %d\n\n" class_c;

  Printf.printf "%s\n"
  @@ run_jtype ~n:10 (fun q -> fresh (x y t1) ((q <-< jtype_inj a) !!true))
