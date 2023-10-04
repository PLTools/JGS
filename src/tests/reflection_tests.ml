(* open OCanren
   open OCanren.Std
   open JGS
   open Mutable_type_table

   let _ =
     let module SampleCT = SampleCT () in
     let module V = Verifier (SampleCT) in
     let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
     and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

     let class_a = SampleCT.make_class [] SampleCT.object_t [] in
     let a = Class (class_a, []) in
     Printf.printf "Class A: %d\n" class_a;

     let class_a1 = SampleCT.make_class [] SampleCT.object_t [] in
     let a1 = Class (class_a1, []) in
     Printf.printf "Class A1: %d\n" class_a1;

     let class_b = SampleCT.make_class [] a [] in
     let b = Class (class_b, []) in
     Printf.printf "Class B: %d\n" class_b;

     let class_c = SampleCT.make_class [] b [] in
     let c = Class (class_c, []) in
     Printf.printf "Class C: %d\n\n" class_c;

     Printf.printf " 1 a  < a  (false) : %b\n" (a -<- a);
     Printf.printf " 2 b  < b  (false) : %b\n" (b -<- b);
     Printf.printf " 3 b  < a  (true)  : %b\n" (b -<- a);
     Printf.printf " 4 c  < a  (false) : %b\n" (c -<- a);
     Printf.printf " 5 a1 < a  (false) : %b\n" (a1 -<- a);
     Printf.printf " 6 a  < a1 (false) : %b\n" (a -<- a1);
     Printf.printf " 7 c  < b  (true)  : %b\n" (c -<- b);
     Printf.printf " 8 b  < c  (false) : %b\n" (b -<- c) *)
