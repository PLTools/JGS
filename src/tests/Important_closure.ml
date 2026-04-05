open OCanren
open JGS
module SampleCT = Mutable_type_table.SampleCT ()

(* Incomplete implementation of transitive subtyping *)
let _ =
  let module V = Verifier (SampleCT) in
  let rec ( <-< ) ta tb = ta -<- tb
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  Printf.printf " 1 Object[] < Object (true) : %b\n"
    (Array SampleCT.object_t -<- SampleCT.object_t);

  (* class A {...} *)
  let class_a = SampleCT.make_class ~name:"A" [] SampleCT.object_t [] in
  Printf.printf " 8 A < Object (true): %b\n"
    (Class (class_a, []) -<- SampleCT.object_t);
  (* class B extends A {...} *)
  let class_b = SampleCT.make_class ~name:"B" [] (Class (class_a, [])) [] in
  Printf.printf " 8 B < A (true) : %b\n"
    (Class (class_b, []) -<- Class (class_a, []));

  Printf.printf
    " 8.1 B < Object (false because of simple implementation) : %b\n"
    (Class (class_b, []) -<- SampleCT.object_t)

(* Copy-paste from standard.ml  *)
module JGS_builder = struct
  module M = Map.Make (Int)

  type tested_type = JGS.jtype

  let names_by_id =
    ref @@ M.add 1 "Object" @@ M.add 2 "Cloneable"
    @@ M.add 3 "Serializable" M.empty

  let get_name : int logic -> string = function
    | Var _ -> "*ANY*"
    | Value id -> M.find id !names_by_id

  let pp_jtype = JGS_Helpers.pp_jtyp_logic get_name

  let mk_simple_class ?(super = SampleCT.object_t) ~supers name =
    let id = SampleCT.make_class ~name [] super supers in
    names_by_id := M.add id name !names_by_id;
    JGS.Class (id, [])

  let obj = SampleCT.object_t
end

let () =
  let module V = JGS.FO.Verifier (SampleCT) in
  let ( <-< ) =
    let { Closure.closure; _ } =
      Closure.make_closure (module SampleCT) V.( -<- )
    in
    closure ~closure_type:Subtyping
  in

  let pp_list xs =
    Format.printf "%a" (Format.pp_print_list JGS_builder.pp_jtype) xs
  in
  let run_jtype ?(n = -1) ~msg query =
    Format.printf "%s, %s answers:\n" msg
      (if n < 0 then "all" else Int.to_string n);
    pp_list @@ Stream.take ~n
    @@ run q query (fun q -> q#reify JGS.HO.jtype_reify)
  in

  let class_a = JGS_builder.mk_simple_class ~supers:[] "A" in
  let class_b = JGS_builder.mk_simple_class ~super:class_a ~supers:[] "B" in

  run_jtype ~n:3 ~msg:"B <-< Object (new implementation)" (fun _q ->
      (* Prints free variable on success *)
      JGS_Helpers.jtype_inj class_b <-< JGS_Helpers.jtype_inj JGS_builder.obj)
