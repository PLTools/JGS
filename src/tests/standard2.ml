open OCanren
open JGS_Helpers
open MutableTypeTable
open Closure

[@@@ocaml.warning "-unused-value-declaration"]

(* Verifier modules *)
module SampleCT = SampleCT ()
module V = JGS.FO.Verifier (SampleCT)

let { closure = ( <-< ); _ } =
  make_closure_subtyping (module SampleCT) V.( -<- )

let { closure = ( <=< ); _ } =
  make_closure_supertyping (module SampleCT) V.( -<- )

module JGS_builder = struct
  module M = Map.Make (Int)

  type tested_type = Simple of JGS.jtype | Generic of (JGS.jtype -> JGS.jtype)

  let names_by_id =
    ref @@ M.add 1 "Object" @@ M.add 2 "Cloneable"
    @@ M.add 3 "Serializable" M.empty

  let get_name : int logic -> string = function
    | Var _ -> "*ANY*"
    | Value id -> M.find id !names_by_id

  let apply_type t arg =
    match (t, arg) with
    | Generic f, Simple a -> Simple (f a)
    | _ -> failwith "Incorrect type applying"

  let pp_jtype = pp_jtyp_logic get_name

  let mk_simple_inteface ~supers name =
    let supers =
      List.map
        (function
          | Simple s -> s
          | _ -> failwith "Generic parameter of simple interface")
        supers
    in
    let id = SampleCT.make_interface ~name [] supers in
    names_by_id := M.add id name !names_by_id;
    Simple (JGS.Interface (id, []))

  let mk_simple_class ?(super = Simple SampleCT.object_t) ~supers name =
    let supers =
      List.map
        (function
          | Simple s -> s | _ -> failwith "Generic parent of simple class")
        supers
    in
    let super =
      match super with
      | Simple s -> s
      | _ -> failwith "Generic parameter of simple class"
    in
    let id = SampleCT.make_class ~name [] super supers in
    names_by_id := M.add id name !names_by_id;
    Simple (JGS.Class (id, []))

  let mk_generic_interface ~supers name =
    let var = SampleCT.make_tvar 0 SampleCT.object_t in
    let supers =
      List.map (function Simple s -> s | Generic f -> f var) supers
    in
    let id = SampleCT.make_interface ~name [ var ] supers in
    names_by_id := M.add id name !names_by_id;
    Generic (fun var -> JGS.Interface (id, [ JGS.Type var ]))

  let mk_generic_class ?(super = Simple SampleCT.object_t) ~supers name =
    let var = SampleCT.make_tvar 0 SampleCT.object_t in
    let supers =
      List.map (function Simple s -> s | Generic f -> f var) supers
    in
    let super = match super with Simple s -> s | Generic f -> f var in
    let id = SampleCT.make_class ~name [ var ] super supers in
    names_by_id := M.add id name !names_by_id;
    Generic (fun var -> JGS.Class (id, [ JGS.Type var ]))

  let obj = Simple SampleCT.object_t
  let cloneable = Simple SampleCT.cloneable_t
  let serializable = Simple SampleCT.serializable_t
end

module CollectionClasses = struct
  open JGS_builder

  let get_simple_type = function
    | Simple t -> t
    | _ -> failwith "Isn't simple class or interface"

  let get_generic_type = function
    | Generic f -> f
    | _ -> failwith "Isn't generic class or interface"

  (* let int = mk_simple_class "Int" ~supers:[] *)
  (* let serializable = mk_simple_inteface "java.io.Serializable" ~supers:[] *)
  let comparable =
    let name = "java.lang.Comparable" in

    let id =
      SampleCT.make_interface_fix ~name
        (fun _ -> [ SampleCT.make_tvar 0 SampleCT.object_t ])
        (fun _self_id -> [])
    in
    names_by_id := M.add id name !names_by_id;
    Generic (fun var -> JGS.Interface (id, [ JGS.Type var ]))

  let collection =
    let name = "ICollection" in

    let id =
      SampleCT.make_interface_fix ~name
        (fun _ -> [ SampleCT.make_tvar 0 SampleCT.object_t ])
        (fun _self_id -> [])
    in
    names_by_id := M.add id name !names_by_id;
    Generic (fun var -> JGS.Interface (id, [ JGS.Type var ]))

  let string =
    let name = "java.lang.String" in
    let id =
      SampleCT.make_class_fix ~name
        ~params:(fun _ -> [])
        (fun _ -> SampleCT.object_t)
        (fun self_id ->
          [
            get_simple_type serializable;
            get_generic_type comparable (JGS.Class (self_id, []));
          ])
    in
    names_by_id := M.add id name !names_by_id;
    Simple (JGS.Class (id, []))

  let list =
    let name = "java.util.List" in

    (* let id =
         SampleCT.make_interface_fix ~name
           (fun _ -> [ SampleCT.make_tvar ~name:"A" 0 SampleCT.object_t ])
           (fun _self_id ->
             let v = SampleCT.make_tvar ~name:"B" 0 SampleCT.object_t in
             [ get_generic_type collection v ])
       in *)
    let var1 = ref (Obj.magic ()) in
    (* TODO: This is full of shit *)
    let id =
      SampleCT.make_interface_fix ~name
        (fun _ ->
          var1 := SampleCT.make_tvar ~name:"A" 0 SampleCT.object_t;
          [ !var1 ])
        (fun _self_id ->
          (* let v = SampleCT.make_tvar ~name:"B" 0 SampleCT.object_t in *)
          [ get_generic_type collection !var1 ])
    in
    names_by_id := M.add id name !names_by_id;
    Generic (fun var -> JGS.Interface (id, [ JGS.Type var ]))
  (* mk_generic_interface "java.util.List" ~supers:[ collection ] *)

  let protocol_string_list =
    mk_simple_class
      "kotlin.reflect.jvm.internal.impl.protobuf.ProtocolStringList"
      ~super:(Simple (get_generic_type list (get_simple_type string)))
      ~supers:[]

  module Types = struct
    (* let int = get_simple_type int *)
    let string = get_simple_type string
    let obj = SampleCT.object_t
    let cloneable = SampleCT.cloneable_t
    let serializable = SampleCT.serializable_t
    let collection = get_generic_type collection
    let list = get_generic_type list
  end
end

(* let () =
   let wrap n =
     Format.printf "decl %d: %a\n" n JGS_Helpers.JGS_PP.decl
       (SampleCT.decl_by_id n)
   in
   wrap 4;
   wrap 6;
   wrap 8;
   wrap 9 *)

let _ =
  let open CollectionClasses.Types in
  let pp_list f l =
    let open Stdlib in
    let rec is_first index elem l =
      match (index, l) with
      | 0, _ :: _ -> true
      | _, x :: _ when x = elem -> false
      | n, _ :: xs -> is_first (n - 1) elem xs
      | _ -> failwith "Out of bounds"
    in

    List.iteri
      (fun i a ->
        Format.printf "%s %a\n" (if is_first i a l then "  " else "--") f a)
      l;

    let counts =
      let module M = Map.Make (struct
        type t = JGS.HO.jtype_logic

        let compare = compare
      end) in
      List.sort (fun (_, c1) (_, c2) -> Int.compare c1 c2)
      @@ M.bindings
      @@ List.fold_left
           (fun acc e ->
             M.update e (function None -> Some 1 | Some n -> Some (n + 1)) acc)
           M.empty l
    in
    Format.printf "\n\nUniq answers:\n";
    List.iter
      (fun (ans, count) -> Format.printf "  %3d - %a\n" count f ans)
      counts;

    Format.printf "\nTotal answers amount:      %d\n" (List.length l);
    Format.printf "Total uniq answers amount: %d\n%!" (List.length counts)
  in

  let run_jtype ?(n = -1) ~msg query =
    sep ();
    Format.printf "%s, %s answers:\n" msg
      (if n < 0 then "all" else Int.to_string n);
    pp_list JGS_builder.pp_jtype
    @@ Stream.take ~n
    @@ run q query (fun q -> q#reify JGS.HO.jtype_reify)
  in

  let _ =
    let upper_bound1 = collection string in
    (* Format.printf "upper_boudn1: %a\n" JGS_Helpers.JGS_PP.jtyp upper_bound1; *)
    (* A remake of the test
       dune exec jsons/run_json.exe -- -n 5 jsons/8.json
    *)
    (* On three answers it hangs *)
    run_jtype ~n:3 ~msg:"? <-< Collection<String>" (fun q ->
        fresh () (q =/= !!JGS.HO.Null) (q <-< jtype_inj upper_bound1) (* *))
  in
  ()
