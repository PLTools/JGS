open OCanren
open JGS_Helpers
open Mutable_type_table
open Closure

(* Verifier modules *)
module SampleCT = SampleCT ()
module V = JGS.Verifier (SampleCT)

let { closure; _ } = make_closure (module SampleCT) V.( -<- )
let ( <-< ) = closure ~closure_type:Subtyping
let ( <=< ) = closure ~closure_type:Supertyping

module JGS_builder = struct
  module M = Map.Make (Int)

  type tested_type =
    | Simple of int JGS.Jtype.ground
    | Generic of (int JGS.Jtype.ground -> int JGS.Jtype.ground)

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
    let id = SampleCT.make_interface [] supers in
    names_by_id := M.add id name !names_by_id;
    Simple (JGS.Jtype.Interface (id, []))

  let mk_simple_class ?(super = Simple SampleCT.Ground.object_t) ~supers name =
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
    let id = SampleCT.make_class [] super supers in
    names_by_id := M.add id name !names_by_id;
    Simple (JGS.Jtype.Class (id, []))

  let mk_generic_interface ~supers name =
    let var = SampleCT.make_tvar 0 SampleCT.Ground.object_t in
    let supers =
      List.map (function Simple s -> s | Generic f -> f var) supers
    in
    let id = SampleCT.make_interface [ var ] supers in
    names_by_id := M.add id name !names_by_id;
    Generic (fun var -> JGS.Jtype.Interface (id, [ JGS.Targ.Type var ]))

  let mk_generic_class ?(super = Simple SampleCT.Ground.object_t) ~supers name =
    let var = SampleCT.make_tvar 0 SampleCT.Ground.object_t in
    let supers =
      List.map (function Simple s -> s | Generic f -> f var) supers
    in
    let super = match super with Simple s -> s | Generic f -> f var in
    let id = SampleCT.make_class [ var ] super supers in
    names_by_id := M.add id name !names_by_id;
    Generic (fun var -> JGS.Jtype.Class (id, [ JGS.Targ.Type var ]))

  let obj = Simple SampleCT.Ground.object_t
  let cloneable = Simple SampleCT.Ground.cloneable_t
  let serializable = Simple SampleCT.Ground.serializable_t
end

module CollectionClasses = struct
  open JGS_builder

  let get_simple_type = function
    | Simple t -> t
    | _ -> failwith "Isn't simple class or interface"

  let get_generic_type = function
    | Generic f -> f
    | _ -> failwith "Isn't generic class or interface"

  let int = mk_simple_class "Int" ~supers:[]
  let float = mk_simple_class "Float" ~supers:[]
  let string = mk_simple_class "String" ~supers:[]
  let iterable = mk_generic_interface "Iterable" ~supers:[]
  let collection = mk_generic_interface "Collection" ~supers:[ iterable ]

  let abstract_collection =
    mk_generic_class "AbstactCollection" ~supers:[ collection ]

  let list = mk_generic_interface "List" ~supers:[ collection ]

  let abstract_list =
    mk_generic_class "AbstractList" ~super:abstract_collection ~supers:[ list ]

  let random_access = mk_simple_inteface "RandomAccess" ~supers:[]

  let array_list =
    mk_generic_class "ArrayList" ~super:abstract_list
      ~supers:[ list; random_access; cloneable; serializable ]

  let attribute_list =
    mk_simple_class "AttributeList"
      ~super:(apply_type array_list obj)
      ~supers:[]

  let role_list =
    mk_simple_class "RoleList" ~super:(apply_type array_list obj) ~supers:[]

  let vector =
    mk_generic_class "Vector" ~super:abstract_list
      ~supers:[ list; random_access; cloneable; serializable ]

  let stack = mk_generic_class "Stack" ~super:vector ~supers:[]

  let abstract_sequential_list =
    mk_generic_class "AbstractSequentialList" ~super:abstract_list ~supers:[]

  let queue = mk_generic_interface "Queue" ~supers:[ collection ]
  let deque = mk_generic_interface "Deque" ~supers:[ queue ]

  let linked_list =
    mk_generic_class "LinkedList" ~super:abstract_sequential_list
      ~supers:[ list; deque; cloneable; serializable ]

  let set = mk_generic_interface "Set" ~supers:[ collection ]

  let abstract_set =
    mk_generic_class "AbstractSet" ~super:abstract_collection ~supers:[ set ]

  let hash_set =
    mk_generic_class "HashSet" ~super:abstract_set
      ~supers:[ set; cloneable; serializable ]

  let sorted_set = mk_generic_interface "SortedSet" ~supers:[ set ]
  let navigable_set = mk_generic_interface "NavigableSet" ~supers:[ sorted_set ]

  let tree_set =
    mk_generic_class "TreeSet" ~super:abstract_set
      ~supers:[ navigable_set; cloneable; serializable ]

  let concurrent_skip_list_set =
    mk_generic_class "ConcurrentSkipListSet" ~super:abstract_set
      ~supers:[ navigable_set; cloneable; serializable ]

  let linked_hash_set =
    mk_generic_class "LinkedHashSet" ~super:hash_set
      ~supers:[ set; cloneable; serializable ]

  let abstract_queue =
    mk_generic_class "AbstractQueue" ~super:abstract_collection
      ~supers:[ queue ]

  let blocking_queue =
    mk_generic_interface "BlockingQueue" ~supers:[ queue; serializable ]

  let array_blocking_queue =
    mk_generic_class "ArrayBlockingQueue" ~super:abstract_queue
      ~supers:[ blocking_queue ]

  let blocking_deque =
    mk_generic_interface "BlockingDeque" ~supers:[ blocking_queue; deque ]

  let linked_blocking_deque =
    mk_generic_class "LinkedBlockingDeque" ~super:abstract_queue
      ~supers:[ blocking_deque; serializable ]

  module Types = struct
    let int = get_simple_type int
    let float = get_simple_type float
    let string = get_simple_type string
    let obj = SampleCT.Ground.object_t
    let cloneable = SampleCT.Ground.cloneable_t
    let serializable = SampleCT.Ground.serializable_t
    let iterable = get_generic_type iterable
    let collection = get_generic_type collection
    let abstract_collection = get_generic_type abstract_collection
    let list = get_generic_type list
    let abstract_list = get_generic_type abstract_list
    let random_access = get_simple_type random_access
    let array_list = get_generic_type array_list
    let attribute_list = get_simple_type attribute_list
    let role_list = get_simple_type role_list
    let vector = get_generic_type vector
    let stack = get_generic_type stack
    let abstract_sequential_list = get_generic_type abstract_sequential_list
    let queue = get_generic_type queue
    let deque = get_generic_type deque
    let linked_list = get_generic_type linked_list
    let set = get_generic_type set
    let abstract_set = get_generic_type abstract_set
    let hash_set = get_generic_type hash_set
    let tree_set = get_generic_type tree_set
  end
end

let _ =
  let open CollectionClasses.Types in
  let pp_list f l =
    let open Stdlib in
    let rec is_first index elem l =
      match (index, l) with
      | 0, _ :: _ -> true
      | n, x :: xs when x = elem -> false
      | n, x :: xs -> is_first (n - 1) elem xs
      | _ -> failwith "Out of bounds"
    in

    List.iteri
      (fun i a ->
        Format.printf "%s %a\n" (if is_first i a l then "  " else "--") f a)
      l;

    let counts =
      let module M = Map.Make (struct
        type t = int logic JGS.Jtype.logic

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
    @@ run q query (fun q -> q#reify (JGS.Jtype.reify OCanren.reify))
  in
  let _ =
    run_jtype ~n:10 ~msg:"? <-< Iterable<Object>" (fun q ->
        q <-< jtype_inj (iterable obj))
  in
  let _ =
    run_jtype ~n:8 ~msg:"? <-< AbstractList<Object>" (fun q ->
        q <-< jtype_inj (abstract_list obj))
  in
  let _ =
    run_jtype ~n:(-1) ~msg:"RoleList <-< ?" (fun q ->
        fresh () (q =/= JGS.Jtype.null ()) (jtype_inj role_list <=< q))
  in
  (* Problems with RoleList and AttributeList *)
  let _ =
    run_jtype ~n:3 ~msg:"? <-< RandomAccess & ? <-< Iterable<Object>" (fun q ->
        fresh ()
          (* (q <-< jtype_inj random_access) *)
          (q <-< jtype_inj (iterable obj))
        (* *))
  in
  (* Problems with RoleList and AttributeList *)
  let _ =
    run_jtype ~n:2 ~msg:"? <-< RandomAccess & ? <-< AbstractCollection<Object>"
      (fun q ->
        fresh ()
          (q =/= JGS.Jtype.null ())
          (q <-< jtype_inj random_access)
          (q <-< jtype_inj (abstract_collection obj))
        (* *))
  in
  let _ =
    run_jtype ~n:(-1) ~msg:"LinkedList<Object> <-< ? & TreeSet<Object> <-< ?"
      (fun q ->
        fresh ()
          (q =/= JGS.Jtype.null ())
          (jtype_inj (linked_list obj) <=< q)
          (jtype_inj (tree_set obj) <=< q)
        (* *))
  in
  let _ =
    run_jtype ~n:5 ~msg:"? <-< Collection<String>" (fun q ->
        fresh () (q =/= JGS.Jtype.null ()) (q <-< jtype_inj (collection string))
        (* *))
  in
  ()
