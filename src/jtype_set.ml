module Int_map = Map.Make (Stdlib.Int)

type state = int * int Int_map.t

(* TODO: Rewrite with state-monad *)
let replace_jtype : JGS.HO.jtype_logic -> JGS.HO.jtype_logic =
  let rec replace_jtype :
      state -> JGS.HO.jtype_logic -> state * JGS.HO.jtype_logic =
    let get_index : int -> state -> state * int =
     fun var ((cur_index, map) as state) ->
      match Int_map.find_opt var map with
      | Some index -> (state, index)
      | None -> ((cur_index + 1, Int_map.add var cur_index map), cur_index)
    in

    let rec update_var state f = function
      | OCanren.Value x ->
          let state, x = f state x in
          (state, OCanren.Value x)
      | Var (n, diseqs) ->
          let state, n = get_index n state in
          let state, rev_diseqs =
            List.fold_left
              (fun (state, acc) t ->
                let state, t = update_var state f t in
                (state, t :: acc))
              (state, []) diseqs
          in
          (state, OCanren.Var (n, List.rev rev_diseqs))
    in
    let rec replace_list f state =
      let open OCanren.Std.List in
      update_var state @@ fun state -> function
      | Cons (hd, tl) ->
          let state, hd = f state hd in
          let state, tl = replace_list f state tl in
          (state, Cons (hd, tl)) | Nil -> (state, Nil)
    in
    let rec replace_peano state =
      let open OCanren.Std.Nat in
      update_var state @@ fun state -> function
      | S x ->
          let state, x = replace_peano state x in
          (state, S x) | O -> (state, O)
    in
    let replace_option f state =
      update_var state @@ fun state -> function
      | Some x ->
          let state, x = f state x in
          (state, Some x) | None -> (state, None)
    in
    let replace_pair f g state =
      update_var state @@ fun state (a, b) ->
      let state, a = f state a in
      let state, b = g state b in
      (state, (a, b))
    in
    let replace_primitive state =
      update_var state @@ fun state x -> (state, x)
    in
    let open JGS.HO in
    let replace_jarg state =
      update_var state @@ fun state -> function
      | Type t ->
          let state, t = replace_jtype state t in
          (state, Type t)
      | Wildcard x ->
          let state, x =
            replace_option
              (replace_pair replace_primitive replace_jtype)
              state x
          in
          (state, Wildcard x)
    in
    fun state ->
      update_var state @@ fun state -> function Null -> (state, Null)
      | Array lt ->
          let state, lt = replace_jtype state lt in
          (state, Array lt)
      | Intersect lts ->
          let state, lts = replace_list replace_jtype state lts in
          (state, Intersect lts)
      | Var { id; index; upb; lwb } ->
          let state, id = replace_primitive state id in
          let state, index = replace_peano state index in
          let state, upb = replace_jtype state upb in
          let state, lwb = replace_option replace_jtype state lwb in
          (state, Var { id; index; upb; lwb })
      | Class (i, args) ->
          let state, i = replace_primitive state i in
          let state, args = replace_list replace_jarg state args in
          (state, Class (i, args))
      | Interface (i, args) ->
          let state, i = replace_primitive state i in
          let state, args = replace_list replace_jarg state args in
          (state, Interface (i, args))
  in
  fun t -> snd @@ replace_jtype (0, Int_map.empty) t

include Set.Make (struct
  type t = JGS.HO.jtype_logic

  let compare a b = Stdlib.compare a b
end)

let add_alpha_converted t set = add (replace_jtype t) set
let mem_alpha_converted t set = mem (replace_jtype t) set
