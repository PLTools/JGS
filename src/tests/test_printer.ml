open OCanren
open JGS

let run_jtype ?(n = -1) ?(need_sorting = true) ?(need_alpha_conversion = true)
    ~msg (query : _ Jtype.injected -> goal) =
  let pp_list f l =
    Printf.sprintf "\n[\n  %s\n]%!"
    @@ String.concat ";\n  "
    @@ (if need_sorting then Stdlib.List.sort String.compare else Fun.id)
    @@ Stdlib.List.map f l
  in

  Printf.printf "%s, %s answers:%s\n" msg
    (if n < 0 then "all" else string_of_int n)
  @@ pp_list (fun jt ->
         JGS_Helpers.pp_ljtype
         @@ if need_alpha_conversion then Jtype_set.replace_jtype jt else jt)
  @@ Stream.take ~n
  @@ run q query (fun q -> q#reify (Jtype.reify OCanren.reify))
