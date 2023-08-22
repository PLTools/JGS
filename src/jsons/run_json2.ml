let () = Printexc.record_backtrace true

type test_args = {
  mutable ct_file : string;
  mutable query_file : string;
  mutable run_default : bool;
  mutable answers_count : int;
  mutable fifo : string option;
  mutable query_hack : bool;
}

let test_args =
  {
    ct_file = "";
    query_file = "";
    run_default = false;
    answers_count = 1;
    fifo = None;
    query_hack = false;
  }

let () =
  OCanren.Runconf.occurs_check_off ();
  Arg.parse
    [
      ( "-default",
        Arg.Unit (fun () -> test_args.run_default <- true),
        " Run a default table-agnostic query (tests only)" );
      ("-v", Arg.Unit CT_of_json.set_verbose, " More verbose output");
      ( "-silent",
        Arg.Unit (fun () -> CT_of_json.verbose_errors := false),
        " Silent JSON errors" );
      ( "-hack",
        Arg.Unit (fun () -> test_args.query_hack <- true),
        " Use old style of query construction" );
      ( "-trace-cc",
        Arg.Unit (fun () -> JGS_stats.set_trace_cc true),
        " trace capture conversion" );
      ( "-trace-arrow",
        Arg.Unit (fun () -> JGS_stats.set_trace_arrow true),
        " trace -<- relation" );
      ( "-trace-closure",
        Arg.Unit (fun () -> JGS_stats.set_trace_closure_subtyping true),
        " trace closure subtyping" );
      ( "-trace-get-superclass-by-id",
        Arg.Unit (fun () -> JGS_stats.set_trace_get_superclass_by_id true),
        " trace superclass searching" );
      ( "-n",
        Arg.Int (fun n -> test_args.answers_count <- n),
        " Number of answers requested (default 1)" );
      ("-ct", Arg.String (fun s -> test_args.ct_file <- s), " class table file");
      ( "-need-simplified",
        Arg.Unit (fun () -> JGS.need_simpified := true),
        " Without capture conversion" );
      ( "-perffifo",
        Arg.String (fun s -> test_args.fifo <- Some s),
        " <file> Specify pipe file to start performace metrics only after JSON \
         parsing" );
      ( "-no-table-spec",
        Arg.Unit
          (fun () ->
            Mutable_type_table.need_table_dynamic_specialisation := false),
        " Switch off table dynamic specialisations" );
      ( "-no-dynamic-closure",
        Arg.Unit (fun () -> Closure.need_dynamic_closure := false),
        " Switch to static closure" );
      ( "-upper-bound-first",
        Arg.Unit (fun () -> CT_of_json.lower_bounds_first := false),
        " Solve upper bounds first" );
    ]
    (fun file -> test_args.query_file <- file)
    ""

let is_timer_enabled =
  match Unix.getenv "NOBENCH" with _ -> false | exception Not_found -> true

let pp_float_time fmt time =
  if time < 1000. then Format.fprintf fmt "%5.3fms" time
  else Format.fprintf fmt "%5.3fs" (Float.div time 1000.)

module Jtype_set = Set.Make (struct
  type t = JGS.HO.jtype_logic

  module Int_map = Map.Make (Stdlib.Int)

  type state = int * int Int_map.t

  let compare a b =
    (* TODO: Rewrite with state-monad *)
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
    Stdlib.compare
      (snd @@ replace_jtype (0, Int_map.empty) a)
      (snd @@ replace_jtype (0, Int_map.empty) b)
end)

let run_jtype pp ?(n = test_args.answers_count) query =
  let total_time = ref 0. in
  let max1_time = ref 0. in
  let max2_time = ref 0. in
  let last_time = ref 0. in
  let answers_set = ref Jtype_set.empty in
  let duplicated = ref Jtype_set.empty in
  let time =
    if is_timer_enabled then (fun f ->
      if JGS_stats.config.enable_counters then JGS_stats.clear_statistics ();
      let start = Mtime_clock.elapsed () in
      let ans = f () in
      let fin = Mtime_clock.elapsed () in
      if JGS_stats.config.enable_counters then JGS_stats.report_counters ();
      let span = Mtime.Span.abs_diff start fin in
      let span_ms = Mtime.Span.to_ms span in

      (match ans with
      | Some _ ->
          total_time := Float.add !total_time span_ms;
          if span_ms > !max1_time then (
            max2_time := !max1_time;
            max1_time := span_ms)
          else if span_ms > !max2_time then max2_time := span_ms
      | None -> last_time := span_ms);

      let msg =
        if Mtime.Span.to_ms span > 1000. then
          Printf.sprintf "%5.2fs" (Mtime.Span.to_s span)
        else Printf.sprintf "%5.2fms" span_ms
      in
      (Some msg, ans))
    else fun f -> (None, f ())
  in
  (* TODO: OCanren.Stream needs iteri_k for stuff like this *)
  let rec loop i stream =
    if i > n then i - 1
    else
      match time (fun () -> OCanren.Stream.msplit stream) with
      | None, None -> i - 1
      | Some msg, None ->
          Format.printf "%s  -  no answer\n%!" msg;
          i - 1
      | Some msg, Some (h, tl) ->
          Format.printf "%s % 3d)  %a\n%!" msg i pp h;
          if Jtype_set.mem h !answers_set then
            duplicated := Jtype_set.add h !duplicated;
          answers_set := Jtype_set.add h !answers_set;
          loop (1 + i) tl
      | None, Some (h, tl) ->
          Format.printf "% 3d)  %a\n%!" i pp h;
          answers_set := Jtype_set.add h !answers_set;
          loop (1 + i) tl
  in
  let total_amount =
    loop 1 @@ OCanren.(run q) query (fun q -> q#reify JGS.HO.jtype_reify)
  in
  (* Format.printf "\n";
     Format.printf "Duplicated answers:\n";
     Jtype_set.iter (fun a -> Format.printf "  %a\n" pp a) !duplicated; *)
  Format.printf "\n";
  Format.printf "Total amount: %d\n" total_amount;
  Format.printf "Total uniq amount: %d\n" @@ Jtype_set.cardinal !answers_set;
  Format.printf "Total time: %a\n" pp_float_time
  @@ Float.add !total_time !last_time;
  Format.printf "Total time without prove: %a\n" pp_float_time !total_time;
  Format.printf "Avg time: %a\n" pp_float_time
  @@ Float.div !total_time @@ Float.of_int total_amount;
  Format.printf "Max time: %a\n" pp_float_time !max1_time;
  Format.printf "Next after max time: %a\n" pp_float_time !max2_time;
  Format.printf "Time to prove: %a\n" pp_float_time !last_time

let class_or_interface typ =
  let open OCanren in
  let open JGS_Helpers in
  conde
    [ fresh (a b) (typ === class_ a b); fresh (a b) (typ === interface a b) ]

let () =
  let open JGS_Helpers in
  let j =
    let ct = Yojson.Safe.from_file test_args.ct_file in
    let query = Yojson.Safe.from_file test_args.query_file in

    let upper, lower =
      match query with
      | `Assoc vals -> (
          ( (try List.assoc "upperBounds" vals
             with Not_found -> (
               try `List [ List.assoc "upperBound" vals ]
               with Not_found -> `List [])),
            try List.assoc "lowerBounds" vals
            with Not_found -> (
              try `List [ List.assoc "lowerBound" vals ]
              with Not_found -> `List []) ))
      | _ -> assert false
    in

    match ct with
    | `Assoc values ->
        let values =
          List.filter (function "table", _ -> true | _ -> false) values
        in
        `Assoc
          (("upper_bounds", upper) :: ("lower_bounds", lower)
          :: ("neg_upper_bounds", upper)
          :: ("neg_lower_bounds", lower)
          :: values)
    | _ -> failwith " Bad class table"
  in
  Out_channel.with_open_text "/tmp/combined.json" (fun ch ->
      Yojson.Safe.pretty_to_channel ch j);

  let (module CT : Mutable_type_table.SAMPLE_CLASSTABLE), goal, name_of_id =
    match CT_of_json.make_query ~hack_goal:test_args.query_hack j with
    | x -> x
    | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, j) ->
        Format.eprintf "%s\n%!" (Printexc.to_string exn);
        Format.eprintf "%a\n%!" (Yojson.Safe.pretty_print ~std:true) j;
        exit 1
  in
  let module CT = struct
    include CT

    let pp_targ =
      JGS_Helpers.pp_targ_logic (function
        | Var (idx, _) -> Printf.sprintf "_.%d" idx
        | Value idx -> name_of_id idx)

    let pp_jtyp =
      JGS_Helpers.pp_jtyp_logic (function
        | Var (idx, _) -> Printf.sprintf "_.%d" idx
        | Value idx -> name_of_id idx)
  end in
  let module V = JGS.FO.Verifier (CT) in
  let open OCanren in
  let open JGS in
  let open Closure in
  let { closure; direct_subtyping = _; _ } =
    Closure.make_closure (module CT) V.( -<- )
  in

  (* let module MM = struct
       open OCanren

       type hack =
         ( JGS.HO.jtype_injected JGS.HO.targ_injected List.HO.list_injected,
           Std.Nat.injected,
           JGS.HO.jtype_injected,
           JGS.HO.jtype_injected Option.HO.option_injected,
           JGS.HO.jtype_injected List.HO.list_injected )
         JGS.HO.jtype_fuly
         ilogic

       let (_ :
             (hack -> hack -> bool ilogic -> Peano.HO.goal) ->
             hack ->
             hack ->
             bool ilogic ->
             Peano.HO.goal) =
         V.( -<- )

       let (_ : hack -> hack -> bool ilogic -> Peano.HO.goal) = ( -<- )
     end in *)
  Format.printf "Running generated query\n%!";
  let pp ppf x =
    let nat_logic_to_int = function
      | Value n -> n
      | _ -> raise OCanren.Not_a_value
      (* let open OCanren in
         let rec helper acc = function
           | Value Std.Nat.O -> acc
           | Value (S p) -> helper (acc + 1) p
           | Var _ -> raise OCanren.Not_a_value
         in
         helper 0 *)
    in
    let lookup id =
      match nat_logic_to_int id with
      | id -> name_of_id id
      | exception OCanren.Not_a_value ->
          Format.eprintf "ERROR: free variables inside class id\n%!";
          "_.??"
    in
    Format.fprintf ppf "%a" (JGS_Helpers.pp_jtyp_logic lookup) x
  in

  let () =
    if test_args.run_default then
      let () = Printf.printf "1.1 (?) < Object :\n" in
      run_jtype pp ~n:test_args.answers_count (fun typ ->
          let open OCanren in
          fresh () (class_or_interface typ)
            (closure ~closure_type:Subtyping typ (jtype_inj CT.object_t)))
  in

  let __ () =
    Format.printf "%a\n%!" JGS_Helpers.JGS_PP.decl (CT.decl_by_id 4);
    Format.printf "%a\n%!" JGS_Helpers.JGS_PP.decl (CT.decl_by_id 5);
    Format.printf "%a\n%!" JGS_Helpers.JGS_PP.decl (CT.decl_by_id 7);
    ()
  in
  let () =
    test_args.fifo
    |> Stdlib.Option.iter (fun s ->
           ignore @@ Sys.command ("echo 'enable\n' > " ^ s))
  in
  run_jtype pp (fun typ ->
      let open OCanren in
      fresh ()
        (typ =/= intersect __)
        (typ =/= !!HO.Null)
        (typ =/= var ~index:__ __ __ __)
        (*  *)
        (goal ~is_subtype:closure Fun.id typ))
