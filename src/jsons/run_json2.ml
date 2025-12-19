(* let () = Printexc.record_backtrace true *)

let () =
  Sys.set_signal Sys.sigterm
    (Sys.Signal_handle
       (fun _ ->
         print_endline "timeout";
         exit 1))

let () =
  (* Intel-specific *)
  let file = "/sys/devices/system/cpu/intel_pstate/no_turbo" in
  if Sys.file_exists file then
    let cnt = In_channel.with_open_text file In_channel.input_all in

    match cnt with
    | "0" | "0\n" -> Format.eprintf "Turbo Boost is enabled\n%!"
    | "1" | "1\n" -> ()
    | _ -> Printf.eprintf "What to do with '%s'\n%!" cnt
  else ()

type test_args = {
  mutable ct_file : string;
  mutable query_file : string;
  mutable run_default : bool;
  mutable warmup_iterations : int;
  mutable answers_count : int;
  mutable fifo : string option;
  mutable query_hack : bool;
  mutable latex_file : string;
  mutable latex_prefix : string;
  mutable lower_before : bool;
}

let test_args =
  {
    ct_file = "";
    query_file = "";
    run_default = false;
    warmup_iterations = 100;
    answers_count = 1;
    fifo = None;
    query_hack = false;
    latex_file = "";
    latex_prefix = "TEST";
    lower_before = false;
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
      ( "-remove-dups-structural",
        Arg.Unit
          (fun () -> CT_of_json.need_remove_dups := CT_of_json.Structural),
        " Remove answers duplacates using structural constraint" );
      ( "-remove-dups-debug-var",
        Arg.Unit (fun () -> CT_of_json.need_remove_dups := CT_of_json.Debug_var),
        " Remove answers duplacates using structural debug_var" );
      ( "-la-file",
        Arg.String (fun s -> test_args.latex_file <- s),
        " Set latex output file for benchmarks" );
      ( "-la-testname",
        Arg.String (fun s -> test_args.latex_prefix <- s),
        " <STRING> Set LaTeX test name" );
      ( "-warmup-n",
        Arg.Int (fun s -> test_args.warmup_iterations <- s),
        " <INT> " );
      ( "-upbefore",
        Arg.Unit (fun () -> test_args.lower_before <- false),
        " Put upper constraint in the beginning of the search (DEFAULT)" );
      ( "-lobefore",
        Arg.Unit (fun () -> test_args.lower_before <- true),
        " Put lower constraint in the beginning of the search " );
      ( "-vct",
        Arg.Unit (fun () -> CT_of_json.verbose_errors := true),
        " Verbose building of class table " );
    ]
    (fun file -> test_args.query_file <- file)
    ""

let is_timer_enabled =
  match Unix.getenv "NOBENCH" with _ -> false | exception Not_found -> true

let pp_float_time fmt time =
  if time < 1000. then Format.fprintf fmt "%5.2fms" time
  else Format.fprintf fmt "%5.2fs" (Float.div time 1000.)

type 'f bench_results = {
  total_amount : int;  (** Total count of answers *)
  uniq_count : int;  (** Count of unique answers *)
  first_time : 'f;  (** Time of the first answer *)
  total_time : 'f;
      (** Synthesis time: from the beginning to the time of last answer *)
  max_time : 'f;  (** Longest time to get next answer *)
  last_time : 'f;  (** Time to prove that no more answers left *)
}

let empty_bench_result () =
  {
    total_amount = 0;
    uniq_count = 0;
    first_time = 0.0;
    total_time = 0.0;
    max_time = 0.0;
    last_time = 0.0;
  }

let join_bench_results br1 br2 =
  let join_count c1 c2 =
    if c1 = 0 && c2 = 0 then failwith "Bad argument";
    if c1 < 0 || c2 < 0 then failwith "Bad argument";
    max c1 c2
  in
  {
    total_amount = join_count br1.total_amount br2.total_amount;
    uniq_count = join_count br1.uniq_count br2.uniq_count;
    first_time = br1.first_time +. br2.first_time;
    total_time = br1.total_time +. br2.total_time;
    max_time = br1.max_time +. br2.max_time;
    last_time = br1.last_time +. br2.last_time;
  }

let avg_bench_results = function
  | [] -> assert false
  | h :: tl ->
      let c = float_of_int (1 + List.length tl) in
      let collect field =
        let f (min, sum, max) r =
          let v = field r in
          (Float.min min v, sum +. v, Float.max max v)
        in
        let min, sum, max =
          List.fold_left f (Float.max_float, 0.0, Float.min_float) tl
        in
        (min, sum /. c, max)
      in
      assert (
        List.for_all (fun { uniq_count; _ } -> uniq_count = h.uniq_count) tl);
      assert (
        List.for_all
          (fun { total_amount; _ } -> total_amount = h.total_amount)
          tl);
      {
        total_amount = h.total_amount;
        uniq_count = h.uniq_count;
        first_time = collect (fun br -> br.first_time);
        total_time = collect (fun br -> br.total_time);
        max_time = collect (fun br -> br.max_time);
        last_time = collect (fun br -> br.last_time);
      }

let snd3 (_, x, _) = x
(* let thr3 (_, _, x) = x *)

let pp_bench_results ppf br =
  Format.fprintf ppf "\n";
  Format.fprintf ppf "Total amount: %d\n" br.total_amount;
  Format.fprintf ppf "Total uniq amount: %d\n" br.uniq_count;
  Format.fprintf ppf "First time: %a\n" pp_float_time (snd3 br.first_time);
  Format.fprintf ppf "Avg time: %a\n" pp_float_time
  @@ Float.div (snd3 br.total_time)
  @@ Float.of_int br.total_amount;
  Format.fprintf ppf "Max time: %a\n" pp_float_time (snd3 br.max_time);
  Format.fprintf ppf "Time to prove: %a\n" pp_float_time (snd3 br.last_time);
  Format.fprintf ppf "Total time: %a\n" pp_float_time
  @@ Float.add (snd3 br.total_time) (snd3 br.last_time);
  Format.fprintf ppf "Total time without prove: %a\n" pp_float_time
    (snd3 br.total_time);
  Format.pp_print_flush ppf ();
  (* TODO: calculate error% *)
  (* TODO: <1\\ms *)
  ()

let pp_bench_latex ~name ~desc ppf br =
  let open Format in
  let pp_float_time fmt time =
    if time < 1. then fprintf fmt ">1~\\ms{}"
    else if time < 1000. then fprintf fmt "%5.0f\\ms{}" time
    else fprintf fmt "%5.0f\\s{}" (Float.div time 1000.)
  in

  let pp_error ppf (min, avg, max) =
    Format.fprintf ppf "-%2.0f\\%% +%2.0f\\%%"
      ((avg -. min) /. avg *. 100.)
      ((max -. avg) /. avg *. 100.)
  in

  fprintf ppf "%% Benchmark '%s': %s\n%!" name desc;
  fprintf ppf "\\def\\b%stotal{%d} %% Total amount\n" name br.total_amount;
  fprintf ppf "\\def\\b%suniqC{%d} %% Total uniq amount\n" name br.uniq_count;
  fprintf ppf "\\def\\b%sfirstTime{%a} %% First time\n" name pp_float_time
    (snd3 br.first_time);
  fprintf ppf "\\def\\b%sfirstTimeError{%a}\n" name pp_error br.first_time;
  fprintf ppf "\\def\\b%sAvgTime{%a} %% Avg time\n" name pp_float_time
  @@ Float.div (snd3 br.total_time)
  @@ Float.of_int br.total_amount;
  fprintf ppf "\\def\\b%sAvgTimeError{%a}\n" name pp_error br.total_time;
  fprintf ppf "\\def\\b%sMaxTime{%a} %% Max time\n" name pp_float_time
    (snd3 br.max_time);
  fprintf ppf "\\def\\b%sMaxTimeError{%a}\n" name pp_error br.max_time;
  fprintf ppf "\\def\\b%sLastTime{%a} %% Time to prove:\n" name pp_float_time
    (snd3 br.last_time);
  fprintf ppf "\\def\\b%sTotalTime{%a} %% Total time\n" name pp_float_time
    (Float.add (snd3 br.total_time) (snd3 br.last_time));
  fprintf ppf "\\def\\b%sTotalWioutProve{%a} %% Total time without prove\n" name
    pp_float_time (snd3 br.total_time);
  fprintf ppf "\\def\\b%sTotalWioutProveError{%a}\n" name pp_error br.total_time;
  fprintf ppf "%!"

let run_jtype ?(verbose = true) ?(n = test_args.answers_count) pp
    (query : JGS.HO.jtype_injected -> OCanren.goal) =
  let _ : JGS.HO.jtype_injected -> OCanren.goal = query in
  let is_first = ref true in
  let total_time = ref 0. in
  let max_time = ref 0. in
  let first_time = ref 0. in
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
      let span_ms = Mtime.Span.to_float_ns span /. 1e6 in

      (match ans with
      | Some _ ->
          total_time := Float.add !total_time span_ms;
          if !is_first then first_time := span_ms
          else if span_ms > !max_time then max_time := span_ms;
          is_first := false
      | None -> last_time := span_ms);

      let msg =
        if Mtime.Span.to_uint64_ns span > 1_000_000_000L then
          Printf.sprintf "%5.1fs" (Mtime.Span.to_float_ns span /. 1e9)
        else Printf.sprintf "%5.1fms" (Mtime.Span.to_float_ns span /. 1e6)
      in
      (Some msg, ans))
    else fun f -> (None, f ())
  in
  (* TODO: OCanren.Stream needs iteri_k for stuff like this *)
  let iteri_k n stream fin f =
    let rec helper i n stream ~f =
      if i > n then fin (i - 1)
      else
        f i
          (fun () -> OCanren.Stream.msplit stream)
          (fun tl -> helper (1 + i) n ~f tl)
    in
    helper 0 n ~f stream
  in

  let total_amount =
    iteri_k n
      (OCanren.(run q) query (fun q -> q#reify JGS.HO.jtype_reify))
      Fun.id
      (fun i calc k ->
        match time calc with
        | None, None -> i - 1
        | Some msg, None ->
            if verbose then Format.printf "%s  -  no answer\n%!" msg;
            i - 1
        | Some msg, Some (h, tl) ->
            if verbose then Format.printf "%s % 3d)  %a\n%!" msg i pp h;
            if Jtype_set.mem_alpha_converted h !answers_set then
              duplicated := Jtype_set.add_alpha_converted h !duplicated;
            answers_set := Jtype_set.add_alpha_converted h !answers_set;
            Jtype_set.alpha_converted_answer_set :=
              Jtype_set.add_alpha_converted h
                !Jtype_set.alpha_converted_answer_set;
            k tl
        | None, Some (h, tl) ->
            if verbose then Format.printf "% 3d)  %a\n%!" i pp h;
            answers_set := Jtype_set.add_alpha_converted h !answers_set;
            k tl)
  in
  (* Format.printf "\n";
     Format.printf "Duplicated answers:\n";
     Jtype_set.iter (fun a -> Format.printf "  %a\n" pp a) !duplicated; *)
  {
    total_amount;
    uniq_count = Jtype_set.cardinal !answers_set;
    first_time = !first_time;
    total_time = !total_time;
    max_time = !max_time;
    last_time = !last_time;
  }

let class_or_interface typ =
  let open OCanren in
  let open JGS_Helpers in
  conde
    [ fresh (a b) (typ === class_ a b); fresh (a b) (typ === interface a b) ]

let perform_KS_test timings =
  let data =
    timings
    |> Stdlib.List.map (fun { first_time = t; _ } -> t)
    (* |> Stdlib.List.sort Float.compare *)
    |> Stdlib.List.map string_of_float
  in
  let fmt : _ format =
    {|
import numpy as np
from scipy import stats
import matplotlib.pyplot as plt

# Step 1: Create a float array
data = [ %s ]

# Step 2: Perform the Shapiro-Wilk Test
statistic, p_value = stats.shapiro(data)

# Step 3: Interpret the results
alpha = 0.05
print(f'Statistic: {statistic}, p-value: {p_value}')

if p_value > alpha:
    print("The data follows a normal distribution (fail to reject H0)")
else:
    print("The data does not follow a normal distribution (reject H0)")

plt.figure(figsize=(10, 6))
plt.hist(data, bins=30, color='skyblue', edgecolor='black')
plt.xlabel('Value')
plt.ylabel('Frequency')
plt.grid(axis='y', alpha=0.75)  # Grid lines for better readability
plt.savefig('histogram.png', format='png', dpi=300)  # Save with high resolution
plt.close()

# Q-Q plot
stats.probplot(data, dist="norm", plot=plt)
plt.title("Normal Q-Q Plot")
plt.xlabel("Theoretical Quantiles")
plt.ylabel("Sample Quantiles")
plt.grid()
plt.savefig('qq.png', format='png', dpi=300)
plt.close()
|}
  in
  let filename = "1.py" in
  Out_channel.with_open_text filename (fun ch ->
      Printf.fprintf ch fmt (String.concat ", " data));
  let _ : int = Sys.command (Printf.sprintf "python3 %s" filename) in
  ()

let () =
  let open JGS_Helpers in
  let j =
    let ct =
      if Sys.file_exists test_args.ct_file then
        Yojson.Safe.from_file test_args.ct_file
      else (
        Printf.eprintf "File '%s' doesn't exist\n%!" test_args.ct_file;
        exit 1)
    in
    let query =
      if Sys.file_exists test_args.query_file then
        Yojson.Safe.from_file test_args.query_file
      else (
        Printf.eprintf "File '%s' doesn't exist\n%!" test_args.query_file;
        exit 1)
    in

    let upper, lower =
      match query with
      | `Assoc vals ->
          ( (try List.assoc "upperBounds" vals
             with Not_found -> (
               try `List [ List.assoc "upperBound" vals ]
               with Not_found -> `List [])),
            try List.assoc "lowerBounds" vals
            with Not_found -> (
              try `List [ List.assoc "lowerBound" vals ]
              with Not_found -> `List []) )
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

  let ( (module CT : Mutable_type_table.SAMPLE_CLASSTABLE),
        goal,
        name_of_id,
        goal_repr ) =
    let join ~upper ~lower =
      if test_args.lower_before then lower @ upper else upper @ lower
    in

    match CT_of_json.make_query ~hack_goal:test_args.query_hack join j with
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

  let __ () =
    if test_args.run_default then
      let () = Printf.printf "1.1 (?) < Object :\n" in
      pp_bench_latex ~name:"test" ~desc:"desc" Format.std_formatter
      @@ avg_bench_results
           [
             run_jtype pp ~n:test_args.answers_count (fun typ ->
                 let open OCanren in
                 fresh () (class_or_interface typ)
                   (closure ~closure_type:Subtyping typ (jtype_inj CT.object_t)));
           ]
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
  let run_synthesis ~verbose () =
    run_jtype ~verbose pp (fun typ ->
        let open OCanren in
        fresh ()
          (typ =/= intersect __)
          (typ =/= !!HO.Null)
          (typ =/= var ~index:__ __ __ __)
          (* *)
          (goal ~is_subtype:closure Fun.id typ))
  in

  let () =
    match Sys.getenv "JGS_BENCH" with
    | exception Not_found -> ()
    | _ ->
        print_endline "Warmup";
        for _ = 1 to test_args.warmup_iterations do
          Gc.compact ();
          let maj_before, min_before =
            let st = Gc.stat () in
            (st.Gc.major_collections, st.Gc.minor_collections)
          in
          let _ = run_synthesis ~verbose:false () in
          let maj_after, min_after =
            let st = Gc.stat () in
            (st.Gc.major_collections, st.Gc.minor_collections)
          in
          Format.printf "Before: %d, %d\n%!" maj_before min_before;
          Format.printf "After:  %d, %d\n%!" maj_after min_after
        done
  in

  Format.printf "Running generated query\n%!";

  match Sys.getenv "JGS_BENCH" with
  | exception Not_found when Sys.getenv_opt "NOBENCH" = None ->
      pp_bench_results Format.std_formatter
      @@ avg_bench_results [ run_synthesis ~verbose:true () ]
  | exception Not_found ->
      let _ = run_synthesis ~verbose:true () in
      ()
  | repeat_str ->
      let repeat =
        match int_of_string_opt repeat_str with
        | None ->
            Printf.eprintf "JGS_BENCH doesn't contain valid iteration count.\n";
            exit 1
        | Some repeat -> repeat
      in
      Printf.printf "Run benchmarks for %d iterations!!!\n%!" repeat;
      let timings =
        Stdlib.List.init repeat (fun _ -> run_synthesis ~verbose:false ())
      in

      let () = perform_KS_test timings in

      let avg = avg_bench_results timings in

      Format.printf "\n\n==== Final bench result:%a\n%!" pp_bench_results avg;
      if test_args.latex_file <> "" then
        Out_channel.with_open_text test_args.latex_file (fun ch ->
            let ppf = Format.formatter_of_out_channel ch in
            Format.fprintf ppf "@[%a@]\n%!"
              (pp_bench_latex ~desc:goal_repr ~name:test_args.latex_prefix)
              avg;
            flush ch)
      else
        Format.printf "@[%a@]\n%!"
          (pp_bench_latex ~desc:goal_repr ~name:"hack")
          avg;
      exit 0
