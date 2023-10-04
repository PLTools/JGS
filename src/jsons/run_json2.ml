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
      ( "-n",
        Arg.Int (fun n -> test_args.answers_count <- n),
        " Number of answers requested (default 1)" );
      ("-ct", Arg.String (fun s -> test_args.ct_file <- s), " class table file");
      ( "-perffifo",
        Arg.String (fun s -> test_args.fifo <- Some s),
        " <file> Specify pipe file to start performace metrics only after JSON \
         parsing" );
      ( "-upper-bound-first",
        Arg.Unit (fun () -> CT_of_json.lower_bounds_first := false),
        " Solve upper bounds first" );
    ]
    (fun file -> test_args.query_file <- file)
    ""

let is_timer_enabled =
  match Unix.getenv "NOBENCH" with _ -> false | exception Not_found -> true

let pp_float_time fmt time =
  if time < 1000. then Format.fprintf fmt "%5.2fms" time
  else Format.fprintf fmt "%5.2fs" (Float.div time 1000.)

let run_jtype pp ?(n = test_args.answers_count) query =
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
      let span_ms = Mtime.Span.to_ms span in

      (match ans with
      | Some _ ->
          total_time := Float.add !total_time span_ms;
          if !is_first then first_time := span_ms
          else if span_ms > !max_time then max_time := span_ms;
          is_first := false
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
          if Jtype_set.mem_alpha_converted h !answers_set then
            duplicated := Jtype_set.add_alpha_converted h !duplicated;
          answers_set := Jtype_set.add_alpha_converted h !answers_set;
          Jtype_set.alpha_converted_answer_set :=
            Jtype_set.add_alpha_converted h
              !Jtype_set.alpha_converted_answer_set;
          loop (1 + i) tl
      | None, Some (h, tl) ->
          Format.printf "% 3d)  %a\n%!" i pp h;
          answers_set := Jtype_set.add_alpha_converted h !answers_set;
          loop (1 + i) tl
  in
  let total_amount =
    loop 1
    @@ OCanren.(run q) query (fun q -> q#reify (JGS.Jtype.reify OCanren.reify))
  in
  (* Format.printf "\n";
     Format.printf "Duplicated answers:\n";
     Jtype_set.iter (fun a -> Format.printf "  %a\n" pp a) !duplicated; *)
  Format.printf "\n";
  Format.printf "Total amount: %d\n" total_amount;
  Format.printf "Total uniq amount: %d\n" @@ Jtype_set.cardinal !answers_set;
  Format.printf "First time: %a\n" pp_float_time !first_time;
  Format.printf "Avg time: %a\n" pp_float_time
  @@ Float.div !total_time @@ Float.of_int total_amount;
  Format.printf "Max time: %a\n" pp_float_time !max_time;
  Format.printf "Time to prove: %a\n" pp_float_time !last_time;
  Format.printf "Total time: %a\n" pp_float_time
  @@ Float.add !total_time !last_time;
  Format.printf "Total time without prove: %a\n" pp_float_time !total_time

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
  let open OCanren in
  let module V = JGS.Verifier (CT) in
  let module C = Closure.Make (CT) in
  let closure = C.closure V.( -<- ) in

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
            (closure typ (jtype_inj CT.Ground.object_t)))
  in

  let __ () =
    Format.printf "%a\n%!" JGS_Helpers.JGS_PP.decl (CT.Ground.decl_by_id 4);
    Format.printf "%a\n%!" JGS_Helpers.JGS_PP.decl (CT.Ground.decl_by_id 5);
    Format.printf "%a\n%!" JGS_Helpers.JGS_PP.decl (CT.Ground.decl_by_id 7);
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
        (typ =/= JGS.Jtype.null ())
        (typ =/= var ~index:__ __ __ __)
        (*  *)
        (goal ~is_subtype:closure Fun.id typ))
