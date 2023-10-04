let () = Printexc.record_backtrace true

type test_args = {
  mutable json_name : string;
  mutable run_default : bool;
  mutable answers_count : int;
}

let test_args = { json_name = ""; run_default = false; answers_count = 1 }

let () =
  Arg.parse
    [
      ( "-default",
        Arg.Unit (fun () -> test_args.run_default <- true),
        " Run a default table-agnostic query (tests only)" );
      ("-v", Arg.Unit CT_of_json.set_verbose, " More verbose output");
      ( "-n",
        Arg.Int (fun n -> test_args.answers_count <- n),
        " Numer of answers requested (default 1)" );
    ]
    (fun file -> test_args.json_name <- file)
    ""

let is_timer_enabled =
  match Unix.getenv "NOBENCH" with _ -> false | exception Not_found -> true

let run_jtype pp ?(n = test_args.answers_count) query =
  let time =
    if is_timer_enabled then fun f ->
      let start = Mtime_clock.elapsed () in
      let ans = f () in
      let fin = Mtime_clock.elapsed () in
      let span = Mtime.Span.abs_diff start fin in
      let msg =
        if Mtime.Span.to_ms span > 1000. then
          Printf.sprintf "%5.1fs" (Mtime.Span.to_s span)
        else Printf.sprintf "%5.1fms" (Mtime.Span.to_ms span)
      in
      (Some msg, ans)
    else fun f -> (None, f ())
  in
  (* TODO: OCanren.Stream needs iteri_k for stiff like this *)
  let rec loop i stream =
    if i > n then ()
    else
      match time (fun () -> OCanren.Stream.msplit stream) with
      | _, None -> ()
      | Some msg, Some (h, tl) ->
          Format.printf "%s % 3d)  %a\n%!" msg i pp h;
          loop (1 + i) tl
      | None, Some (h, tl) ->
          Format.printf "% 3d)  %a\n%!" i pp h;
          loop (1 + i) tl
  in
  loop 1
  @@ OCanren.(run q) query (fun q -> q#reify (JGS.Jtype.reify OCanren.reify))

let class_or_interface typ =
  let open OCanren in
  let open JGS_Helpers in
  conde
    [ fresh (a b) (typ === class_ a b); fresh (a b) (typ === interface a b) ]

let () =
  let open JGS_Helpers in
  let j = Yojson.Safe.from_file test_args.json_name in

  let (module CT : Mutable_type_table.SAMPLE_CLASSTABLE), goal, name_of_id =
    match CT_of_json.make_query j with
    | x -> x
    | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, j) ->
        Format.eprintf "%s\n%!" (Printexc.to_string exn);
        Format.eprintf "%a\n%!" (Yojson.Safe.pretty_print ~std:true) j;
        exit 1
  in
  let module V = JGS.Verifier (CT) in
  let open OCanren in
  let open JGS in
  let open Closure in
  let { closure = ( <-< ); direct_subtyping = ( -<- ); _ } =
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
            (( -<- ) typ (jtype_inj CT.Ground.object_t) ~closure_type:Subtyping))
  in
  (*
  let () =
    let wrap n =
      Format.printf "decl %d: %a\n" n JGS_Helpers.JGS_PP.decl (CT.decl_by_id n)
    in
    wrap 4;
    wrap 6;
    wrap 8;
    wrap 9
  in *)
  run_jtype pp (fun typ ->
      let open OCanren in
      fresh ()
        (typ =/= intersect __) (* (typ =/= !!HO.Null) *)
        (typ =/= var ~index:__ __ __ __)
        (*  *)
        (goal ~is_subtype:( <-< ) Fun.id typ))
