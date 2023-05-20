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

let run_jtype pp ?(n = test_args.answers_count) query =
  let pp_list f l =
    Printf.sprintf "\n[\n  %s\n]%!"
    @@ String.concat ";\n  " @@ Stdlib.List.map f l
  in
  pp_list pp @@ OCanren.Stream.take ~n
  @@ OCanren.(run q) query (fun q -> q#reify JGS.HO.jtype_reify)

let class_or_interface typ =
  let open OCanren in
  let open JGS_Helpers in
  conde
    [ fresh (a b) (typ === class_ a b); fresh (a b) (typ === interface a b) ]

let () =
  let open JGS_Helpers in
  let j = Yojson.Safe.from_file test_args.json_name in

  let (module CT : MutableTypeTable.SAMPLE_CLASSTABLE), goal, name_of_id =
    match CT_of_json.make_query j with
    | x -> x
    | exception Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (exn, j) ->
        Format.eprintf "%s\n%!" (Printexc.to_string exn);
        Format.eprintf "%a\n%!" (Yojson.Safe.pretty_print ~std:true) j;
        exit 1
  in

  let module V = JGS.FO.Verifier (CT) in
  let rec ( <-< ) ta tb = failwith "<-<"
  (* ta -<- tb  *)
  (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

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
  let pp x =
    let nat_logic_to_int =
      let open OCanren in
      let rec helper acc = function
        | Value Std.Nat.O -> acc
        | Value (S p) -> helper (acc + 1) p
        | Var _ -> raise OCanren.Not_a_value
      in
      helper 0
    in
    let lookup id =
      let id = nat_logic_to_int id in
      (* Format.printf "lookup %d\n%!" id; *)
      match id with
      | id -> name_of_id id
      | exception OCanren.Not_a_value -> assert false
    in
    Format.asprintf "%a" (JGS_Helpers.pp_jtyp_logic lookup) x
  in

  let () =
    if test_args.run_default then
      Printf.printf "1.1 (?) < Object : %s\n"
      @@ run_jtype pp ~n:test_args.answers_count (fun typ ->
             let open OCanren in
             fresh () (class_or_interface typ)
               (( -<- ) typ (jtype_inj CT.object_t) !!true))
  in

  print_endline
  @@ run_jtype pp (fun typ ->
         let open OCanren in
         fresh ()
           (typ =/= intersect __)
           success success (goal ( -<- ) Fun.id typ))
