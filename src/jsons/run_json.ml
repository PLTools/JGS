type test_args = {
  mutable json_name : string;
  mutable run_default : bool;
  mutable answers_count : int;
}

let test_args = { json_name = ""; run_default = false; answers_count = 10 }

let () =
  Arg.parse
    [
      ("-default", Arg.Unit (fun () -> test_args.run_default <- true), "");
      ("-n", Arg.Int (fun n -> test_args.answers_count <- n), "");
    ]
    (fun file -> test_args.json_name <- file)
    ""

let run_jtype ?(n = test_args.answers_count) query =
  let pp_list f l =
    Printf.sprintf "\n[\n  %s\n]%!"
    @@ String.concat ";\n  " @@ Stdlib.List.map f l
  in
  pp_list JGS_Helpers.pp_ljtype
  @@ OCanren.Stream.take ~n
  @@ OCanren.(run q) query (fun q -> q#reify JGS.HO.jtype_reify)

let () =
  let open JGS_Helpers in
  let j = Yojson.Safe.from_file test_args.json_name in
  Format.printf "%a\n%!" (Yojson.Safe.pretty_print ~std:true) j;

  let (module CT : MutableTypeTable.SAMPLE_CLASSTABLE), goal =
    CT_of_json.make_query j
  in

  let module V = JGS.FO.Verifier (CT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
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
  let () =
    if test_args.run_default then
      Printf.printf "1.1 (?) < Object : %s\n"
      @@ run_jtype ~n:test_args.answers_count (fun typ ->
             let open OCanren in
             ( -<- ) typ (jtype_inj @@ CT.object_t) !!true)
  in
  (* let e_d_b_a =
       let open JGS in
       Class
         ( class_e,
           [
             Type (Class (class_d, [ Type (Class (class_b, [])) ]));
             Type (Class (class_a, []));
           ] )
     in *)
  let (_ : JGS.jtype JGS.targ -> _) = targ_inj in
  Format.printf "Running generated query\n%!";
  print_endline @@ run_jtype (fun typ -> goal ( -<- ) jtype_inj typ)
