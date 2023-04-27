let () =
  let open JGS_Helpers in
  let jsonname = Sys.argv.(1) in
  let j = Yojson.Safe.from_file jsonname in
  Format.printf "%a\n%!" (Yojson.Safe.pretty_print ~std:true) j;

  let (module CT : MutableTypeTable.SAMPLE_CLASSTABLE) =
    CT_of_json.make_classtable j
  in
  let module V = JGS.FO.Verifier (CT) in
  let run_jtype ?(n = -1) query =
    let pp_list f l =
      Printf.sprintf "\n[\n  %s\n]%!"
      @@ String.concat ";\n  " @@ Stdlib.List.map f l
    in
    pp_list pp_ljtype @@ OCanren.Stream.take ~n
    @@ OCanren.(run q) query (fun q -> q#reify JGS.HO.jtype_reify)
  in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  Printf.printf "1.1 (?) < Object : %s\n"
  @@ run_jtype ~n:10 (fun typ ->
         let open OCanren in
         ( -<- ) typ (jtype_inj @@ CT.object_t) !!true)
