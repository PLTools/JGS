include struct
  open Stdlib

  type bool_counters = int * int * int

  let empty_counters = (0, 0, 0)

  type cross_bool = {
    mutable cb_both : int;
    mutable cb_left : int;
    mutable cb_right : int;
    mutable cb_both_false : int;
  }

  let cross_bool_empty () =
    { cb_both = 0; cb_left = 0; cb_right = 0; cb_both_false = 0 }

  type statistics = {
    mutable fish : (int * int) * (int * int);  (** [<-<] *)
    mutable fat_fish : bool_counters;  (** [<=<] *)
    mutable arrow : bool_counters;  (** [-<-] *)
    mutable class_int_sub : cross_bool;
    mutable cc_ground_id : int * int;
    mutable cc_ground_args : int * int; (* challow groundness *)
    mutable cc_args_fully_ground : int * int;
  }

  let get_fish st = st.fish
  let set_fish st x = st.fish <- x
  let get_fat_fish st = st.fat_fish
  let set_fat_fish st x = st.fat_fish <- x
  let get_arr st = st.arrow
  let set_arr st x = st.arrow <- x
  let get_cc st = st.cc_ground_id
  let set_cc st x = st.cc_ground_id <- x
  let get_cc_args st = st.cc_ground_args
  let set_cc_args st x = st.cc_ground_args <- x
  let get_cc_args_fully_ground st = st.cc_args_fully_ground
  let set_cc_args_fully_ground st x = st.cc_args_fully_ground <- x

  let stats =
    {
      fish = ((0, 0), (0, 0));
      class_int_sub = cross_bool_empty ();
      fat_fish = empty_counters;
      arrow = empty_counters;
      cc_ground_id = (0, 0);
      cc_ground_args = (0, 0);
      cc_args_fully_ground = (0, 0);
    }

  let clear_statistics () =
    set_fish stats ((0, 0), (0, 0));
    stats.class_int_sub <- cross_bool_empty ();
    set_fat_fish stats empty_counters;
    set_arr stats empty_counters;
    set_cc stats (0, 0);
    set_cc_args stats (0, 0);
    set_cc_args_fully_ground stats (0, 0);
    ()

  let report_counters () =
    let wrap v sum =
      float_of_int v *. 100. /. float_of_int sum |> int_of_float
    in
    let single_counter0 get stats name =
      let (taf, tag), (tbf, tbg) = get stats in

      (* let sum = taf + tag + tbf + tbg |> float_of_int in *)
      Printf.printf
        "\t\t%s:  ta %3d%% free (total=%d), tab v/g %3d%% free (total=%d)\n"
        name
        (wrap taf (taf + tag))
        (taf + tag)
        (wrap tbf (tbf + tbg))
        (tbf + tbg)
    in
    let single_counter get stats name =
      let v, tr, fa = get stats in
      let sum = v + tr + fa |> float_of_int in
      let wrap v = float_of_int v *. 100. /. sum |> int_of_float in
      Printf.printf "\t\t%s:  var %3d%%, true %3d%%,  false %3d%%, total = %d\n"
        name (wrap v) (wrap tr) (wrap fa) (int_of_float sum)
    in
    single_counter0 get_fish stats "<-<";
    single_counter get_fat_fish stats "<=<";
    single_counter get_arr stats "-<-";
    let () =
      let { cb_both; cb_left; cb_right; cb_both_false } = stats.class_int_sub in
      let sum = cb_both + cb_left + cb_right + cb_both_false in
      let to_percent x = wrap x sum in
      Printf.printf "\t\t%s:  g/g %3d%%, g/v %3d%%, v/g %3d%%, v/v %3d%%\n"
        "class_int_sub" (to_percent cb_both) (to_percent cb_left)
        (to_percent cb_right) (to_percent cb_both_false)
    in
    let () =
      let id_is_g, id_is_v = stats.cc_ground_id in
      let sum = id_is_g + id_is_v in
      let to_percent x = wrap x sum in
      Printf.printf "\t\t%s:  g %3d%%, v %3d%% (total = %d)\n" "cc_id"
        (to_percent id_is_g) (to_percent id_is_v) sum
    in
    let () =
      let id_is_g, id_is_v = stats.cc_ground_args in
      let sum = id_is_g + id_is_v in
      let to_percent x = wrap x sum in
      Printf.printf "\t\t%s:  g %3d%%, v %3d%% (total = %d)\n"
        "cc_args are shallow ground" (to_percent id_is_g) (to_percent id_is_v)
        sum
    in
    let () =
      let id_is_g, id_is_v = stats.cc_args_fully_ground in
      let sum = id_is_g + id_is_v in
      let to_percent x = wrap x sum in
      Printf.printf "\t\t%s:  g %3d%%, v %3d%% (total = %d)\n"
        "cc_args_fully_ground" (to_percent id_is_g) (to_percent id_is_v) sum
    in

    ()

  let st_add_var (get : statistics -> bool_counters)
      (set : statistics -> bool_counters -> unit) (stats : statistics) =
    let a, b, c = get stats in
    set stats Stdlib.(a + 1, b, c)

  let st_add_gen f (get : statistics -> bool_counters)
      (set : statistics -> bool_counters -> unit) (stats : statistics) =
    set stats (f (get stats))

  let st_add_true = st_add_gen (fun (a, b, c) -> Stdlib.(a, b + 1, c))
  let st_add_false = st_add_gen (fun (a, b, c) -> Stdlib.(a, b, c + 1))
  let st_add_var = st_add_gen (fun (a, b, c) -> Stdlib.(a + 1, b, c))
end
