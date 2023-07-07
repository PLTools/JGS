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
  }

  let get_fish st = st.fish
  let set_fish st x = st.fish <- x
  let get_fat_fish st = st.fat_fish
  let set_fat_fish st x = st.fat_fish <- x
  let get_arr st = st.arrow
  let set_arr st x = st.arrow <- x

  let stats =
    {
      fish = ((0, 0), (0, 0));
      class_int_sub = cross_bool_empty ();
      fat_fish = empty_counters;
      arrow = empty_counters;
    }

  let clear_statistics () =
    set_fish stats ((0, 0), (0, 0));
    set_fat_fish stats empty_counters;
    set_arr stats empty_counters

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
