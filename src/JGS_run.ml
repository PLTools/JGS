open OCanren
open OCanren.Std
open JGS

(**************************************************************************************************)
(************************************* Pretty-printing ********************************************)
(**************************************************************************************************)

let pp_lnat : Std.Nat.logic -> string =
 fun n ->
  let rec helper n =
    match n with
    | OCanren.Var _ -> (0, Some n)
    | Value Std.Nat.O -> (0, None)
    | Value (S n') ->
        let n, v = helper n' in
        (n + 1, v)
  in
  match helper n with
  | 0, None -> "0"
  | 0, Some v -> [%show: Std.Nat.logic] () v
  | n, None -> Printf.sprintf "%d" n
  | n, Some v -> Printf.sprintf "%d + %s" n @@ [%show: Std.Nat.logic] () v

type 'a x = 'a HO.targ

let rec pp_ltarg : HO.jtype_logic HO.targ_logic -> string =
 fun arg ->
  GT.show OCanren.logic
    (GT.show HO.targ_fuly pp_ljtype
       (GT.show Std.Option.logic
       @@ GT.show Std.Pair.logic (GT.show HO.polarity_logic) pp_ljtype))
    arg

and pp_ljtype : HO.jtype_logic -> string =
 fun t ->
  GT.show OCanren.logic
    (GT.show HO.jtype_fuly
       (GT.show Std.List.logic pp_ltarg)
       pp_lnat pp_ljtype
       (GT.show Std.Option.logic pp_ljtype)
       (GT.show Std.List.logic pp_ljtype))
    t

(**************************************************************************************************)
(**************************************** Injectors ***********************************************)
(**************************************************************************************************)

let pair_inj : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> ('b, 'd) Std.Pair.injected
    =
 fun f g (a, b) -> !!(f a, g b)

let polarity_inj : polarity -> HO.polarity_injected = function
  | Extends -> !!HO.Extends
  | Super -> !!HO.Super

let option_inj : ('a -> 'b) -> 'a option -> 'b Std.Option.injected =
 fun f -> function None -> Std.none () | Some x -> Std.some (f x)

let rec targ_inj : jtype targ -> HO.jtype_injected HO.targ_injected = function
  | Type t -> !!(HO.Type (jtype_inj t))
  | Wildcard x ->
      !!(HO.Wildcard (option_inj (pair_inj polarity_inj jtype_inj) x))

and jtype_inj : jtype -> HO.jtype_injected = function
  | Null -> !!HO.Null
  | Array t -> !!(HO.Array (jtype_inj t))
  | Class (id, args) -> !!(HO.Class (Std.nat id, Std.list targ_inj args))
  | Interface (id, args) ->
      !!(HO.Interface (Std.nat id, Std.list targ_inj args))
  | Var { id; index; upb; lwb } ->
      !!(HO.Var
           {
             id = Std.nat id;
             index = Std.nat index;
             upb = jtype_inj upb;
             lwb = option_inj jtype_inj lwb;
           })
  | Intersect l -> !!(HO.Intersect (Std.list jtype_inj l))

let idecl_inj : idecl -> HO.idecl_injected =
 fun { params; supers } ->
  !!HO.
      { params = Std.list jtype_inj params; supers = Std.list jtype_inj supers }

let cdecl_inj : cdecl -> HO.cdecl_injected =
 fun { params; super; supers } ->
  !!HO.
      {
        params = Std.list jtype_inj params;
        super = jtype_inj super;
        supers = Std.list jtype_inj supers;
      }

let decl_inj : decl -> HO.decl_injected = function
  | I i -> !!(HO.I (idecl_inj i))
  | C c -> !!(HO.C (cdecl_inj c))

(**************************************************************************************************)
(*************************** Functional-relational fuctor parameter *******************************)
(**************************************************************************************************)

module SampleCT = struct
  let reset_vars, new_id =
    let n = ref 1 in
    ( (fun () -> n := 1),
      fun () ->
        let i = !n in
        incr n;
        i )

  module M = Map.Make (struct
    type t = int

    let compare = compare
  end)

  let reset_map, add_class, add_interface, decl_by_id, decl_by_id_rel =
    let m = ref M.empty in
    ( (fun () -> m := M.empty),
      (fun c ->
        let id = new_id () in
        let d = C c in
        m := M.add id d !m;
        id),
      (fun i ->
        let id = new_id () in
        let d = I i in
        m := M.add id d !m;
        id),
      (fun id -> M.find id !m),
      fun id rez ->
        fresh id_val (id id_val)
          (let disjs =
             Stdlib.List.map (fun (k, v) ->
                 fresh () (id_val === Std.nat k) (rez === decl_inj v))
             @@ M.bindings !m
           in
           match disjs with [] -> failure | _ -> conde disjs) )

  let reset () =
    reset_vars ();
    reset_map ()

  let make_tvar index upb = Var { id = new_id (); index; upb; lwb = None }
  let make_class params super supers = add_class { params; super; supers }
  let make_interface params supers = add_interface { params; supers }
  let top = Class (0, [])

  let object_t =
    let id = make_class [] top [] in
    Class (id, [])

  let cloneable_t =
    let id = make_interface [] [] in
    Interface (id, [])

  let serializable_t =
    let id = make_interface [] [] in
    Interface (id, [])

  let new_var = new_id

  module HO = struct
    let decl_by_id = decl_by_id_rel
    let top = Class (-1, [])
    let object_t x = x === jtype_inj object_t
    let cloneable_t x = x === jtype_inj cloneable_t
    let serializable_t x = x === jtype_inj serializable_t
    let new_var _ x = x === Std.nat (new_id ())
  end
end

let sep () = Printf.printf "\n\n%s\n\n" @@ String.make 100 '*'

(**************************************************************************************************)
(************************************** Functional tests ******************************************)
(**************************************************************************************************)

let _ =
  Printf.printf "Fuctional tests:\n";

  let module V = Verifier (SampleCT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  Printf.printf " 1 Object[] < Object (true) : %b\n"
    (Array SampleCT.object_t -<- SampleCT.object_t);
  Printf.printf " 2 Object[] < Cloneable (true) : %b\n"
    (Array SampleCT.object_t -<- SampleCT.cloneable_t);
  Printf.printf " 3 Object[] < Serializable (true) : %b\n"
    (Array SampleCT.object_t -<- SampleCT.serializable_t);

  Printf.printf " 4 Object < Object[] (false): %b\n"
    (SampleCT.object_t -<- Array SampleCT.object_t);
  Printf.printf " 5 Cloneable < Object[] (false): %b\n"
    (SampleCT.cloneable_t -<- Array SampleCT.object_t);
  Printf.printf " 6 Serializable < Object[] (false): %b\n"
    (SampleCT.serializable_t -<- Array SampleCT.object_t);

  Printf.printf " 7 Object[][] < Serializable[] (true) : %b\n"
    (Array (Array SampleCT.object_t) -<- Array SampleCT.serializable_t);

  (* class A {...} *)
  let class_a = SampleCT.make_class [] SampleCT.object_t [] in

  (* class B extends A {...} *)
  let class_b = SampleCT.make_class [] (Class (class_a, [])) [] in
  Printf.printf " 8 B < A (true) : %b\n"
    (Class (class_b, []) -<- Class (class_a, []));

  (* interface IA {...} *)
  let intf_a = SampleCT.make_interface [] [] in

  (* class C extends A implements IA {...} *)
  let class_c =
    SampleCT.make_class [] (Class (class_a, [])) [ Interface (intf_a, []) ]
  in
  Printf.printf " 9 C < A (true) : %b\n"
    (Class (class_c, []) -<- Class (class_a, []));
  Printf.printf "10 C < IA (true) : %b\n"
    (Class (class_c, []) -<- Interface (intf_a, []));

  (* interface IB extends IA {...} *)
  let intf_b = SampleCT.make_interface [] [ Interface (intf_a, []) ] in
  Printf.printf "11 IB < IA (true) : %b\n"
    (Interface (intf_b, []) -<- Interface (intf_a, []));

  (* class D<X> {...} *)
  let class_d =
    SampleCT.make_class [ SampleCT.object_t ] SampleCT.object_t []
  in

  (* class E<X, Y> {...} *)
  let class_e =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      SampleCT.object_t []
  in

  (* class F<X, Y> extends E<D<Y>, X> {...} *)
  let class_f =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      (Class
         ( class_e,
           [
             Type
               (Class
                  (class_d, [ Type (SampleCT.make_tvar 1 SampleCT.object_t) ]));
             Type (SampleCT.make_tvar 0 SampleCT.object_t);
           ] ))
      []
  in
  Printf.printf "12 F<A, B> < E<D<B>, A> (true) : %b\n"
    (Class (class_f, [ Type (Class (class_a, [])); Type (Class (class_b, [])) ])
    -<- Class
          ( class_e,
            [
              Type (Class (class_d, [ Type (Class (class_b, [])) ]));
              Type (Class (class_a, []));
            ] ))

(**************************************************************************************************)
(********************************* Relational tests (forward) *************************************)
(**************************************************************************************************)

let _ =
  sep ();
  SampleCT.reset ();

  Printf.printf "\n\nRelational tests (forward):\n";
  let module V = FO.Verifier (SampleCT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  let run_bool query =
    [%show: GT.bool OCanren.logic GT.list] ()
    @@ Stream.take ~n:(-1)
    @@ run q query (fun q -> q#reify Std.Bool.reify)
  in

  Printf.printf " 1 Object[] < Object (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array SampleCT.object_t)
           (jtype_inj @@ SampleCT.object_t)
           q);

  Printf.printf " 2 Object[] < Cloneable (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array SampleCT.object_t)
           (jtype_inj @@ SampleCT.cloneable_t)
           q);

  Printf.printf " 3 Object[] < Serializable (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array SampleCT.object_t)
           (jtype_inj @@ SampleCT.serializable_t)
           q);

  Printf.printf " 4 Object < Object[] (false): %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ SampleCT.object_t)
           (jtype_inj @@ Array SampleCT.object_t)
           q);

  Printf.printf " 5 Cloneable < Object[] (false):%s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ SampleCT.cloneable_t)
           (jtype_inj @@ Array SampleCT.object_t)
           q);

  Printf.printf " 6 Serializable < Object[] (false): %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ SampleCT.serializable_t)
           (jtype_inj @@ Array SampleCT.object_t)
           q);

  Printf.printf " 7 Object[][] < Serializable[] (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Array (Array SampleCT.object_t))
           (jtype_inj @@ Array SampleCT.serializable_t)
           q);

  (* class A {...} *)
  let class_a = SampleCT.make_class [] SampleCT.object_t [] in

  (* class B extends A {...} *)
  let class_b = SampleCT.make_class [] (Class (class_a, [])) [] in
  Printf.printf " 8 B < A (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Class (class_b, []))
           (jtype_inj @@ Class (class_a, []))
           q);

  (* interface IA {...} *)
  let intf_a = SampleCT.make_interface [] [] in

  (* class C extends A implements IA {...} *)
  let class_c =
    SampleCT.make_class [] (Class (class_a, [])) [ Interface (intf_a, []) ]
  in
  Printf.printf " 9 C < A (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Class (class_c, []))
           (jtype_inj @@ Class (class_a, []))
           q);

  Printf.printf "10 C < IA (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Class (class_c, []))
           (jtype_inj @@ Interface (intf_a, []))
           q);

  (* interface IB extends IA {...} *)
  let intf_b = SampleCT.make_interface [] [ Interface (intf_a, []) ] in
  Printf.printf "11 IB < IA (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj @@ Interface (intf_b, []))
           (jtype_inj @@ Interface (intf_a, []))
           q);

  (* class D<X> {...} *)
  let class_d =
    SampleCT.make_class [ SampleCT.object_t ] SampleCT.object_t []
  in

  (* class E<X, Y> {...} *)
  let class_e =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      SampleCT.object_t []
  in

  (* class F<X, Y> extends E<D<Y>, X> {...} *)
  let class_f =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      (Class
         ( class_e,
           [
             Type
               (Class
                  (class_d, [ Type (SampleCT.make_tvar 1 SampleCT.object_t) ]));
             Type (SampleCT.make_tvar 0 SampleCT.object_t);
           ] ))
      []
  in
  Printf.printf "12 F<A, B> < E<D<B>, A> (true) : %s\n"
  @@ run_bool (fun q ->
         ( -<- )
           (jtype_inj
           @@ Class
                ( class_f,
                  [ Type (Class (class_a, [])); Type (Class (class_b, [])) ] ))
           (jtype_inj
           @@ Class
                ( class_e,
                  [
                    Type (Class (class_d, [ Type (Class (class_b, [])) ]));
                    Type (Class (class_a, []));
                  ] ))
           q)

(**************************************************************************************************)
(********************************* Relational tests (backward) ************************************)
(**************************************************************************************************)

let _ =
  SampleCT.reset ();

  sep ();
  Printf.printf "\n\nRelational tests (backward):\n";
  let module V = FO.Verifier (SampleCT) in
  let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
  and ( -<- ) ta tb = V.( -<- ) ( <-< ) ta tb in

  let run_jtype ?(n = -1) query =
    let pp_list f l =
      Printf.sprintf "\n[\n  %s\n]%!"
      @@ String.concat ";\n  " @@ Stdlib.List.map f l
    in
    pp_list pp_ljtype @@ Stream.take ~n
    @@ run q query (fun q -> q#reify HO.jtype_reify)
  in

  Printf.printf "1.1 (?) < Object : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ SampleCT.object_t) !!true);

  sep ();

  Printf.printf "1.2 Object[] < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Array SampleCT.object_t) q !!true);

  sep ();

  Printf.printf "2 (?) < Cloneable : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ SampleCT.cloneable_t) !!true);

  sep ();

  Printf.printf "3 (?) < Serializable : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ SampleCT.serializable_t) !!true);

  sep ();

  Printf.printf "4.1 (?) < Object[] : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ Array SampleCT.object_t) !!true);

  sep ();

  Printf.printf "4.2 Object < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ SampleCT.object_t) q !!true);

  sep ();

  Printf.printf "5 Cloneable < (?): %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ SampleCT.cloneable_t) q !!true);

  sep ();

  Printf.printf "6 Serializable < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ SampleCT.serializable_t) q !!true);

  sep ();

  Printf.printf "7.1 (?) < Serializable[] : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ Array SampleCT.serializable_t) !!true);

  sep ();

  Printf.printf "7.2 Object[][] < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Array (Array SampleCT.object_t)) q !!true);

  sep ();

  (* class A {...} *)
  let class_a = SampleCT.make_class [] SampleCT.object_t [] in

  (* class B extends A {...} *)
  let class_b = SampleCT.make_class [] (Class (class_a, [])) [] in

  Printf.printf "Class A: %d\n\n" class_a;

  Printf.printf "Class B: %d\n\n" class_b;

  Printf.printf "8.1 (?) < A : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ Class (class_a, [])) !!true);

  sep ();

  Printf.printf "8.2 B < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Class (class_b, [])) q !!true);

  sep ();

  Printf.printf "8.3 (?) < B : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ Class (class_b, [])) !!true);

  sep ();

  Printf.printf "8.4 A < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Class (class_a, [])) q !!true);

  sep ();

  (* interface IA {...} *)
  let intf_a = SampleCT.make_interface [] [] in

  (* class C extends A implements IA {...} *)
  let class_c =
    SampleCT.make_class [] (Class (class_a, [])) [ Interface (intf_a, []) ]
  in

  Printf.printf "Interface A: %d\n\n" intf_a;

  Printf.printf "Class C: %d\n\n" class_c;

  Printf.printf "9 C < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Class (class_c, [])) q !!true);

  sep ();

  Printf.printf "10.1 (?) < IA : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         ( -<- ) q (jtype_inj @@ Interface (intf_a, [])) !!true);

  sep ();

  Printf.printf "10.2 C < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Class (class_c, [])) q !!true);

  sep ();

  (* interface IB extends IA {...} *)
  let intf_b = SampleCT.make_interface [] [ Interface (intf_a, []) ] in

  Printf.printf "Interface B: %d\n\n" intf_b;

  Printf.printf "11 IB < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q ->
         ( -<- ) (jtype_inj @@ Interface (intf_b, [])) q !!true);

  sep ();

  (* class D<X> {...} *)
  let class_d =
    SampleCT.make_class [ SampleCT.object_t ] SampleCT.object_t []
  in

  (* class E<X, Y> {...} *)
  let class_e =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      SampleCT.object_t []
  in

  (* class F<X, Y> extends E<D<Y>, X> {...} *)
  let class_f =
    SampleCT.make_class
      [ SampleCT.object_t; SampleCT.object_t ]
      (Class
         ( class_e,
           [
             Type
               (Class
                  (class_d, [ Type (SampleCT.make_tvar 1 SampleCT.object_t) ]));
             Type (SampleCT.make_tvar 0 SampleCT.object_t);
           ] ))
      []
  in
  Printf.printf "Class D<X>: %d\n\n" class_d;
  Printf.printf "Class E<X, Y>: %d\n\n" class_e;
  Printf.printf "Class F<X, Y> : %d\n\n" class_f;

  let f_ab =
    Class (class_f, [ Type (Class (class_a, [])); Type (Class (class_b, [])) ])
  in

  let e_d_b_a =
    Class
      ( class_e,
        [
          Type (Class (class_d, [ Type (Class (class_b, [])) ]));
          Type (Class (class_a, []));
        ] )
  in

  Printf.printf "12.1 (?) < E<D<B>, A> : %s\n"
  @@ run_jtype ~n:10 (fun q -> ( -<- ) q (jtype_inj @@ e_d_b_a) !!true);

  sep ();

  Printf.printf "12.2 (? - is class) < E<D<B>, A> : %s\n"
  @@ run_jtype ~n:10 (fun q ->
         fresh (a b)
           (q === !!(HO.Class (a, b)))
           (( -<- ) q (jtype_inj @@ e_d_b_a) !!true));

  sep ();

  Printf.printf "12.3 F<A, B> < (?) : %s\n"
  @@ run_jtype ~n:(-1) (fun q -> ( -<- ) (jtype_inj @@ f_ab) q !!true)
