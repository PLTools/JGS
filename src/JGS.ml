let need_simpified = ref false

[@@@ocaml.warning "-8"]

open Option
open Peano
open List
open Option

type polarity = Extends | Super
type 'jtype targ = Type of 'jtype | Wildcard of (polarity * 'jtype) option

type jtype =
  | Array of jtype
  | Class of int * jtype targ list
  | Interface of int * jtype targ list
  | Var of { id : int; index : nat; upb : jtype; lwb : jtype option }
  | Null
  | Intersect of jtype list

type cdecl = { params : jtype list; super : jtype; supers : jtype list }
type idecl = { params : jtype list; supers : jtype list }
type decl = C of cdecl | I of idecl
type capture_conversion_subst = CC_inter of jtype * jtype | CC_subst of jtype

type capture_conversion_type =
  | CC_type of jtype
  | CC_var of int * nat * capture_conversion_subst * jtype option

let rec substitute_typ subst = function
  | Array typ -> Array (substitute_typ subst typ)
  | Class (id, args) -> Class (id, List.map (substitute_arg subst) args)
  | Interface (id, args) -> Interface (id, List.map (substitute_arg subst) args)
  | Intersect typs -> Intersect (List.map (substitute_typ subst) typs)
  | Var _ -> failwith "*** should not happen ***"
  | Null -> Null

and substitute_arg subst = function
  | Type (Var { index; _ }) -> List.nth subst index
  | Type typ -> Type (substitute_typ subst typ)
  | Wildcard None -> Wildcard None
  | Wildcard (Some (p, typ1)) -> Wildcard (Some (p, substitute_typ subst typ1))

module Verifier (CT : sig
  val decl_by_id : int -> decl
  val get_superclass_by_id : int -> int -> jtype option
  val object_t : jtype
  val cloneable_t : jtype
  val serializable_t : jtype
  val new_var : unit -> int
end) =
struct
  let rec ( -<- ) (( <-< ) : jtype -> jtype -> bool) (ta : jtype) (tb : jtype) =
    let ( <=< ) ta tb =
      match (ta, tb) with
      | Wildcard (Some (Extends, t)), Wildcard (Some (Extends, s)) -> t <-< s
      | Wildcard (Some (Extends, _)), Wildcard None -> true
      | Wildcard (Some (Super, t)), Wildcard (Some (Super, s)) -> s <-< t
      | Wildcard (Some (Super, _)), Wildcard None -> true
      | Wildcard (Some (Super, _)), Wildcard (Some (Extends, o)) ->
          o = CT.object_t
      | Type t1, Type t2
      | Type t1, Wildcard (Some (Extends, t2))
      | Type t1, Wildcard (Some (Super, t2)) ->
          t1 = t2
      | _ -> false
    in
    let capture_conversion id targs =
      let params =
        match CT.decl_by_id id with
        | C { params; _ } | I { params; _ } -> params
      in
      let raw =
        List.mapi
          (fun i -> function
            | Type t -> CC_type t
            | Wildcard None ->
                CC_var
                  (CT.new_var (), i, CC_subst (List.nth params i), Some Null)
            | Wildcard (Some (Super, t)) ->
                CC_var (CT.new_var (), i, CC_subst (List.nth params i), Some t)
            | Wildcard (Some (Extends, t)) ->
                CC_var
                  (CT.new_var (), i, CC_inter (t, List.nth params i), Some Null))
          targs
      in
      let subst =
        List.map
          (function
            | CC_type t -> Type t
            | CC_var (id, i, _, _) ->
                Type (Var { lwb = None; upb = Null; index = i; id }))
          raw
      in
      let targs =
        List.map
          (function
            | CC_type t -> Type (substitute_typ subst t)
            | CC_var (id, i, CC_subst p, lwb) ->
                Type (Var { lwb; upb = substitute_typ subst p; index = i; id })
            | CC_var (id, i, CC_inter (t, p), lwb) ->
                Type
                  (Var
                     {
                       lwb;
                       upb =
                         (match substitute_typ subst p with
                         | Intersect ts -> Intersect (t :: ts)
                         | typ -> Intersect [ t; typ ]);
                       index = i;
                       id;
                     }))
          raw
      in
      if
        List.for_all
          (function
            | Type (Var { upb; lwb = Some lwb; _ }) -> lwb <-< upb | _ -> true)
          targs
      then Some targs
      else None
    in
    let class_int_sub id_a targs_a id_b targs_b =
      if id_a = id_b then
        List.fold_left2 (fun f ta tb -> f && ta <=< tb) true targs_a targs_b
      else
        match CT.get_superclass_by_id id_a id_b with
        | Some (Class (_, targs_b')) | Some (Interface (_, targs_b')) ->
            targs_b = List.map (fun t -> substitute_arg targs_a t) targs_b'
        | None -> false
    in
    let ( -<- ) = ( -<- ) ( <-< ) in
    match ta with
    | Class (id_a, targs_a) -> (
        match capture_conversion id_a targs_a with
        | None -> false
        | Some targs_a -> (
            match tb with
            | Interface (id_b, targs_b) | Class (id_b, targs_b) ->
                class_int_sub id_a targs_a id_b targs_b
            | Var { lwb = Some typ; _ } -> typ = ta
            | _ -> false))
    | Interface (id_a, targs_a) -> (
        match capture_conversion id_a targs_a with
        | None -> false
        | Some targs_a -> (
            match tb with
            | Class (id_b, targs_b) | Interface (id_b, targs_b) ->
                class_int_sub id_a targs_a id_b targs_b
            | Var { lwb = Some typ; _ } -> typ = ta
            | _ -> false))
    | Array ta -> (
        if ta = CT.object_t then
          if tb = CT.object_t || tb = CT.cloneable_t || tb = CT.serializable_t
          then true
          else match tb with Array tb -> ta -<- tb | _ -> false
        else match tb with Array tb -> ta -<- tb | _ -> false)
    | Intersect ts -> List.mem tb ts
    | Var { upb = typ; _ } -> typ = tb
    | Null -> tb <> Null
end

[@@@ocaml.warning "+8"]

open GT
open OCanren

module HO = struct
  open Option.HO
  open Peano.HO
  open List.HO
  open Option.HO

  [%%distrib
  type polarity = Extends | Super [@@deriving gt ~options:{ show; fmt; gmap }]]

  [%%distrib
  type 'jtype targ = Type of 'jtype | Wildcard of (polarity * 'jtype) option
  [@@deriving gt ~options:{ show; fmt; gmap }]]

  [%%distrib
  type jtype =
    | Array of jtype
    | Class of int * jtype targ list
    | Interface of int * jtype targ list
    | Var of { id : int; index : nat; upb : jtype; lwb : jtype option }
    | Null
    | Intersect of jtype list
  [@@deriving gt ~options:{ show; fmt; gmap }]]

  [%%distrib
  type cdecl = { params : jtype list; super : jtype; supers : jtype list }
  [@@deriving gt ~options:{ show; fmt; gmap }]]

  let ctor_cdecl params super supers = inj { params; super; supers }

  [%%distrib
  type idecl = { params : jtype list; supers : jtype list }
  [@@deriving gt ~options:{ show; fmt; gmap }]]

  let ctor_idecl params supers = inj { params; supers }
  let var id index upb lwb = !!(Var { id; index; upb; lwb })

  [%%distrib
  type decl = C of cdecl | I of idecl
  [@@deriving gt ~options:{ show; fmt; gmap }]]

  [%%distrib
  type capture_conversion_subst =
    | CC_inter of jtype * jtype
    | CC_subst of jtype
  [@@deriving gt ~options:{ show; fmt; gmap }]]

  [%%distrib
  type capture_conversion_type =
    | CC_type of jtype
    | CC_var of int * nat * capture_conversion_subst * jtype option
  [@@deriving gt ~options:{ show; fmt; gmap }]]

  let cc_var id index subst t = !!(CC_var (id, index, subst, t))

  module Util = struct
    let rec mapio_helper :
        (Std.Nat.injected -> 'a ilogic -> 'b ilogic -> goal) ->
        Std.Nat.injected ->
        'a ilogic Std.List.injected ->
        'b ilogic Std.List.injected ->
        goal =
     fun f i l res ->
      let open Std in
      conde
        [
          fresh () (l === nil ()) (res === nil ());
          fresh (l_hd l_tl res_hd res_tl)
            (l === l_hd % l_tl)
            (res === res_hd % res_tl)
            (f i l_hd res_hd)
            (mapio_helper f (Nat.s i) l_tl res_tl);
        ]

    let mapio :
        (Std.Nat.injected -> 'a ilogic -> 'b ilogic -> goal) ->
        'a ilogic Std.List.injected ->
        'b ilogic Std.List.injected ->
        goal =
     fun f l res -> mapio_helper f Std.Nat.o l res

    let rec memo :
        'a ilogic -> 'a ilogic Std.List.injected -> bool ilogic -> goal =
     fun e l res ->
      let open Std in
      conde
        [
          fresh () (l === nil ()) (res === !!false);
          fresh (hd tl)
            (l === hd % tl)
            (conde
               [
                 fresh () (hd === e) (res === !!true);
                 fresh () (hd =/= e) (memo e tl res);
               ]);
        ]

    let rec ntho :
        'a ilogic Std.List.injected -> Std.Nat.injected -> 'a ilogic -> goal =
     fun l n rez ->
      let open Std in
      fresh (hd tl)
        (l === hd % tl)
        (conde
           [
             fresh () (n === Nat.o) (hd === rez);
             fresh n' (n === Nat.s n') (ntho tl n' rez);
           ])

    let rec for_allo :
        ('a ilogic -> bool ilogic -> goal) ->
        'a ilogic Std.List.injected ->
        bool ilogic ->
        goal =
     fun p l res ->
      let open Std in
      conde
        [
          fresh () (l === nil ()) (res === !!true);
          fresh (hd tl p_res)
            (l === hd % tl)
            (p hd p_res)
            (conde
               [
                 fresh () (p_res === !!true) (for_allo p tl res);
                 fresh () (p_res === !!false) (res === !!false);
               ]);
        ]

    let rec fold_left2o :
        ('a ilogic -> 'b ilogic -> 'c ilogic -> 'a ilogic -> goal) ->
        'a ilogic ->
        'b ilogic Std.List.injected ->
        'c ilogic Std.List.injected ->
        'a ilogic ->
        goal =
     fun f acc l1 l2 res ->
      let open Std in
      conde
        [
          fresh () (l1 === nil ()) (l2 === nil ()) (res === acc);
          fresh (hd1 tl1 hd2 tl2 new_acc)
            (l1 === hd1 % tl1)
            (l2 === hd2 % tl2)
            (f acc hd1 hd2 new_acc)
            (fold_left2o f new_acc tl1 tl2 res);
        ]
  end

  let rec substitute_typ :
      jtype_injected targ_injected Std.List.injected ->
      jtype_injected ->
      jtype_injected ->
      goal =
   fun subst typ res ->
    conde
      [
        fresh (param new_param) (typ === !!(Array param))
          (res === !!(Array new_param))
          (substitute_typ subst param new_param);
        fresh (id args new_args)
          (typ === !!(Class (id, args)))
          (res === !!(Class (id, new_args)))
          (Std.List.mapo
             (fun targ res -> substitute_arg subst targ res)
             args new_args);
        fresh (id args new_args)
          (typ === !!(Interface (id, args)))
          (res === !!(Interface (id, new_args)))
          (Std.List.mapo
             (fun targ res -> substitute_arg subst targ res)
             args new_args);
        fresh (typs new_typs)
          (typ === !!(Intersect typs))
          (res === !!(Intersect new_typs))
          (Std.List.mapo
             (fun typ res -> substitute_typ subst typ res)
             typs new_typs);
        fresh () (typ === !!Null) (res === !!Null);
      ]

  and substitute_arg :
      jtype_injected targ_injected Std.List.injected ->
      jtype_injected targ_injected ->
      jtype_injected targ_injected ->
      goal =
   fun subst targ res ->
    conde
      [
        fresh index
          (targ === !!(Type (var __ index __ __)))
          (Util.ntho subst index res);
        fresh (typ new_typ) (targ === !!(Type typ)) (res === !!(Type new_typ))
          (substitute_typ subst typ new_typ);
        fresh () (targ === !!(Wildcard !!None)) (res === !!(Wildcard !!None));
        fresh (p typ new_typ)
          (targ === !!(Wildcard !!(Some (Std.pair p typ))))
          (res === !!(Wildcard !!(Some (Std.pair p new_typ))))
          (substitute_typ subst typ new_typ);
      ]

  module Verifier (CT : sig
    module HO : sig
      val decl_by_id : int ilogic -> decl_injected -> OCanren.goal

      val get_superclass_by_id :
        ?from:int ->
        int ilogic ->
        int ilogic ->
        jtype_injected option_injected ->
        OCanren.goal

      val object_t : jtype_injected
      val cloneable_t : jtype_injected
      val serializable_t : jtype_injected
      val new_var : unit -> int ilogic
    end

    val pp_targ : Format.formatter -> jtype_logic targ_logic -> unit
    val pp_jtyp : Format.formatter -> jtype_logic -> unit
  end) =
  struct
    let params : int ilogic -> jtype_injected Std.List.injected -> goal =
     fun id p ->
      fresh decl (CT.HO.decl_by_id id decl)
        (conde
           [
             decl === !!(C (ctor_cdecl p __ __));
             decl === !!(I (ctor_idecl p __));
           ])

    let raw_helper :
        int ilogic ->
        Std.Nat.injected ->
        jtype_injected targ_injected ->
        capture_conversion_type_injected ->
        goal =
     fun id i targ cc_targ ->
      conde
        [
          fresh t (targ === !!(Type t)) (cc_targ === !!(CC_type t));
          fresh (param params_val)
            (targ === !!(Wildcard !!None))
            (cc_targ
            === cc_var (CT.HO.new_var ()) i !!(CC_subst param) !!(Some !!Null))
            (params id params_val)
            (Util.ntho params_val i param);
          fresh (t subst params_val)
            (targ === !!(Wildcard !!(Some (Std.pair !!Super t))))
            (cc_targ
            === cc_var (CT.HO.new_var ()) i !!(CC_subst subst) !!(Some t))
            (params id params_val)
            (Util.ntho params_val i subst);
          fresh (t t2 params_val)
            (targ === !!(Wildcard !!(Some (Std.pair !!Extends t))))
            (cc_targ
            === cc_var (CT.HO.new_var ()) i !!(CC_inter (t, t2)) !!(Some !!Null)
            )
            (params id params_val)
            (Util.ntho params_val i t2);
        ]

    let subst_helper :
        capture_conversion_type_injected -> jtype_injected targ_injected -> goal
        =
     fun raw_element targ ->
      conde
        [
          fresh t (raw_element === !!(CC_type t)) (targ === !!(Type t));
          fresh (id i)
            (raw_element === cc_var id i __ __)
            (targ === !!(Type (var id i !!Null !!None)));
        ]

    let targs_helper :
        jtype_injected targ_injected list_injected ->
        capture_conversion_type_injected ->
        jtype_injected targ_injected ->
        goal =
     fun subst cc_typ res ->
      conde
        [
          fresh (t new_t) (cc_typ === !!(CC_type t)) (res === !!(Type new_t))
            (substitute_typ subst t new_t);
          fresh (id i p new_p lwb)
            (cc_typ === cc_var id i !!(CC_subst p) lwb)
            (res === !!(Type (var id i new_p lwb)))
            (substitute_typ subst p new_p);
          fresh (id i t p lwb upb new_p)
            (cc_typ === cc_var id i !!(CC_inter (t, p)) lwb)
            (res === !!(Type (var id i upb lwb)))
            (substitute_typ subst p new_p)
            (conde
               [
                 fresh ts
                   (new_p === !!(Intersect ts))
                   (upb === !!(Intersect Std.(t % ts)));
                 fresh ()
                   (upb === !!(Intersect (Std.list Fun.id [ t; new_p ])))
                   (new_p =/= !!(Intersect __));
               ]);
        ]

    let targs_pred :
        (jtype_injected -> jtype_injected -> bool ilogic -> goal) ->
        jtype_injected targ_injected ->
        bool ilogic ->
        goal =
     fun ( <-< ) targ res ->
      conde
        [
          fresh (upb lwb)
            (targ === !!(Type (var __ __ upb !!(Some lwb))))
            (( <-< ) lwb upb res);
          fresh () (res === !!true)
            (targ =/= !!(Type (var __ __ __ !!(Some __))));
        ]

    let capture_conversion :
        ?from:int ->
        (jtype_injected -> jtype_injected -> bool ilogic -> goal) ->
        int ilogic ->
        jtype_injected targ_injected Std.List.injected ->
        jtype_injected targ_injected Std.List.injected option ilogic ->
        goal =
     fun ?(from = 0) ( <-< ) id targs res st ->
      let () =
        if JGS_stats.config.trace_cc then
          Format.printf
            "capture_conversion(%d): id = %a, targs = %a, rez = %a\n%!" from
            (GT.fmt OCanren.logic (GT.fmt GT.int))
            (OCanren.reify_in_state st OCanren.reify id)
            (GT.fmt OCanren.Std.List.logic CT.pp_targ)
            (OCanren.reify_in_state st
               (Std.List.reify @@ targ_reify jtype_reify)
               targs)
            (GT.fmt Std.Option.logic @@ GT.fmt OCanren.Std.List.logic CT.pp_targ)
            (OCanren.reify_in_state st
               (Std.Option.reify (Std.List.reify @@ targ_reify jtype_reify))
               res);
        if JGS_stats.config.enable_counters then (
          let open JGS_stats in
          let change b =
            let open Stdlib in
            if b then fun (a, b) -> (a + 1, b) else fun (a, b) -> (a, b + 1)
          in
          OCanren.is_ground id st (fun b ->
              set_cc stats (change b (get_cc stats)));
          OCanren.is_ground targs st (fun b ->
              set_cc_args stats (change b (get_cc_args stats)));
          match
            OCanren.reify_in_state st
              (Std.List.prj_exn @@ targ_prj_exn jtype_prj_exn)
              targs
          with
          | exception OCanren.Not_a_value ->
              set_cc_args_fully_ground stats
                (change false (get_cc_args_fully_ground stats))
          | _ ->
              set_cc_args_fully_ground stats
                (change true (get_cc_args_fully_ground stats)))
      in

      st
      |>
      if !need_simpified then res === !!(Some targs)
      else
        fresh
          (raw subst new_targs are_correct_targs)
          (Util.mapio
             (fun targ cc_targ -> raw_helper id targ cc_targ)
             targs raw)
          (Std.List.mapo subst_helper raw subst)
          (Std.List.mapo
             (fun cc_typ res -> targs_helper subst cc_typ res)
             raw new_targs)
          (Util.for_allo
             (fun targ res -> targs_pred ( <-< ) targ res)
             new_targs are_correct_targs)
          (conde
             [
               fresh () (are_correct_targs === !!true) (res === !!(Some targs));
               fresh () (are_correct_targs === !!false) (res === !!None);
             ])

    let rec ( <=< ) :
        _ -> jtype_injected targ_injected -> jtype_injected targ_injected -> _ =
     fun ( <-< ) type_a type_b res : _ ->
      fun st ->
       let () =
         if JGS_stats.config.enable_counters then
           let open JGS_stats in
           OCanren.is_ground_bool res st
             ~on_ground:(fun b ->
               (if b then st_add_true else st_add_false)
                 get_fat_fish set_fat_fish stats)
             ~onvar:(fun () -> st_add_var get_fat_fish set_fat_fish stats)
       in
       if !need_simpified then
         conde
           [
             fresh () (type_a === type_b) (res === !!true);
             fresh () (type_a =/= type_b) (res === !!false);
           ]
           st
       else
         conde
           [
             fresh (t s)
               (type_a === !!(Wildcard !!(Some (Std.pair !!Extends t))))
               (type_b === !!(Wildcard !!(Some (Std.pair !!Extends s))))
               (( <-< ) t s res);
             fresh t
               (type_a === !!(Wildcard !!(Some (Std.pair !!Extends t))))
               (type_b === !!(Wildcard !!None))
               (res === !!true);
             fresh (t s)
               (type_a === !!(Wildcard !!(Some (Std.pair !!Super t))))
               (type_b === !!(Wildcard !!(Some (Std.pair !!Super s))))
               (( <-< ) s t res);
             fresh t
               (type_a === !!(Wildcard !!(Some (Std.pair !!Super t))))
               (type_b === !!(Wildcard !!None))
               (res === !!true);
             fresh (t o)
               (type_a === !!(Wildcard !!(Some (Std.pair !!Super t))))
               (type_b === !!(Wildcard !!(Some (Std.pair !!Extends o))))
               (conde
                  [
                    fresh () (o === CT.HO.object_t) (res === !!true);
                    fresh () (res === !!false) (o =/= CT.HO.object_t);
                  ]);
             fresh (t1 t2) (type_a === !!(Type t1))
               (conde
                  [
                    type_b === !!(Type t2);
                    type_b === !!(Wildcard !!(Some (Std.pair !!Extends t2)));
                    type_b === !!(Wildcard !!(Some (Std.pair !!Super t2)));
                  ])
               (conde
                  [
                    fresh () (t1 === t2) (res === !!true);
                    fresh () (t1 =/= t2) (res === !!false);
                  ]);
             fresh () (res === !!false)
               (conde [ type_a =/= !!(Type __); type_b =/= !!(Type __) ])
               (conde
                  [
                    type_a =/= !!(Type __);
                    type_b =/= !!(Wildcard !!(Some (Std.pair !!Extends __)));
                  ])
               (conde
                  [
                    type_a =/= !!(Type __);
                    type_b =/= !!(Wildcard !!(Some (Std.pair !!Super __)));
                  ])
               (conde
                  [
                    type_a =/= !!(Wildcard !!(Some (Std.pair !!Super __)));
                    type_b =/= !!(Wildcard !!(Some (Std.pair !!Extends __)));
                  ])
               (conde
                  [
                    type_a =/= !!(Wildcard !!(Some (Std.pair !!Super __)));
                    type_b =/= !!(Wildcard !!None);
                  ])
               (conde
                  [
                    type_a =/= !!(Wildcard !!(Some (Std.pair !!Super __)));
                    type_b =/= !!(Wildcard !!(Some (Std.pair !!Super __)));
                  ])
               (conde
                  [
                    type_a =/= !!(Wildcard !!(Some (Std.pair !!Extends __)));
                    type_b =/= !!(Wildcard !!None);
                  ])
               (conde
                  [
                    type_a =/= !!(Wildcard !!(Some (Std.pair !!Extends __)));
                    type_b =/= !!(Wildcard !!(Some (Std.pair !!Extends __)));
                  ]);
           ]
           st

    and class_int_sub :
        (jtype_injected -> jtype_injected -> bool ilogic -> goal) ->
        int ilogic ->
        jtype_injected targ_injected Std.List.injected ->
        int ilogic ->
        jtype_injected targ_injected Std.List.injected ->
        bool ilogic ->
        goal =
     fun ( <-< ) id_a targs_a id_b targs_b res st ->
      let () =
        let open JGS_stats in
        let open Stdlib in
        let ta_is_g = ref false in
        let tb_is_g = ref false in
        OCanren.is_ground id_a st (fun b -> ta_is_g := b);
        OCanren.is_ground id_b st (fun b -> tb_is_g := b);
        match (!ta_is_g, !tb_is_g) with
        | true, true ->
            stats.class_int_sub.cb_both <- stats.class_int_sub.cb_both + 1
        | true, false ->
            stats.class_int_sub.cb_left <- stats.class_int_sub.cb_left + 1
        | false, true ->
            stats.class_int_sub.cb_right <- stats.class_int_sub.cb_right + 1
        | false, false ->
            stats.class_int_sub.cb_both_false <-
              stats.class_int_sub.cb_both_false + 1
      in

      st
      |> conde
           [
             fresh () (id_a === id_b)
               (Util.fold_left2o
                  (fun acc ta tb res ->
                    conde
                      [
                        fresh () (acc === !!false) (res === !!false);
                        fresh () (acc === !!true) (( <=< ) ( <-< ) ta tb res);
                      ])
                  !!true targs_a targs_b res);
             fresh super (id_a =/= id_b)
               (CT.HO.get_superclass_by_id id_a id_b super)
               (conde
                  [
                    fresh (targs_b' new_targs_b')
                      (conde
                         [
                           super === !!(Some !!(Class (__, targs_b')));
                           super === !!(Some !!(Interface (__, targs_b')));
                         ])
                      (Std.List.mapo
                         (fun arg res -> substitute_arg targs_a arg res)
                         targs_b' new_targs_b')
                      (conde
                         [
                           fresh () (targs_b === new_targs_b') (res === !!true);
                           fresh () (res === !!false) (targs_b =/= new_targs_b');
                         ]);
                    fresh () (super === !!None) (res === !!false);
                  ]);
           ]

    and ( -<- ) :
        (jtype_injected -> jtype_injected -> bool ilogic -> goal) ->
        jtype_injected ->
        jtype_injected ->
        bool ilogic ->
        goal =
     fun ( <-< ) type_a type_b res st ->
      let () =
        (if JGS_stats.config.enable_counters then
           let open JGS_stats in
           OCanren.is_ground_bool res st
             ~on_ground:(fun b ->
               (if b then st_add_true else st_add_false) get_arr set_arr stats)
             ~onvar:(fun () -> st_add_var get_arr set_arr stats));
        if JGS_stats.config.trace_arrow then
          Format.printf " -<-: type_a = %a, type_b = %a, rez = %a\n%!"
            CT.pp_jtyp
            (OCanren.reify_in_state st jtype_reify type_a)
            CT.pp_jtyp
            (OCanren.reify_in_state st jtype_reify type_b)
            (GT.fmt OCanren.logic (GT.fmt GT.bool))
            (OCanren.reify_in_state st OCanren.reify res)
      in

      st
      |> conde
           [
             fresh (id_a targs_a converted)
               (type_a === !!(Class (id_a, targs_a)))
               (capture_conversion ( <-< ) id_a targs_a converted)
               (conde
                  [
                    fresh () (converted === !!None) (res === !!false);
                    fresh targs_a
                      (converted === !!(Some targs_a))
                      (conde
                         [
                           fresh (id_b targs_b)
                             (conde
                                [
                                  type_b === !!(Interface (id_b, targs_b));
                                  type_b === !!(Class (id_b, targs_b));
                                ])
                             (class_int_sub ( <-< ) id_a targs_a id_b targs_b
                                res);
                           fresh typ
                             (type_b === var __ __ __ !!(Some typ))
                             (conde
                                [
                                  fresh () (typ === type_a) (res === !!true);
                                  fresh () (res === !!false) (typ =/= type_a);
                                ]);
                           fresh () (res === !!false)
                             (type_b =/= var __ __ __ !!(Some __))
                             (type_b =/= !!(Interface (__, __)))
                             (type_b =/= !!(Class (__, __)));
                         ]);
                  ]);
             fresh (id_a targs_a converted)
               (type_a === !!(Interface (id_a, targs_a)))
               (capture_conversion ( <-< ) id_a targs_a converted)
               (conde
                  [
                    fresh () (converted === !!None) (res === !!false);
                    fresh targs_a
                      (converted === !!(Some targs_a))
                      (conde
                         [
                           fresh (id_b targs_b)
                             (conde
                                [
                                  type_b === !!(Class (id_b, targs_b));
                                  type_b === !!(Interface (id_b, targs_b));
                                ])
                             (class_int_sub ( <-< ) id_a targs_a id_b targs_b
                                res);
                           fresh (__FILE__ __ typ)
                             (type_b === var __ __ __ !!(Some typ))
                             (conde
                                [
                                  fresh () (typ === type_a) (res === !!true);
                                  fresh () (res === !!false) (typ =/= type_a);
                                ]);
                           fresh () (res === !!false)
                             (type_b =/= var __ __ __ !!(Some __))
                             (type_b =/= !!(Class (__, __)))
                             (type_b =/= !!(Interface (__, __)));
                         ]);
                  ]);
             fresh ta (type_a === !!(Array ta))
               (conde
                  [
                    fresh () (ta === CT.HO.object_t)
                      (conde
                         [
                           fresh () (res === !!true)
                             (conde
                                [
                                  type_b === CT.HO.object_t;
                                  type_b === CT.HO.cloneable_t;
                                  type_b === CT.HO.serializable_t;
                                ]);
                           fresh ()
                             (type_b =/= CT.HO.object_t)
                             (type_b =/= CT.HO.serializable_t)
                             (type_b =/= CT.HO.cloneable_t)
                             (conde
                                [
                                  fresh tb (type_b === !!(Array tb))
                                    (( -<- ) ( <-< ) ta tb res);
                                  fresh () (res === !!false)
                                    (type_b =/= !!(Array __));
                                ]);
                         ]);
                    fresh () (ta =/= CT.HO.object_t)
                      (conde
                         [
                           fresh tb (type_b === !!(Array tb))
                             (( -<- ) ( <-< ) ta tb res);
                           fresh () (res === !!false) (type_b =/= !!(Array __));
                         ]);
                  ]);
             fresh ts (type_a === !!(Intersect ts)) (Util.memo type_b ts res);
             fresh upb_typ
               (type_a === var __ __ upb_typ __)
               (conde
                  [
                    fresh () (upb_typ === type_b) (res === !!true);
                    fresh () (res === !!false) (upb_typ =/= type_b);
                  ]);
             fresh () (type_a === !!Null)
               (conde
                  [
                    fresh () (type_b === !!Null) (res === !!false);
                    fresh () (res === !!true) (type_b =/= !!Null);
                  ]);
           ]
  end
end

module FO = struct
  open Option.HO
  open Peano.HO
  open List.HO
  open Option.HO
  open HO

  let substitute_typ q33 q32 q31 = substitute_typ q33 q32 q31
  let substitute_arg q66 q65 q64 = substitute_arg q66 q65 q64

  module Verifier (CT : sig
    module HO : sig
      val decl_by_id : int ilogic -> decl_injected -> OCanren.goal

      val get_superclass_by_id :
        ?from:int ->
        int ilogic ->
        int ilogic ->
        jtype_injected option_injected ->
        OCanren.goal

      val object_t : jtype_injected
      val cloneable_t : jtype_injected
      val serializable_t : jtype_injected
      val new_var : unit -> int ilogic
    end

    val pp_targ : Format.formatter -> jtype_logic targ_logic -> unit
    val pp_jtyp : Format.formatter -> jtype_logic -> unit
  end) =
  struct
    open Verifier (CT)

    let ( -<- ) = ( -<- )
  end
end
