let need_simpified = false

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
  val get_superclass : int -> int -> jtype option
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
        match CT.get_superclass id_a id_b with
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

  [@@@ocaml.warning "-27"]

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

  [@@@ocaml.warning "+27"]

  let rec substitute_typ subst q0 q30 =
    conde
      [
        fresh (typ q4) (q0 === !!(Array typ)) (q30 === !!(Array q4))
          (substitute_typ subst typ q4);
        fresh (id args q8)
          (q0 === !!(Class (id, args)))
          (q30 === !!(Class (id, q8)))
          (Std.List.mapo (substitute_arg subst) args q8);
        fresh (id args q13)
          (q0 === !!(Interface (id, args)))
          (q30 === !!(Interface (id, q13)))
          (Std.List.mapo (substitute_arg subst) args q13);
        fresh (typs q17)
          (q0 === !!(Intersect typs))
          (q30 === !!(Intersect q17))
          (Std.List.mapo (substitute_typ subst) typs q17);
        fresh () (q0 === !!Null) (q30 === !!Null);
      ]

  and substitute_arg subst q34 q63 =
    conde
      [
        fresh index
          (q34 === !!(Type (var __ index __ __)))
          (List.HO.nth subst (( === ) index) q63);
        fresh (typ q48) (q34 === !!(Type typ)) (q63 === !!(Type q48))
          (q34 =/= !!(Type (var __ __ __ __)))
          (substitute_typ subst typ q48);
        fresh () (q34 === !!(Wildcard !!None)) (q63 === !!(Wildcard !!None));
        fresh (p typ q59)
          (q34 === !!(Wildcard !!(Some (Std.pair p typ))))
          (q63 === !!(Wildcard !!(Some (Std.pair p q59))))
          (substitute_typ subst typ q59);
      ]

  module Verifier (CT : sig
    module HO : sig
      val decl_by_id :
        (int ilogic -> OCanren.goal) -> decl_injected -> OCanren.goal

      val get_superclass :
        (int ilogic -> OCanren.goal) ->
        (int ilogic -> OCanren.goal) ->
        jtype_injected option_injected ->
        OCanren.goal

      val object_t : jtype_injected -> OCanren.goal
      val cloneable_t : jtype_injected -> OCanren.goal
      val serializable_t : jtype_injected -> OCanren.goal

      val new_var :
        (unit OCanren.ilogic -> OCanren.goal) -> int ilogic -> OCanren.goal
    end
  end) =
  struct
    let capture_conversion :
        (jtype_injected -> jtype_injected -> bool ilogic -> goal) ->
        (int ilogic -> goal) ->
        jtype_injected targ_injected Std.List.injected ->
        jtype_injected targ_injected Std.List.injected option ilogic ->
        goal =
     fun ( <-< ) id targs q205 ->
      if need_simpified then q205 === !!(Some targs)
      else
        fresh ()
          (let params q98 =
             fresh (q99 q100 q101 q102) (CT.HO.decl_by_id id q99)
               (conde
                  [
                    q99 === !!(C (ctor_cdecl q98 q100 q101));
                    q99 === !!(I (ctor_idecl q98 q102));
                  ])
           in
           let raw =
             List.FO.mapi
               (fun i q107 q133 ->
                 conde
                   [
                     fresh t (q107 === !!(Type t)) (q133 === !!(CC_type t));
                     fresh (q114 q117)
                       (q107 === !!(Wildcard !!None))
                       (q133
                       === !!(CC_var
                                (q114, i, !!(CC_subst q117), !!(Some !!Null))))
                       (CT.HO.new_var (( === ) !!()) q114)
                       (List.HO.nth params (( === ) i) q117);
                     fresh (t q119 q122)
                       (q107 === !!(Wildcard !!(Some (Std.pair !!Super t))))
                       (q133
                       === !!(CC_var (q119, i, !!(CC_subst q122), !!(Some t))))
                       (CT.HO.new_var (( === ) !!()) q119)
                       (List.HO.nth params (( === ) i) q122);
                     fresh (t q126 q130)
                       (q107 === !!(Wildcard !!(Some (Std.pair !!Extends t))))
                       (q133
                       === !!(CC_var
                                ( q126,
                                  i,
                                  !!(CC_inter (t, q130)),
                                  !!(Some !!Null) )))
                       (CT.HO.new_var (( === ) !!()) q126)
                       (List.HO.nth params (( === ) i) q130);
                   ])
               targs
           in
           let subst =
             List.HO.map
               (fun q136 q137 ->
                 fresh q138 (q136 q138)
                   (conde
                      [
                        fresh t (q138 === !!(CC_type t)) (q137 === !!(Type t));
                        fresh (id i q142 q143)
                          (q138 === !!(CC_var (id, i, q142, q143)))
                          (q137 === !!(Type (var id i !!Null !!None)));
                      ]))
               raw
           in
           let targs =
             List.HO.map
               (fun q151 q152 ->
                 fresh q153 (q151 q153)
                   (conde
                      [
                        fresh vvv (q153 === !!(CC_type vvv))
                          (vvv === var __ __ __ __)
                          (q152 === !!(Type vvv));
                        fresh (t q154) (q153 === !!(CC_type t))
                          (q152 === !!(Type q154))
                          (substitute_typ subst t q154);
                        fresh (id i p lwb q159)
                          (q153 === !!(CC_var (id, i, !!(CC_subst p), lwb)))
                          (q152 === !!(Type (var id i q159 lwb)))
                          (substitute_typ subst p q159);
                        fresh (id i t p lwb q168 q170)
                          (q153 === !!(CC_var (id, i, !!(CC_inter (t, p)), lwb)))
                          (q152 === !!(Type (var id i q168 lwb)))
                          (substitute_typ subst p q170)
                          (conde
                             [
                               fresh ts
                                 (q170 === !!(Intersect ts))
                                 (q168 === !!(Intersect (Std.( % ) t ts)));
                               fresh ()
                                 (q168
                                 === !!(Intersect (Std.list Fun.id [ t; q170 ]))
                                 )
                                 (q170 =/= !!(Intersect __));
                             ]);
                      ]))
               raw
           in
           fresh q186
             (List.HO.for_all
                (fun q191 q192 ->
                  fresh q193 (q191 q193)
                    (conde
                       [
                         fresh (q194 q195 upb lwb)
                           (q193 === !!(Type (var q194 q195 upb !!(Some lwb))))
                           (( <-< ) lwb upb q192);
                         fresh () (q192 === !!true)
                           (q193 =/= !!(Type (var __ __ __ !!(Some __))));
                       ]))
                targs q186)
             (conde
                [
                  fresh q189 (q186 === !!true) (q205 === !!(Some q189))
                    (targs q189);
                  fresh () (q186 === !!false) (q205 === !!None);
                ]))

    let rec ( -<- ) :
        (jtype_injected -> jtype_injected -> bool ilogic -> Option.HO.goal) ->
        jtype_injected ->
        jtype_injected ->
        bool ilogic ->
        goal =
     fun ( <-< ) type_a type_b res ->
      let ( <=< ) type_a type_b res =
        if need_simpified then
          conde
            [
              fresh () (type_a === type_b) (res === !!true);
              fresh () (type_a =/= type_b) (res === !!false);
            ]
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
                (fresh q83
                   (type_a === !!(Wildcard !!(Some (Std.pair !!Super t))))
                   (type_b === !!(Wildcard !!(Some (Std.pair !!Extends o))))
                   (CT.HO.object_t q83)
                   (conde
                      [
                        fresh () (o === q83) (res === !!true);
                        fresh () (res === !!false) (o =/= q83);
                      ]));
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
      in
      let class_int_sub id_a targs_a id_b targs_b res =
        conde
          [
            fresh () (id_a === id_b)
              (List.HO.fold_left2
                 (fun f ta tb q228 ->
                   fresh (q226 ta_val tb_val) (ta ta_val) (tb tb_val) (f q226)
                     (conde
                        [
                          fresh () (q226 === !!false) (q228 === !!false);
                          fresh () (q226 === !!true)
                            (( <=< ) ta_val tb_val q228);
                        ]))
                 (( === ) !!true) targs_a targs_b res);
            fresh q211 (id_a =/= id_b)
              (CT.HO.get_superclass (( === ) id_a) (( === ) id_b) q211)
              (conde
                 [
                   fresh (targs_b' q217 q218)
                     (conde
                        [
                          q211 === !!(Some !!(Class (__, targs_b')));
                          q211 === !!(Some !!(Interface (__, targs_b')));
                        ])
                     (targs_b q217)
                     (Std.List.mapo (substitute_arg targs_a) targs_b' q218)
                     (conde
                        [
                          fresh () (q217 === q218) (res === !!true);
                          fresh () (res === !!false) (q217 =/= q218);
                        ]);
                   fresh () (q211 === !!None) (res === !!false);
                 ]);
          ]
      in
      let ( -<- ) = ( -<- ) ( <-< ) in
      fresh ()
        (conde
           [
             fresh (id_a targs_a q243)
               (type_a === !!(Class (id_a, targs_a)))
               (capture_conversion ( <-< ) (( === ) id_a) targs_a q243)
               (conde
                  [
                    fresh () (q243 === !!None) (res === !!false);
                    fresh targs_a
                      (q243 === !!(Some targs_a))
                      (conde
                         [
                           fresh (id_b targs_b)
                             (conde
                                [
                                  type_b === !!(Interface (id_b, targs_b));
                                  type_b === !!(Class (id_b, targs_b));
                                ])
                             (class_int_sub id_a (( === ) targs_a) id_b
                                (( === ) targs_b) res);
                           fresh typ
                             (fresh ()
                                (type_b === var __ __ __ !!(Some typ))
                                (conde
                                   [
                                     fresh () (typ === type_a) (res === !!true);
                                     fresh () (res === !!false) (typ =/= type_a);
                                   ]));
                           fresh () (res === !!false)
                             (type_b =/= var __ __ __ !!(Some __))
                             (type_b =/= !!(Interface (__, __)))
                             (type_b =/= !!(Class (__, __)));
                         ]);
                  ]);
             fresh (id_a targs_a q271)
               (type_a === !!(Interface (id_a, targs_a)))
               (capture_conversion ( <-< ) (( === ) id_a) targs_a q271)
               (conde
                  [
                    fresh () (q271 === !!None) (res === !!false);
                    fresh targs_a
                      (q271 === !!(Some targs_a))
                      (conde
                         [
                           fresh (id_b targs_b)
                             (conde
                                [
                                  type_b === !!(Class (id_b, targs_b));
                                  type_b === !!(Interface (id_b, targs_b));
                                ])
                             (class_int_sub id_a (( === ) targs_a) id_b
                                (( === ) targs_b) res);
                           fresh (q289 q290 q291 typ)
                             (type_b === var q289 q290 q291 !!(Some typ))
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
             fresh (ta q353) (type_a === !!(Array ta)) (CT.HO.object_t q353)
               (conde
                  [
                    fresh (q318 q330) (ta === q353) (CT.HO.object_t q330)
                      (conde
                         [
                           fresh () (type_b === q330) (q318 === !!true);
                           fresh q335 (type_b =/= q330) (CT.HO.cloneable_t q335)
                             (conde
                                [
                                  fresh () (type_b === q335) (q318 === !!true);
                                  fresh q340 (type_b =/= q335)
                                    (CT.HO.serializable_t q340)
                                    (conde
                                       [
                                         fresh () (type_b === q340)
                                           (q318 === !!true);
                                         fresh () (q318 === !!false)
                                           (type_b =/= q340);
                                       ]);
                                ]);
                         ])
                      (conde
                         [
                           fresh () (q318 === !!true) (res === !!true);
                           fresh () (q318 === !!false)
                             (conde
                                [
                                  fresh tb (type_b === !!(Array tb))
                                    (( -<- ) ta tb res);
                                  fresh q323 (type_b === q323) (res === !!false)
                                    (type_b =/= !!(Array __));
                                ]);
                         ]);
                    fresh () (ta =/= q353)
                      (conde
                         [
                           fresh tb (type_b === !!(Array tb))
                             (( -<- ) ta tb res);
                           fresh q315 (type_b === q315) (res === !!false)
                             (type_b =/= !!(Array __));
                         ]);
                  ]);
             fresh ts (type_a === !!(Intersect ts)) (List.FO.mem type_b ts res);
             fresh typ
               (type_a === var __ __ typ __)
               (conde
                  [
                    fresh () (typ === type_b) (res === !!true);
                    fresh () (res === !!false) (typ =/= type_b);
                  ]);
             fresh () (type_a === !!Null)
               (conde
                  [
                    fresh () (type_b === !!Null) (res === !!false);
                    fresh () (res === !!true) (type_b =/= !!Null);
                  ]);
           ])
  end
end

module FO = struct
  open Option.HO
  open Peano.HO
  open List.HO
  open Option.HO
  open HO

  let substitute_typ q33 q32 q31 = substitute_typ (( === ) q33) q32 q31
  let substitute_arg q66 q65 q64 = substitute_arg (( === ) q66) q65 q64

  module Verifier (CT : sig
    module HO : sig
      val decl_by_id :
        (int ilogic -> OCanren.goal) -> decl_injected -> OCanren.goal

      val get_superclass :
        (int ilogic -> OCanren.goal) ->
        (int ilogic -> OCanren.goal) ->
        jtype_injected option_injected ->
        OCanren.goal

      val object_t : jtype_injected -> OCanren.goal
      val cloneable_t : jtype_injected -> OCanren.goal
      val serializable_t : jtype_injected -> OCanren.goal

      val new_var :
        (unit OCanren.ilogic -> OCanren.goal) -> int ilogic -> OCanren.goal
    end
  end) =
  struct
    open Verifier (CT)

    let ( -<- ) = ( -<- )
  end
end
