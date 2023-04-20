(* The code is written with the compliance of the
   "Java Language Specifcation Java SE Edition"
   https://docs.oracle.com/javase/specs/jls/se19/jls19.pdf

   Further references are given according to this document.
*)

open Option
open Peano
open List

(* A type to identify various objects *)
(* NEED TO RETURN: type id = int *)
type polarity = Extends | Super

(* Types; only reference types since primitive types seem not to be involved;
   section 4.3, page 63.
*)
(* Type arguments; section 4.5.1, page 71 *)
type 'jtype targ = Type of 'jtype | Wildcard of (polarity * 'jtype) option

type jtype =
  (* array type *)
  | Array of jtype
  (* class type *)
  | Class of (* NEED TO RETURN: id *) nat * jtype targ list
  (* interface type *)
  | Interface of (* NEED TO RETURN: id *) nat * jtype targ list
  (* type variable: *)
  | Var of {
      (* 1. identity *)
      id : (* NEED TO RETURN: id *) nat;
      (* 2. index in declaration list *)
      index : nat;
      (* 3. upper bound *)
      upb : jtype;
      (* 4. lower bound *)
      lwb : jtype option;
    }
    (* null type *)
  | Null
  (* intersection type *)
  | Intersect of jtype list

(* Type parameters:
   1. represented implicitly by their indices;
   2. the number of parameters is the length of the bounds' array.
*)
(* NEED TO RETURN: type params = jtype list *)

(* Class declaration; only type-specific informanion is retained.
   Section 8.1, page 237, section 8.1.2, page 241.
*)
type cdecl = {
  (* type parameters *)
  params : (* NEED TO RETURN: params *) jtype list;
  (* supeclass *)
  super : jtype;
  (* superinterfaces *)
  supers : jtype list;
}

(* Interface declaration; only type-specific informanion is retained.
   Section 9.1, page 342, section 9.1.2, page 344.
*)
type idecl = {
  (* type parameters *)
  params : (* NEED TO RETURN: params *) jtype list;
  (* superinterfaces *)
  supers : jtype list;
}

(* Type declaration: a class or an interface *)
type decl = C of cdecl | I of idecl
type capture_conversion_subst = CC_inter of jtype * jtype | CC_subst of jtype

type capture_conversion_type =
  | CC_type of jtype
  | CC_var of
      (* NEED TO RETURN: id *) nat
      * (* NEED TO RETURN: id *) nat
      * capture_conversion_subst
      * jtype option

(* Substitution of type parameters *)
let rec substitute_typ subst = function
  | Array typ -> Array (substitute_typ subst typ)
  | Class (id, args) -> Class (id, List.map (substitute_arg subst) args)
  | Interface (id, args) -> Interface (id, List.map (substitute_arg subst) args)
  | Intersect typs -> Intersect (List.map (substitute_typ subst) typs)
  | Var _ -> failwith "*** should not happen ***"
  | Null -> Null

and substitute_arg subst = function
  | Type (Var { index }) -> List.nth subst index
  | Type typ -> Type (substitute_typ subst typ)
  | Wildcard None -> Wildcard None
  | Wildcard (Some (p, typ)) -> Wildcard (Some (p, substitute_typ subst typ))

(* Subtyping verifier functor parameterized by a class table CT*)
module Verifier (CT : sig
  (* Gets a class/interface declaration by is *)
  val decl_by_id : (* NEED TO RETURN: id *) nat -> decl

  (* Synonyms for some specific classes *)
  val object_t : jtype

  (* mentioned in the JLS *)
  val cloneable_t : jtype

  (* mentioned in the JLS *)
  val serializable_t : jtype

  (* Gets a new var's id *)
  val new_var : unit -> (* NEED TO RETURN: id *) nat
end) =
struct
  (* Direct subtyping relation; only for non-raw types, section 4.10.2, page 82.
     The first argument is suptyping relation which has to be a transitive-reflexive closure
     of the direct syptyping.
  *)
  let rec ( -<- ) (( <-< ) : jtype -> jtype -> bool) (ta : jtype) (tb : jtype) =
    (* "Contains" relation, section 4.5.1, page 72 *)
    let ( <=< ) ta tb =
      match (ta, tb) with
      | Wildcard (Some (Extends, t)), Wildcard (Some (Extends, s)) -> t <-< s
      | Wildcard (Some (Extends, t)), Wildcard None -> true
      | Wildcard (Some (Super, t)), Wildcard (Some (Super, s)) -> s <-< t
      | Wildcard (Some (Super, t)), Wildcard None -> true
      | Wildcard (Some (Super, t)), Wildcard (Some (Extends, o)) ->
          o = CT.object_t
      | Type t1, Type t2
      | Type t1, Wildcard (Some (Extends, t2))
      | Type t1, Wildcard (Some (Super, t2)) ->
          t1 = t2
      | _ -> false
    in
    (* Capture conversion, section 5.1.10, page 125 *)
    let capture_conversion id targs =
      let params =
        match CT.decl_by_id id with C { params } | I { params } -> params
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
                Type (Var { id; index = i; upb = Null; lwb = None })
                (* upb and lwb are dummy here! *))
          raw
      in
      List.map
        (function
          | CC_type t -> Type (substitute_typ subst t)
          | CC_var (id, i, CC_subst p, lwb) ->
              Type (Var { id; index = i; upb = substitute_typ subst p; lwb })
          | CC_var (id, i, CC_inter (t, p), lwb) ->
              Type
                (Var
                   {
                     id;
                     index = i;
                     upb =
                       (match substitute_typ subst p with
                       | Intersect ts -> Intersect (t :: ts)
                       | typ -> Intersect [ t; typ ]);
                     lwb;
                   }))
        raw
    in
    (* helper function *)
    let class_int_sub id_a targs_a id_b targs_b supers =
      if id_a = id_b then
        List.fold_left2 (fun f ta tb -> f && ta <=< tb) true targs_a targs_b
      else
        match
          List.find_opt
            (function
              | Class (id, _) | Interface (id, _) -> id = id_b | _ -> false)
            supers
        with
        | Some (Class (_, targs_b')) | Some (Interface (_, targs_b')) ->
            targs_b = List.map (fun t -> substitute_arg targs_a t) targs_b'
        | None -> false
    in
    let ( -<- ) = ( -<- ) ( <-< ) in
    match ta with
    | Class (id_a, targs_a) -> (
        let targs_a = capture_conversion id_a targs_a in
        match tb with
        | Interface (id_b, targs_b) | Class (id_b, targs_b) ->
            class_int_sub id_a targs_a id_b targs_b
              (let (C decl) = CT.decl_by_id id_a in
               decl.super :: decl.supers)
        | Var { lwb = Some typ } -> typ = ta
        | _ -> false)
    | Interface (id_a, targs_a) -> (
        let targs_a = capture_conversion id_a targs_a in
        match tb with
        | Class (id_b, targs_b) | Interface (id_b, targs_b) -> (
            let supers =
              let (I decl) = CT.decl_by_id id_a in
              decl.supers
            in
            match supers with
            | [] -> tb = CT.object_t
            | _ -> class_int_sub id_a targs_a id_b targs_b supers)
        | Var { lwb = Some typ } -> typ = ta
        | _ -> false)
    | Array ta -> (
        if ta = CT.object_t then
          if tb = CT.object_t || tb = CT.cloneable_t || tb = CT.serializable_t
          then true
          else match tb with Array tb -> ta -<- tb | _ -> false
        else match tb with Array tb -> ta -<- tb | _ -> false)
    | Intersect ts -> List.mem tb ts
    | Var { upb = typ } -> typ = tb
    | Null -> tb <> Null
end

(* module Verify = Verifier (SampleCT) *)

(* let rec ( <-< ) ta tb = ta -<- tb (* not complete! *)
   and ( -<- ) ta tb = Verify.( -<- ) ( <-< ) ta tb

   let _ =
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
               ] )) *)
