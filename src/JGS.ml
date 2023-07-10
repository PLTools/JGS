(* The code is written with the compliance of the
   "Java Language Specifcation Java SE Edition"
   https://docs.oracle.com/javase/specs/jls/se19/jls19.pdf

   Further references are given according to this document.
*)

(* A fold for two arrays *)
let fold_left2 f c a b =
  fst (Array.fold_left (fun (c, i) a -> f c a b.(i), i+1) (c, 0) a)

(* A type to identify various objects *)
type id = int

(* Types; only reference types since primitive types seem not to be involved;
   section 4.3, page 63.
*)
type jtype =
| Array     of jtype           (* array type                     *)
| Class     of id * targ array (* class type                     *)
| Interface of id * targ array (* interface type                 *)
| Var       of {               (* type variable:                 *)
    id    : id;                (*   1. identity                  *)
    index : int;               (*   2. index in declaration list *)
    upb   : jtype;             (*   3. upper bound               *)
    lwb   : jtype option       (*   4. lower bound               *)
  }
| Null                         (* null type                      *)
| Intersect of jtype list      (* intersection type              *)

(* Type arguments; section 4.5.1, page 71 *)
and  polarity = Extends | Super

and  targ =
| Type     of jtype
| Wildcard of (polarity * jtype) option

(* Type parameters:
   1. represented implicitly by their indices;
   2. the number of parameters is the length of the bounds' array.
*)
type  params = jtype array

(* Class declaration; only type-specific informanion is retained.
   Section 8.1, page 237, section 8.1.2, page 241.
*)
type cdecl = {
  params : params;    (* type parameters *)
  super  : jtype;     (* supeclass       *)
  supers : jtype list (* superinterfaces *)
}

(* Interface declaration; only type-specific informanion is retained.
   Section 9.1, page 342, section 9.1.2, page 344.
*)
type idecl = {
  params : params;    (* type parameters *)
  supers : jtype list (* superinterfaces *)
}

(* Type declaration: a class or an interface *)
type decl =
| C of cdecl
| I of idecl

(* Substitution of type parameters *)
let rec substitute_typ (subst : targ array) = function
| Array      typ       -> Array     (substitute_typ subst typ)
| Class     (id, args) -> Class     (id, Array.map (substitute_arg subst) args)
| Interface (id, args) -> Interface (id, Array.map (substitute_arg subst) args)
| Intersect typs       -> Intersect (List.map  (substitute_typ subst) typs)
| Var        _         -> invalid_arg "*** should not happen ***"
| Null                 -> Null
and substitute_arg (subst : targ array) = function
| Type (Var {index=n})     -> subst.(n)
| Type typ                 -> Type (substitute_typ subst typ)
| Wildcard None            -> Wildcard None
| Wildcard (Some (p, typ)) -> Wildcard (Some (p, substitute_typ subst typ))

(* Subtyping verifier functor parameterized by a class table CT*)
module Verifier (CT : sig

                        val decl_by_id     : id -> decl (* Gets a class/interface declaration by is *)

                        val object_t       : jtype      (* Synonyms for some specific classes       *)
                        val cloneable_t    : jtype      (* mentioned in the JLS                     *)
                        val serializable_t : jtype      (*                                          *)

                        val new_var        : unit -> id (* Gets a new var's id                      *)

                      end) =
  struct


    (* Direct subtyping relation; only for non-raw types, section 4.10.2, page 82.
       The first argument is suptyping relation which has to be a transitive-reflexive closure
       of the direct syptyping.
    *)
    let rec (-<-) ((<-<) : jtype -> jtype -> bool) (ta : jtype) (tb : jtype) =
      (* "Contains" relation, section 4.5.1, page 72 *)
      let (<=<) (ta : targ) (tb : targ) =
        match ta, tb with
        | Wildcard (Some (Extends, t)), Wildcard (Some (Extends, s))      -> t <-< s
        | Wildcard (Some (Extends, t)), Wildcard None                     -> true
        | Wildcard (Some (Super  , t)), Wildcard (Some (Super  , s))      -> s <-< t
        | Wildcard (Some (Super  , t)), Wildcard None                     -> true
        | Wildcard (Some (Super  , t)), Wildcard (Some (Extends, o)) when o = CT.object_t -> true

        | Type      t1                , Type      t2
        | Type      t1                , Wildcard (Some (Extends, t2))
        | Type      t1                , Wildcard (Some (Super  , t2)) -> t1 = t2

        | _ -> false
      in
      (* Capture conversion, section 5.1.10, page 125 *)
      let capture_conversion id targs =
        let params = match CT.decl_by_id id with C {params=params} | I {params=params} -> params in
        let raw    =
          Array.mapi (fun i ->
                      function
                      | Type t                       -> `Type t
                      | Wildcard None                -> `Var (CT.new_var (), i, `Subst params.(i)     , Some Null)
                      | Wildcard (Some (Super  , t)) -> `Var (CT.new_var (), i, `Subst params.(i)     , Some t   )
                      | Wildcard (Some (Extends, t)) -> `Var (CT.new_var (), i, `Inter (t, params.(i)), Some Null)
                     ) targs
        in
        let subst =
          Array.map (function
                     | `Type t            -> Type t
                     | `Var (id, i, _, _) -> Type (Var {id=id; index=i; upb=Null; lwb=None}) (* upb and lwb are dummy here! *)
                    ) raw
        in
        let targs =
          Array.map (function
                     | `Type t                          -> Type (substitute_typ subst t)
                     | `Var (id, i, `Subst p, lwb)      -> Type (Var {id=id; index=i; upb=substitute_typ subst p; lwb=lwb})
                     | `Var (id, i, `Inter (t, p), lwb) ->
                         Type (Var {id=id; index=i; upb=(match substitute_typ subst p with
                                                         | Intersect ts -> Intersect (t :: ts)
                                                         | typ          -> Intersect [t; typ]); lwb=lwb})
                    ) raw
        in
        if Array.for_all (function
                          | Type (Var {upb=upb; lwb=Some lwb}) -> lwb <-< upb
                          | _ -> true
                         ) targs
        then Some targs
        else None
      in
      (* helper function *)
      let class_int_sub id_a targs_a id_b targs_b supers =
        if id_a = id_b
        then fold_left2 (fun f ta tb -> f && ta <=< tb) true targs_a targs_b
        else match List.find_opt (function Class (id, _) | Interface (id, _) -> id = id_b | _ -> false) supers with
             | Some (Class     (_, targs_b'))
             | Some (Interface (_, targs_b')) ->
                targs_b =  Array.map (fun t -> substitute_arg targs_a t) targs_b'
             | None -> false
      in
      let (-<-) = (-<-) (<-<) in
      match ta with
      | Class (id_a, targs_a) ->
         (match capture_conversion id_a targs_a with
          | None         -> false
          | Some targs_a ->
             (match tb with
              | Interface (id_b, targs_b)
              | Class     (id_b, targs_b) ->
                  class_int_sub id_a targs_a id_b targs_b (let C decl = CT.decl_by_id id_a in
                                                           decl.super :: decl.supers)
              | Var {lwb=Some typ} -> typ = ta
              | _                  -> false
             )
         )

      | Interface (id_a, targs_a) ->
         (match capture_conversion id_a targs_a with
          | None         -> false
          | Some targs_a ->
             (match tb with
              | Class     (id_b, targs_b)
              | Interface (id_b, targs_b) ->
                 let supers =
                   let I decl = CT.decl_by_id id_a in
                   decl.supers
                 in
                 (match supers with
                  | [] -> tb = CT.object_t
                  | _  -> class_int_sub id_a targs_a id_b targs_b supers
                 )
              | Var {lwb=Some typ} -> typ = ta
              | _                  -> false
             )
         )

      | Array ta ->
         if ta = CT.object_t
         then if tb = CT.object_t       ||
                 tb = CT.cloneable_t    ||
                 tb = CT.serializable_t
              then true
              else (match tb with Array tb -> ta -<- tb | _ -> false)
         else (match tb with Array tb -> ta -<- tb | _ -> false)

      | Intersect ts -> List.mem tb ts

      | Var {upb=typ} -> typ = tb

      | Null -> tb <> Null

  end

module SampleCT =
  struct

    let new_id =
      let n = ref 0 in
      fun () ->
        let i = !n in
        incr n;
        i

    module M = Map.Make (struct type t = id let compare = compare end)

    let make_tvar index upb = Var {id=new_id (); index=index; upb=upb; lwb=None}

    let add_class, add_interface, add_class_fix, add_interface_fix, decl_by_id =
      let make_params params =
        Array.mapi (fun i p -> make_tvar i p) params
      in
      let m = ref M.empty in
      (fun (c : cdecl) -> let id = new_id () in let d = C {c with params=make_params c.params} in m := M.add id d !m; id),
      (fun (i : idecl) -> let id = new_id () in let d = I {i with params=make_params i.params} in m := M.add id d !m; id),
      (fun (c : id -> cdecl) -> let id = new_id () in let c = c id in let d = C {c with params=make_params c.params} in m := M.add id d !m; id),
      (fun (i : id -> idecl) -> let id = new_id () in let i = i id in let d = I {i with params=make_params i.params} in m := M.add id d !m; id),
      (fun id   -> M.find id !m)

    let make_class         params super supers = add_class         {params; super; supers}
    let make_interface     params supers       = add_interface     {params; supers}
    let make_class_fix     params super supers = add_class_fix     (fun id -> {params=params id; super=super id; supers=supers id})
    let make_interface_fix params supers       = add_interface_fix (fun id -> {params=params id; supers=supers id})

    let top = Class (-1, [||])

    let object_t =
      let id = make_class [||] top [] in
      Class (id, [||])

    let cloneable_t =
      let id = make_interface [||] [] in
      Interface (id, [||])

    let serializable_t =
      let id = make_interface [||] [] in
      Interface (id, [||])

    let new_var = new_id

  end

module Verify = Verifier (SampleCT)

let rec (<-<) ta tb = ta -<- tb (* not complete! *)
and (-<-) ta tb     = Verify.(-<-) (<-<) ta tb

let _ =
  Printf.printf " 1 Object[]     < Object         (true) : %b\n" ((Array SampleCT.object_t) -<- SampleCT.object_t);
  Printf.printf " 2 Object[]     < Cloneable      (true) : %b\n" ((Array SampleCT.object_t) -<- SampleCT.cloneable_t);
  Printf.printf " 3 Object[]     < Serializable   (true) : %b\n" ((Array SampleCT.object_t) -<- SampleCT.serializable_t);

  Printf.printf " 4 Object       < Object[]       (false): %b\n" (SampleCT.object_t -<- (Array SampleCT.object_t));
  Printf.printf " 5 Cloneable    < Object[]       (false): %b\n" (SampleCT.cloneable_t -<- (Array SampleCT.object_t));
  Printf.printf " 6 Serializable < Object[]       (false): %b\n" (SampleCT.serializable_t -<- (Array SampleCT.object_t));

  Printf.printf " 7 Object[][]   < Serializable[] (true) : %b\n" (Array (Array SampleCT.object_t) -<- Array SampleCT.serializable_t);

  (* class A {...} *)
  let class_a = SampleCT.make_class [||] SampleCT.object_t [] in

  (* class B extends A {...} *)
  let class_b = SampleCT.make_class [||] (Class (class_a, [||])) [] in
  Printf.printf " 8 B            < A              (true) : %b\n" (Class (class_b, [||]) -<- Class (class_a, [||]));

  (* interface IA {...}       *)
  let intf_a = SampleCT.make_interface [||] [] in

  (* class C extends A implements IA {...} *)
  let class_c = SampleCT.make_class [||] (Class (class_a, [||])) [Interface (intf_a, [||])] in
  Printf.printf " 9 C            < A              (true) : %b\n" (Class (class_c, [||]) -<- Class (class_a, [||]));
  Printf.printf "10 C            < IA             (true) : %b\n" (Class (class_c, [||]) -<- Interface (intf_a, [||]));

  (* interface IB extends IA {...} *)
  let intf_b = SampleCT.make_interface [||] [Interface (intf_a, [||])] in
  Printf.printf "11 IB           < IA             (true) : %b\n" (Interface (intf_b, [||]) -<- Interface (intf_a, [||]));

  (* class D<X> {...} *)
  let class_d = SampleCT.make_class [|SampleCT.object_t|] SampleCT.object_t [] in

  (* class E<X, Y> {...} *)
  let class_e = SampleCT.make_class [|SampleCT.object_t; SampleCT.object_t|] SampleCT.object_t [] in

  (* class G<X> extends D <G<A>> *)
  let class_g = SampleCT.make_class_fix
                  (fun self -> [|SampleCT.object_t|])
                  (fun self -> (Class (class_d, [|Type (Class (self, [|Type (Class (class_a, [||]))|]))|])))
                  (fun self -> [])
  in

  (* class F<X, Y> extends E<D<Y>, X> {...} *)
  let class_f = SampleCT.make_class
                  [|SampleCT.object_t; SampleCT.object_t|]
                  (Class (class_e, [|Type (Class (class_d, [|Type (SampleCT.make_tvar 1 SampleCT.object_t)|]));
                                     Type (SampleCT.make_tvar 0 SampleCT.object_t)|]))
                  []
  in
  Printf.printf "12 F<A, B>      < E<D<B>, A>     (true) : %b\n" (Class (class_f, [|Type (Class (class_a, [||]));
                                                                                    Type (Class (class_b, [||]))|]) -<-
                                                                  Class (class_e, [|Type (Class (class_d, [|Type (Class (class_b, [||]))|]));
                                                                                    Type (Class (class_a, [||]))|]))
