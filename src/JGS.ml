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

module Verifier (CT : sig

                        val decl_by_id     : id -> decl
                        val bound_by_id    : id -> jtype list

                        val object_t       : jtype
                        val cloneable_t    : jtype
                        val serializable_t : jtype

                        val new_var        : unit -> id
                          
                      end) =
  struct

    (* Subtype placeholder *)                                      
    let (<-<) _ _ = true
             
    (* "contains" relation, section 4.5.1, page 72 *)
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
       
    (* Direct subtyping relation; only for non-raw types;
       section 4.10.2, page 82.
     *)     
    let rec (-<-) (ta : jtype) (tb : jtype) =
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
        Array.map (function
                   | `Type t                          -> Type (substitute_typ subst t)
                   | `Var (id, i, `Subst p, lwb)      -> Type (Var {id=id; index=i; upb=substitute_typ subst p; lwb=lwb})
                   | `Var (id, i, `Inter (t, p), lwb) ->
                        Type (Var {id=id; index=i; upb=(match substitute_typ subst p with
                                                        | Intersect ts -> Intersect (t :: ts)
                                                        | typ          -> Intersect [t; typ]); lwb=lwb})
                  ) raw
      in
      let class_int_sub id_a targs_a id_b targs_b supers =
        if id_a = id_b
        then fold_left2 (fun f ta tb -> f && ta <=< tb) true targs_a targs_b
        else match List.find_opt (function Class (id, _) -> id = id_b | _ -> false) supers with
             | Some (Class (_, targs_b')) ->
                targs_b =  Array.map (fun t -> substitute_arg targs_a t) targs_b'               
             | None -> false          
      in
      match ta with
      | Class (id_a, targs_a) ->
         let targs_a = capture_conversion id_a targs_a in
         (match tb with
          | Interface (id_b, targs_b)
          | Class     (id_b, targs_b) ->
             let targs_b = capture_conversion id_b targs_b in   
             class_int_sub id_a targs_a id_b targs_b (let C decl = CT.decl_by_id id_a in
                                                      decl.super :: decl.supers)

          | Var {lwb=Some typ} -> typ = ta
          | _                  -> false
         )
         
      | Interface (id_a, targs_a) ->
         let targs_a = capture_conversion id_a targs_a in
         (match tb with
          | Class     (id_b, targs_b) 
          | Interface (id_b, targs_b) ->
             let targs_b = capture_conversion id_b targs_b in   
              let supers =
                let I decl = CT.decl_by_id id_a in 
                decl.supers
              in
              (match supers with
               | [] -> tb = CT.object_t
               | _  ->
                  class_int_sub id_a targs_a id_b targs_b supers
              )
          | Var {lwb=Some typ} -> typ = ta
          | _                  -> false
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
