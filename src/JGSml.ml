(* The code is written with the compliance of the
   "Java Language Specifcation Java SE Edition"
   https://docs.oracle.com/javase/specs/jls/se19/jls19.pdf

   Further references are given according to this document.
*)

open List
open Peano

(* To add in noCanren's std *)   
let rec find f = function
| []       -> None
| hd :: tl -> if f hd then Some hd else find f tl
            
(* A type to identify various objects *)
type id = int 

(* Types; only reference types since
   primitive types seem not to be involved;
   section 4.3, page 63.
*)
type jtype =
| Array of jtype           (* array type                                     *)
| Class of id * targ array (* class/interface type                           *)
| Var   of int             (* type variable; argument is the position of the
                              variable in generic class declaration
                           *)
| Object                   (* specific cases with explicit subtyping rules   *)
| Cloneable
| Serializable                    

(* Type arguments; section 4.5.1, page 71 *)
and  polarity = Extends | Super

and  targ =
| Type     of jtype
| Wildcard of polarity * jtype

(* Class/interface declaration; only type-specific informanion is retained.
   Section 8.1, page 237, section 8.1.2, page 241;
   Section 9.1, page 342, section 9.1.2, page 344.
*)
type decl = {
  is_interface : bool;            (* is interface                                                   *)
  parms        : jtype list list; (* type parameters:
                                     1. type parameters are represented implicitly by their indices;
                                     2. the number of parameters is the length of the bounds' array.
                                  *)
  supers       : jtype list;      (* superclass/superinterfaces                                     *)
}

(* Substitution of type parameters *)
let rec substitute : targ array -> jtype -> jtype = fun subst -> function
| Array  typ       -> Array (substitute subst typ) 
| Class (id, args) -> Class (id, Array.map (function
                                            | Type         typ  -> Type (substitute subst typ)
                                            | Wildcard (p, typ) -> Wildcard (p, substitute subst typ)
                                           ) args) (*
| Var    n         -> subst.(n) *)
| other            -> other 
  
(* Accessor functions; expected to be imported from class table *)
let decl_by_id : id -> decl = fun id -> invalid_arg "*** decl_by_id ***"
                                       
(* Direct subtyping relation; only for non-raw types;
   section 4.10.2, page 82.
*)     
let rec (<) : jtype -> jtype -> bool = fun ta tb ->
  match ta with
  | Class (id_a, targs_a) ->     
     (match tb with
      | Class (id_b, targs_b) ->
          let supers = (decl_by_id id_a).supers in
          (match find (function Class (id, _) -> id = id_b | _ -> false) supers with
           | Some (Class (_, targs_b')) ->
              false
           | None     -> false
          )
      | _ -> false
     )     
    
  | Array ta ->
     (match ta with
      | Object -> (match tb with
                   | Object
                   | Cloneable
                   | Serializable -> true
                   | _            -> (match tb with Array tb -> ta < tb | _ -> false)
                  )
      | _      -> (match tb with Array tb -> ta < tb | _ -> false)
     )
  | Object                    -> false
  | Var        _              -> invalid_arg "*** should not happen ***"
