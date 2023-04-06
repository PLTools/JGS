(* The code is written with the compliance of the
   "Java Language Specifcation Java SE Edition"
   https://docs.oracle.com/javase/specs/jls/se19/jls19.pdf

   Further references are given according to this document.
*)

open List
open Peano
   
(* A type to identify various objects *)
type id = A | B | C (* int *)

(* Types; only reference types since
   primitive types seem to be not involved;
   section 4.3, page 63.
*)
type 'targ jtype =
| Array        of 'targ jtype     (* array type                                   *)
| Class        of id * 'targ list (* class type                                   *)
| Interface    of id * 'targ list (* interface type                               *)
| Var          of id              (* type variable                                *)
| Object                          (* specific cases with explicit subtyping rules *)
| Cloneable
| Serializable                    

(* Type arguments; section 4.5.1, page 71 *)
type polarity = Extends | Super

type targ =
| Type     of targ jtype
| Wildcard of polarity * targ jtype

(* Class declaration; only type-specific informanion is retained.
   Section 8.1, page 237, section 8.1.2, page 241
*)
type cdecl = {
  parms      : targ jtype list list; (* type parameters:
                                        1. type parameters are represented implicitly by their indices;
                                        2. the number of parameters is the length of the bounds' array.
                                     *)
  superclass : targ jtype;           (* superclass      *)
  superints  : targ jtype list       (* superinterfaces *)
}

(* Interface declaration; only type-specific informanion is retained.
   Section 9.1, page 342, section 9.1.2, page 344
*)
type idecl = {
  parms     : targ jtype list list; (* type parameters:
                                       1. type parameters are represented implicitly by their indices;
                                       2. the number of parameters is the length of the bounds' array.
                                    *)
  superints : targ jtype list       (* superinterfaces *)
}

(* Type declaration: class or interface *)
type tdecl =
| C of id * cdecl
| I of id * idecl               

(* Accessor functions; expected to be imported from
   class table
*)
let decl_by_id : id -> tdecl = fun id -> invalid_arg "*** decl_by_id ***"
                                       
(* Direct subtyping relation; only for non-raw types;
   section 4.10.2, page 82.
*)
     
let rec (<) : targ jtype -> targ jtype -> bool = fun ta tb ->
  match ta with
  | Class     (id_a, targs_a) -> invalid_arg "*** should not happen ***"
  | Array      ta             ->
     (match ta with
      | Object -> (match tb with
                   | Object
                   | Cloneable
                   | Serializable -> true
                   | _            -> (match tb with Array tb -> ta < tb | _ -> false)
                  )
      | _      -> (match tb with Array tb -> ta < tb | _ -> false)
     )
  | Interface (id_a, targs_a) -> invalid_arg "*** should not happen ***"
  | Object                    -> false
  | Var        _              -> invalid_arg "*** should not happen ***"
