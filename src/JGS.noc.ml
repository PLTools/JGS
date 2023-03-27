open List
open Peano
  
type id = A | B | C (* should be generated from a class table *)

(* Class or interface declaration *)                
type cidecl = {
  id     : id;          (* identifier *)
  parms  : bound array; (* type parameters:
                             1. type parameters are represented implicitly by their indices;
                             2. the number of parameters is the length of the bounds' array.
                         *)
  super_c: jtype;       (* superclass      *)
  super_i: jtype array  (* superinterfaces *)
  }
(* Java types; only reference types since
   primitive types seem to be not involved
*)            
and jtype =
| Array     of jtype
| Class     of id * targ array  (* class type     *)
| Interface of id * targ array  (* interface type *)
| Var       of int              (* type variable  *)
and bound    = jtype array
and targ     = Type of jtype | Wildcard of polarity * jtype
and polarity = Extends | Super
