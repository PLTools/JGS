open List
open Peano

type id = A | B | C (* should be generated from a class table *)

type polarity = Extends | Super

(* Java types; only reference types since
   primitive types seem to be not involved
*)
type 'targ jtype =
| Array     of 'targ jtype
| Class     of id * 'targ list  (* class type     *)
| Interface of id * 'targ list  (* interface type *)
| Var       of int              (* type variable  *)

type targ     = Type of targ jtype | Wildcard of polarity * targ jtype

(* Class or interface declaration *)
type cidecl = {
  id     : id;          (* identifier *)
  parms  : targ jtype list list; (* type parameters:
                             1. type parameters are represented implicitly by their indices;
                             2. the number of parameters is the length of the bounds' array.
                         *)
  super_c: targ jtype;       (* superclass      *)
  super_i: targ jtype list  (* superinterfaces *)
  }
