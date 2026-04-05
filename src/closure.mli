open OCanren
open JGS.HO

val need_dynamic_closure : bool ref

type closure_type = Subtyping | Supertyping

type closure = {
  is_correct_type :
    closure_type:closure_type -> ?constr:goal -> jtype_injected -> goal;
  direct_subtyping :
    closure_type:closure_type ->
    ?constr:goal ->
    jtype_injected ->
    jtype_injected ->
    goal;
  closure :
    closure_type:closure_type ->
    ?constr:goal ->
    jtype_injected ->
    jtype_injected ->
    goal;
}

val make_closure :
  (module Mutable_type_table.SAMPLE_CLASSTABLE) ->
  ((jtype_injected -> jtype_injected -> bool ilogic -> goal) ->
  jtype_injected ->
  jtype_injected ->
  bool ilogic ->
  goal) ->
  closure
