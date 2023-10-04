open OCanren
open JGS

val need_dynamic_closure : bool ref

module type SCT = Mutable_type_table.SAMPLE_CLASSTABLE

type closure_type = Subtyping | Supertyping

type closure = {
  is_correct_type :
    closure_type:closure_type ->
    ?constr:goal ->
    int ilogic Jtype.injected ->
    goal;
  direct_subtyping :
    closure_type:closure_type ->
    ?constr:goal ->
    int ilogic Jtype.injected ->
    int ilogic Jtype.injected ->
    goal;
  closure :
    closure_type:closure_type ->
    ?constr:goal ->
    int ilogic Jtype.injected ->
    int ilogic Jtype.injected ->
    goal;
}

val make_closure :
  (module SCT) ->
  ((int ilogic Jtype.injected ->
   int ilogic Jtype.injected ->
   Std.Bool.groundi ->
   goal) ->
  int ilogic Jtype.injected ->
  int ilogic Jtype.injected ->
  Std.Bool.groundi ->
  goal) ->
  closure
