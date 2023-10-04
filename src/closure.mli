open OCanren
open JGS

module type SCT = Mutable_type_table.SAMPLE_CLASSTABLE

type closure = {
  is_correct_type : ?constr:goal -> int ilogic Jtype.injected -> goal;
  direct_subtyping :
    ?constr:goal ->
    int ilogic Jtype.injected ->
    int ilogic Jtype.injected ->
    goal;
  closure :
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
