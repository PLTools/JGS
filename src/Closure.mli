open OCanren
open JGS.HO

module type SCT = MutableTypeTable.SAMPLE_CLASSTABLE

type closure = {
  is_correct_type : jtype_injected -> goal;
  direct_subtyping : jtype_injected -> jtype_injected -> goal;
  closure : jtype_injected -> jtype_injected -> goal;
}

val make_closure :
  (module SCT) ->
  ((jtype_injected -> jtype_injected -> Std.Bool.groundi -> goal) ->
  jtype_injected ->
  jtype_injected ->
  Std.Bool.groundi ->
  goal) ->
  closure
