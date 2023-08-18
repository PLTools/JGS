open OCanren
open JGS.HO

val need_dynamic_closure : bool ref

module type SCT = MutableTypeTable.SAMPLE_CLASSTABLE

type closure_type = Subtyping | Supertyping

type closure = {
  is_correct_type : closure_type:closure_type -> jtype_injected -> goal;
  direct_subtyping :
    closure_type:closure_type -> jtype_injected -> jtype_injected -> goal;
  closure :
    closure_type:closure_type -> jtype_injected -> jtype_injected -> goal;
}

val make_closure :
  (module SCT) ->
  ((jtype_injected -> jtype_injected -> Std.Bool.groundi -> goal) ->
  jtype_injected ->
  jtype_injected ->
  Std.Bool.groundi ->
  goal) ->
  closure
