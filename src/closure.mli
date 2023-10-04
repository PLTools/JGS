open OCanren
open JGS

module type MAKE = sig
  val direct_subtyping :
    ((int ilogic Jtype.injected ->
     int ilogic Jtype.injected ->
     bool ilogic ->
     goal) ->
    int ilogic Jtype.injected ->
    int ilogic Jtype.injected ->
    bool ilogic ->
    goal) ->
    ?query_constr:goal ->
    int ilogic Jtype.injected ->
    int ilogic Jtype.injected ->
    goal

  val closure :
    ((int ilogic Jtype.injected ->
     int ilogic Jtype.injected ->
     bool ilogic ->
     goal) ->
    int ilogic Jtype.injected ->
    int ilogic Jtype.injected ->
    bool ilogic ->
    goal) ->
    ?query_constr:goal ->
    int ilogic Jtype.injected ->
    int ilogic Jtype.injected ->
    goal
end

module Make (_ : CLASSTABLE) : MAKE
