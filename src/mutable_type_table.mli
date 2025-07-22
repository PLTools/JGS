open JGS

module type SAMPLE_CLASSTABLE = sig
  include CLASSTABLE

  val make_class :
    ?name:string ->
    int Jtype.ground list ->
    int Jtype.ground ->
    int Jtype.ground list ->
    int

  val make_tvar : ?name:string -> int -> int Jtype.ground -> int Jtype.ground

  val make_interface :
    ?name:string -> int Jtype.ground list -> int Jtype.ground list -> int

  val make_class_fix :
    ?name:string ->
    params:(int -> int Jtype.ground list) ->
    (int -> int Jtype.ground) ->
    (int -> int Jtype.ground list) ->
    int

  val make_interface_fix :
    ?name:string ->
    (int -> int Jtype.ground list) ->
    (int -> int Jtype.ground list) ->
    int
  (** [make_interface_fix make_params make_superinterfaces] creates a new interface in open recursion style *)

  val pp_targ :
    Format.formatter -> int OCanren.logic Jtype.logic Targ.logic -> unit

  val pp_jtyp : Format.formatter -> int OCanren.logic Jtype.logic -> unit

  module Ground : sig
    val decl_by_id : int -> int Decl.ground
    val object_t : int Jtype.ground
    val cloneable_t : int Jtype.ground
    val serializable_t : int Jtype.ground
    val array_t : int Jtype.ground -> int Jtype.ground
    val primitive_t : string -> int Jtype.ground
    val new_var : unit -> int
  end
end

module SampleCT : functor () -> SAMPLE_CLASSTABLE
