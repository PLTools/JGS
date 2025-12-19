type t

val empty : t
val cardinal : t -> int
val alpha_converted_answer_set : t ref
val add_alpha_converted : JGS.HO.jtype_logic -> t -> t
val mem_alpha_converted : JGS.HO.jtype_logic -> t -> bool
val not_alpha_covered : JGS.HO.jtype_logic -> t -> bool
