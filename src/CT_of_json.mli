val failwiths : ('a, Format.formatter, unit, 'b) format4 -> 'a
val verbose_errors : bool ref
val lower_bounds_first : bool ref

type deplicates_tactic = No_remove | Structural | Debug_var

val need_remove_dups : deplicates_tactic ref

type polarity = JGS.polarity = Extends | Super

val yojson_of_polarity : polarity -> Yojson.Safe.t
val polarity_of_yojson : Yojson.Safe.t -> polarity

type class_id = string

val yojson_of_class_id : class_id -> Yojson.Safe.t
val class_id_of_yojson : Yojson.Safe.t -> class_id

type jtype =
  | Array of jtype
  | Class of class_id * jtype list
  | Interface of class_id * jtype list
  | Var of { id : class_id; index : int; upb : jtype; lwb : jtype option }
  | Null
  | Wildcard of (polarity * jtype) option
  | Intersect of jtype list
  | Primitive of class_id

val yojson_of_jtype : jtype -> Yojson.Safe.t
val jtype_of_yojson : Yojson.Safe.t -> jtype
val jtype_of_yojson : Yojson.Safe.t -> jtype
val var : ?lwb:jtype -> class_id -> jtype -> jtype

type param = { pname : class_id; p_upper : jtype list }

val yojson_of_param : param -> Yojson.Safe.t
val param_of_yojson : Yojson.Safe.t -> param
val param_of_yojson : Yojson.Safe.t -> param
val make_param : ?up:jtype list -> class_id -> param

type cdecl = {
  cname : class_id;
  params : param list;
  super : jtype option;
  supers : jtype list;
}

val yojson_of_cdecl : cdecl -> Yojson.Safe.t
val cdecl_of_yojson : Yojson.Safe.t -> cdecl
val cdecl_of_yojson : Yojson.Safe.t -> cdecl

type idecl = { iname : class_id; iparams : param list; isupers : jtype list }

val yojson_of_idecl : idecl -> Yojson.Safe.t
val idecl_of_yojson : Yojson.Safe.t -> idecl

type decl = C of cdecl | I of idecl

val yojson_of_decl : decl -> Yojson.Safe.t
val decl_of_yojson : Yojson.Safe.t -> decl

type table = decl list

val yojson_of_table : table -> Yojson.Safe.t
val table_of_yojson : Yojson.Safe.t -> table
val make_c : class_id -> params:param list -> ?sup:jtype -> jtype list -> decl
val make_i : class_id -> params:param list -> jtype list -> decl

type query = {
  table : table;
  upper_bounds : jtype list;
  lower_bounds : jtype list;
  neg_upper_bounds : jtype list;
  neg_lower_bounds : jtype list;
}

val yojson_of_query : query -> Yojson.Safe.t
val query_of_yojson : Yojson.Safe.t -> query
val make_sample_ct : unit -> (module Mutable_type_table.SAMPLE_CLASSTABLE)
val set_verbose : unit -> unit

val make_classtable :
  decl list ->
  (module Mutable_type_table.SAMPLE_CLASSTABLE)
  * (string -> int)
  * (int -> string)

exception Id_not_found of int
exception Name_not_found of class_id

type result_query =
  is_subtype:
    (closure_type:Closure.closure_type ->
    ?constr:OCanren.goal ->
    JGS.HO.jtype_injected ->
    JGS.HO.jtype_injected ->
    OCanren.goal) ->
  (JGS.HO.jtype_injected -> JGS.HO.jtype_injected) ->
  JGS.HO.jtype_injected ->
  OCanren.goal

val make_query :
  ?hack_goal:bool ->
  Yojson.Safe.t ->
  (module Mutable_type_table.SAMPLE_CLASSTABLE)
  * result_query
  * (int -> string)
  * string
