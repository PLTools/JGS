open OCanren
open JGS

(**************************************************************************************************)
(************************************* Pretty-printing ********************************************)
(**************************************************************************************************)

let pp_lnat : Std.Nat.logic -> string =
 fun n ->
  let rec helper n =
    match n with
    | OCanren.Var _ -> (0, Some n)
    | Value Std.Nat.O -> (0, None)
    | Value (S n') ->
        let n, v = helper n' in
        (n + 1, v)
  in
  match helper n with
  | 0, None -> "0"
  | 0, Some v -> [%show: Std.Nat.logic] () v
  | n, None -> Printf.sprintf "%d" n
  | n, Some v -> Printf.sprintf "%d + %s" n @@ [%show: Std.Nat.logic] () v

let pp_lint n = GT.show OCanren.logic Int.to_string n

let rec pp_ltarg : int OCanren.logic Jtype.logic Targ.logic -> string =
 fun arg ->
  GT.show OCanren.logic
    (GT.show Targ.t pp_ljtype
       (GT.show Std.Option.logic
       @@ GT.show Std.Pair.logic (GT.show Polarity.logic) pp_ljtype))
    arg

and pp_ljtype : int logic Jtype.logic -> string =
 fun t ->
  GT.show OCanren.logic
    (GT.show Jtype.t pp_lint
       (GT.show Std.List.logic pp_ltarg)
       pp_lnat pp_ljtype
       (GT.show Std.Option.logic pp_ljtype)
       (GT.show Std.List.logic pp_ljtype))
    t

let rec pp_jtyp_logic name_of :
    Format.formatter -> int logic Jtype.logic -> unit =
  let open Format in
  let open OCanren.Std in
  let rec helper ppf :
      ( int logic,
        int logic Jtype.logic Targ.logic List.logic,
        Nat.logic,
        int logic Jtype.logic,
        int logic Jtype.logic Option.logic,
        int logic Jtype.logic List.logic )
      Jtype.t ->
      _ = function
    | Jtype.Null -> fprintf ppf "null"
    | Array t -> fprintf ppf "Array<%a>" main t
    | Interface (id, Value Std.List.Nil) | Class (id, Value Std.List.Nil) ->
        fprintf ppf "%s" (name_of id)
    | Interface (id, args) | Class (id, args) ->
        fprintf ppf "%s<%a>" (name_of id)
          (GT.fmt Std.List.logic (pp_targ_logic name_of))
          args
    | Intersect args ->
        fprintf ppf "Intersect %a" (GT.fmt Std.List.logic main) args
    | Var { upb; lwb = Value None; _ } -> fprintf ppf "(? extends %a)" main upb
    | Var { upb; lwb = Value (Some lwb); _ } ->
        fprintf ppf "(? extends %a super %a)" main upb main lwb
    | Var { upb = _; lwb = Var _; _ } ->
        fprintf ppf "Not implemented %s %d" __FILE__ __LINE__
  and main : _ -> int logic Jtype.logic -> _ =
   fun ppf x -> GT.fmt OCanren.logic helper ppf x
  in
  main

and pp_pol ppf a = Format.fprintf ppf "%s" ((GT.show Polarity.logic) a)

and pp_targ_logic name_of : Format.formatter -> _ -> _ =
 fun ppf ->
  GT.fmt OCanren.logic
    (fun ppf -> function
      | Targ.Type t -> pp_jtyp_logic name_of ppf t
      | Wildcard (Value None) -> Format.fprintf ppf "?"
      | Wildcard (Value (Some (Value (pol, t)))) ->
          Format.fprintf ppf "? %a %a" pp_pol pol (pp_jtyp_logic name_of) t
      | _ -> assert false)
    ppf

let sep () = Printf.printf "\n\n%s\n\n" @@ String.make 100 '*'

(**************************************************************************************************)
(**************************************** Injectors ***********************************************)
(**************************************************************************************************)

let pair_inj : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> ('b, 'd) Std.Pair.injected
    =
 fun f g (a, b) -> !!(f a, g b)

let polarity_inj : Polarity.t -> Polarity.injected = OCanren.inj

let option_inj : ('a -> 'b) -> 'a option -> 'b Std.Option.injected =
 fun f -> function None -> Std.none () | Some x -> Std.some (f x)

let rec targ_inj :
    int Jtype.ground Targ.ground -> int ilogic Jtype.injected Targ.injected =
  let open Targ in
  function
  | Type t -> !!(Type (jtype_inj t))
  | Wildcard x -> !!(Wildcard (option_inj (pair_inj polarity_inj jtype_inj) x))

and jtype_inj : int Jtype.ground -> int ilogic Jtype.injected =
  let open Jtype in
  function
  | Null -> !!Null
  | Array t -> !!(Array (jtype_inj t))
  | Class (id, args) -> !!(Class (!!id, Std.list targ_inj args))
  | Interface (id, args) -> !!(Interface (!!id, Std.list targ_inj args))
  | Var { id; index; upb; lwb } ->
      !!(Var
           {
             id = !!id;
             index = Std.Nat.nat index;
             upb = jtype_inj upb;
             lwb = option_inj jtype_inj lwb;
           })
  | Intersect l -> !!(Intersect (Std.list jtype_inj l))

let decl_inj : int Decl.ground -> int ilogic Decl.injected =
  let open Decl in
  function
  | I { params; supers } ->
      !!(I
           {
             params = Std.list jtype_inj params;
             supers = Std.list jtype_inj supers;
           })
  | C { params; super; supers } ->
      !!(C
           {
             params = Std.list jtype_inj params;
             super = jtype_inj super;
             supers = Std.list jtype_inj supers;
           })

let class_ id args : int ilogic Jtype.injected = !!(Jtype.Class (id, args))
let interface id args = !!(Jtype.Interface (id, args))
let array t = !!(Jtype.Array t)
let intersect xs = !!(Jtype.Intersect xs)
let wildcard xs : _ Targ.injected = !!(Targ.Wildcard xs)
let type_ t : _ Targ.injected = !!(Targ.Type t)
let var ~index id lwb upb = !!(Jtype.Var { index; id; lwb; upb })

(**************************************************************************************************)
(*********************************** Relational helpers *******************************************)
(**************************************************************************************************)

let only_classes_interfaces_and_arrays : int ilogic Jtype.injected -> goal =
 fun q ->
  fresh ()
    (q =/= Jtype.null ())
    (q =/= Jtype.intersect __)
    (q =/= Jtype.var __ __ __ __)

module JGS_PP = struct
  [@@@ocaml.warnerror "-8-39"]

  open Format

  let rec jtyp ppf = function
    | Jtype.Var { id; index; upb; lwb } -> (
        fprintf ppf "Var { id=%d; index=%a; upb=%a " id Std.Nat.fmt_ground index
          jtyp upb;
        match lwb with
        | None -> fprintf ppf "}"
        | Some n -> fprintf ppf "; lwb=%a }" jtyp n)
    | Interface (id, []) -> fprintf ppf "interface %d" id
    | Interface (id, ts) ->
        fprintf ppf "(interface %d<%a>)" id (pp_print_list pp_arg) ts
    | Class (id, []) -> fprintf ppf "class %d" id
    | _ -> fprintf ppf "jtyp"

  and jtyp_list ppf ps = fprintf ppf "%a" (GT.fmt GT.list jtyp) ps

  and pp_arg ppf = function
    | Targ.Type t -> fprintf ppf "%a" jtyp t
    | Wildcard _ -> fprintf ppf "arg?"

  let decl : _ -> int Decl.ground -> unit =
   fun ppf -> function
    | C { params; super; supers } -> (
        fprintf ppf "class ";
        (match params with
        | [] -> ()
        | ps -> fprintf ppf "<%a>" (pp_print_list jtyp) ps);
        fprintf ppf "extends %a" jtyp super;
        match supers with
        | [] -> ()
        | is -> fprintf ppf "implements %a" (pp_print_list jtyp) is)
    | I { params; supers } -> (
        fprintf ppf "interface ";
        (match params with
        | [] -> ()
        | ps -> fprintf ppf "<%a>" (pp_print_list jtyp) ps);
        match supers with
        | [] -> ()
        | is -> fprintf ppf "implements %a" (pp_print_list jtyp) is)
end
