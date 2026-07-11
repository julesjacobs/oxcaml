(* Sidecar model files for the sat direction (and unsat-refutation witnesses).

   Since the solver does not exist yet, sat models are supplied by hand in a sibling file:
   [foo.smt2] -> [foo.model]. Format (an s-expression):

   (model (sort S 2) ; uninterpreted sort S has cardinality 2 => Fin 2 (const x 3) ;
   Int-sorted const x := 3 (value may be negative) (const a 0) ; S-sorted const a :=
   element index 0 (0 <= i < card) (const p true) ; Bool-sorted const p := true (fun f
   (default 0) ; every function needs a default (case (0) 0) ; f applied to arg-value 0
   gives 0 (case (1) 0))) ; arg-values are model values (ints / indices / bools)

   The reader recovers each symbol's SMT sort from the query, so a value like [0] is
   interpreted as an Int or a Fin index according to the symbol's declared sort — the
   model file itself is untyped. *)

exception Bad_model of string

let badf fmt = Printf.ksprintf (fun s -> raise (Bad_model s)) fmt

type value =
  | Vint of string (* signed decimal literal *)
  | Vidx of int (* element index into a Usort's Fin n *)
  | Vbool of bool

(* A value as written in the file, before we know the target sort. *)
type raw =
  | Rnum of string (* possibly-signed integer literal *)
  | Rbool of bool

type fundef =
  { default : raw
  ; cases : (raw list * raw) list
  }

type t =
  { sort_card : (string * int) list
  ; consts : (string * raw) list
  ; funs : (string * fundef) list
  }

let parse_int_lit s =
  let ok =
    String.length s > 0
    &&
    let body = if s.[0] = '-' then String.sub s 1 (String.length s - 1) else s in
    String.length body > 0 && String.for_all (fun c -> c >= '0' && c <= '9') body
  in
  if ok then s else badf "not an integer literal: %s" s
;;

let raw_of_sexp = function
  | Sexp.Atom "true" -> Rbool true
  | Sexp.Atom "false" -> Rbool false
  | Sexp.Atom a -> Rnum (parse_int_lit a)
  | s -> badf "bad model value: %s" (Sexp.to_string s)
;;

let parse_fun name rest =
  let default = ref None in
  let cases = ref [] in
  List.iter
    (fun item ->
       match item with
       | Sexp.List [ Sexp.Atom "default"; v ] -> default := Some (raw_of_sexp v)
       | Sexp.List [ Sexp.Atom "case"; Sexp.List args; v ] ->
         cases := (List.map raw_of_sexp args, raw_of_sexp v) :: !cases
       | _ -> badf "bad fun clause in %s: %s" name (Sexp.to_string item))
    rest;
  match !default with
  | None -> badf "function %s has no (default ...)" name
  | Some default -> { default; cases = List.rev !cases }
;;

let of_string (src : string) : t =
  let sexps =
    try Sexp.parse_many src with
    | Sexp.Malformed m -> badf "s-expression: %s" m
  in
  let body =
    match sexps with
    | [ Sexp.List (Sexp.Atom "model" :: body) ] -> body
    | _ -> badf "model file must be a single (model ...) form"
  in
  let sort_card = ref [] in
  let consts = ref [] in
  let funs = ref [] in
  List.iter
    (fun item ->
       match item with
       | Sexp.List [ Sexp.Atom "sort"; Sexp.Atom s; Sexp.Atom n ] ->
         let n =
           try int_of_string n with
           | _ -> badf "bad cardinality for %s: %s" s n
         in
         if n < 1 then badf "sort %s cardinality must be >= 1" s;
         sort_card := (s, n) :: !sort_card
       | Sexp.List [ Sexp.Atom "const"; Sexp.Atom c; v ] ->
         consts := (c, raw_of_sexp v) :: !consts
       | Sexp.List (Sexp.Atom "fun" :: Sexp.Atom f :: rest) ->
         funs := (f, parse_fun f rest) :: !funs
       | _ -> badf "bad model item: %s" (Sexp.to_string item))
    body;
  { sort_card = List.rev !sort_card; consts = List.rev !consts; funs = List.rev !funs }
;;

let sort_card_of t name =
  match List.assoc_opt name t.sort_card with
  | Some n -> n
  | None -> badf "model does not give cardinality for sort %s" name
;;

(* Coerce a raw value to a typed value given the expected SMT sort. *)
let coerce t (sort : Ast.sort) (r : raw) : value =
  match sort, r with
  | Ast.Int, Rnum s -> Vint s
  | Ast.Bool, Rbool b -> Vbool b
  | Ast.Usort s, Rnum idx ->
    let i = int_of_string idx in
    let card = sort_card_of t s in
    if i < 0 || i >= card then badf "index %d out of range for sort %s (card %d)" i s card;
    Vidx i
  | Ast.Int, Rbool _ -> badf "expected Int value, got bool"
  | Ast.Bool, Rnum _ -> badf "expected Bool value, got number"
  | Ast.Usort s, Rbool _ -> badf "expected element of %s, got bool" s
;;
