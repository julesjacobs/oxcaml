open Oxsmt_core

type fun_table =
  { default : Value.t
  ; cases : (Value.t list * Value.t) list
  }

type t =
  { consts : (string, Value.t) Hashtbl.t
  ; funs : (string, fun_table) Hashtbl.t
  ; sort_card : (string, int) Hashtbl.t
  }

exception Malformed of string

let pow2 width =
  let rec loop acc base n =
    if n = 0
    then acc
    else
      loop
        (if n land 1 = 1 then Bigint.mul acc base else acc)
        (if n = 1 then base else Bigint.mul base base)
        (n lsr 1)
  in
  loop Bigint.one (Bigint.of_int 2) width
;;

let atom_string = function
  | Sexp.Atom s -> Some s
  | Sexp.Quoted _ | Sexp.List _ -> None
;;

let unary_minus = function
  | Sexp.List [ Sexp.Atom "-"; x ] -> Some x
  | Sexp.Atom _ | Sexp.Quoted _ | Sexp.List _ -> None
;;

let binary_divide = function
  | Sexp.List [ Sexp.Atom "/"; p; q ] -> Some (p, q)
  | Sexp.Atom _ | Sexp.Quoted _ | Sexp.List _ -> None
;;

let rec real_parts = function
  | Sexp.Atom s ->
    (match Rational_syntax.decimal s with
     | Some q -> Ok q
     | None -> Error Rational_syntax.Not_a_fraction)
  | Sexp.List [ Sexp.Atom "-"; inner ] ->
    Result.map (fun (num, den) -> Bigint.neg num, den) (real_parts inner)
  | sexp ->
    Rational_syntax.fraction
      ~atom:atom_string
      ~minus:unary_minus
      ~divide:binary_divide
      sexp
;;

(* Interpret a raw sidecar token against a target sort. *)
let value_of_token (sort : Sort.t) (s : Sexp.t) : Value.t =
  let int_of s =
    match int_of_string_opt s with
    | Some n -> n
    | None -> raise (Malformed ("not an integer literal: " ^ s))
  in
  let as_int = function
    | Sexp.Atom a -> int_of a
    (* SMT-LIB writes a negative as [(- n)] with [n] an unsigned numeral. Parse the whole
       signed literal ["-" ^ n] directly rather than negating a parsed-positive [n]: the
       magnitude of [min_int] is [max_int + 1], which is NOT a representable positive
       native int, so [-int_of n] spuriously rejects the perfectly representable
       [min_int]. ADR-0003 makes [min_int] a valid [Int_const] — only operations whose
       RESULT leaves native range (e.g. [neg min_int]) overflow; a value at the boundary
       does not. Anything genuinely out of range (e.g. one past [min_int]) still fails. *)
    | Sexp.List [ Sexp.Atom "-"; Sexp.Atom a ] -> int_of ("-" ^ a)
    | Sexp.Quoted _ | Sexp.List _ -> raise (Malformed "expected an integer value")
  in
  let as_nonnegative_bigint = function
    | Sexp.Atom a ->
      (match Bigint.of_string a with
       | n when Bigint.sign n >= 0 -> n
       | _ -> raise (Malformed "bitvector value must be non-negative")
       | exception Invalid_argument _ ->
         raise (Malformed ("not a canonical bitvector numeral: " ^ a)))
    | Sexp.Quoted _ | Sexp.List _ ->
      raise (Malformed "expected an unsigned decimal bitvector value")
  in
  match sort with
  | Sort.Bool ->
    (match s with
     | Sexp.Atom "true" -> Value.Bool true
     | Sexp.Atom "false" -> Value.Bool false
     | _ -> raise (Malformed "expected true/false for a Bool value"))
  | Sort.Int _ -> Value.Int (as_int s)
  | Sort.Real ->
    (match real_parts s with
     | Ok (num, den) when Bigint.sign den > 0 ->
       (match Value.Rational.of_big_frac ~num ~den with
        | q -> Value.Real q
        | exception Invalid_argument _ -> raise (Malformed "invalid Real value"))
     | Ok _ -> raise (Malformed "Real model denominator must be positive")
     | Error _ -> raise (Malformed "expected an exact Real decimal or fraction"))
  | Sort.Uninterpreted _ ->
    let id = as_int s in
    if id < 0 then raise (Malformed "uninterpreted element index must be non-negative");
    Value.Uninterp (sort, id)
  (* The datatype theory does not emit model values yet, so no datatype-sorted binding
     ever reaches this reader; a datatype value would be a constructor tree, not a scalar
     token. Reject rather than mis-parse until that value shape exists. *)
  | Sort.Datatype _ ->
    raise (Malformed "datatype-sorted model values are not supported yet")
  (* Arrays degrade [sat] to [unknown] (v1), so no array-sorted binding is ever emitted
     into a model for this reader; an array value would be a function graph, not a scalar
     token. Reject rather than mis-parse. *)
  | Sort.Array _ -> raise (Malformed "array-sorted model values are not supported yet")
  | Sort.BitVec width ->
    let bits = as_nonnegative_bigint s in
    if Bigint.compare bits (pow2 width) >= 0
    then
      raise
        (Malformed
           (Printf.sprintf
              "bitvector value %s is out of range for width %d"
              (Bigint.to_string bits)
              width));
    Value.BitVec { width; bits }
;;

let card_check t (sort : Sort.t) (v : Value.t) =
  match v, sort with
  | Value.Uninterp (_, id), Sort.Uninterpreted sym ->
    (match Hashtbl.find_opt t.sort_card (Symbol.name sym) with
     | Some k when id >= k ->
       raise
         (Malformed
            (Printf.sprintf
               "element index %d out of range for sort %s (card %d)"
               id
               (Symbol.name sym)
               k))
     | _ -> ())
  | _ -> ()
;;

let parse_const t decls name value_s =
  if Hashtbl.mem t.consts name || Hashtbl.mem t.funs name
  then raise (Malformed ("duplicate model binding for " ^ name));
  match Reader.Decls.const_sort decls name with
  | None -> raise (Malformed ("model defines undeclared / non-nullary symbol: " ^ name))
  | Some sort ->
    let v = value_of_token sort value_s in
    card_check t sort v;
    Hashtbl.add t.consts name v
;;

let parse_fun t decls name entries =
  if Hashtbl.mem t.consts name || Hashtbl.mem t.funs name
  then raise (Malformed ("duplicate model binding for " ^ name));
  match Reader.Decls.fun_rank decls name with
  | None -> raise (Malformed ("model defines undeclared / non-function symbol: " ^ name))
  | Some rank ->
    let dom = Iarr.to_list rank.Rank.domain in
    let cod = rank.Rank.codomain in
    let default = ref None in
    let cases = ref [] in
    let parse_case args_s res_s =
      let args =
        match args_s with
        | Sexp.List xs ->
          if List.length xs <> List.length dom
          then raise (Malformed ("case arity mismatch for " ^ name));
          List.map2 (fun sort a -> value_of_token sort a) dom xs
        | _ -> raise (Malformed "case arguments must be a list")
      in
      let res = value_of_token cod res_s in
      List.iter2 (fun sort v -> card_check t sort v) dom args;
      card_check t cod res;
      if List.exists (fun (prior, _) -> List.for_all2 Value.equal prior args) !cases
      then raise (Malformed ("duplicate case arguments for function " ^ name));
      cases := (args, res) :: !cases
    in
    List.iter
      (function
        | Sexp.List [ Sexp.Atom "default"; v ] ->
          if Option.is_some !default
          then raise (Malformed ("function " ^ name ^ " has duplicate defaults"));
          default := Some (value_of_token cod v)
        | Sexp.List [ Sexp.Atom "case"; args_s; res_s ] -> parse_case args_s res_s
        | _ -> raise (Malformed ("malformed fun entry for " ^ name)))
      entries;
    (match !default with
     | None -> raise (Malformed ("function " ^ name ^ " has no (default ...)"))
     | Some default ->
       card_check t cod default;
       Hashtbl.add t.funs name { default; cases = List.rev !cases })
;;

let of_string decls (src : string) : t =
  let t =
    { consts = Hashtbl.create 32
    ; funs = Hashtbl.create 16
    ; sort_card = Hashtbl.create 16
    }
  in
  let sexps = Sexp.parse_all src in
  let entries =
    match sexps with
    | [ Sexp.List (Sexp.Atom "model" :: entries) ] -> entries
    | _ -> raise (Malformed "expected a single (model ...) s-expression")
  in
  (* First pass: record sort cardinalities so element-index range checks can fire. *)
  List.iter
    (function
      | Sexp.List [ Sexp.Atom "sort"; name_s; Sexp.Atom card ] ->
        let name =
          match name_s with
          | Sexp.Atom s | Sexp.Quoted s -> s
          | Sexp.List _ -> raise (Malformed "sort name must be an atom")
        in
        if Hashtbl.mem t.sort_card name
        then raise (Malformed ("duplicate cardinality entry for sort " ^ name));
        (match int_of_string_opt card with
         | Some k when k > 0 -> Hashtbl.add t.sort_card name k
         | _ -> raise (Malformed ("sort cardinality must be a positive integer: " ^ card)))
      | _ -> ())
    entries;
  List.iter
    (function
      | Sexp.List [ Sexp.Atom "sort"; _; _ ] -> ()
      | Sexp.List [ Sexp.Atom "const"; name_s; value_s ] ->
        let name =
          match name_s with
          | Sexp.Atom s | Sexp.Quoted s -> s
          | Sexp.List _ -> raise (Malformed "const name must be an atom")
        in
        parse_const t decls name value_s
      | Sexp.List (Sexp.Atom "fun" :: name_s :: entries) ->
        let name =
          match name_s with
          | Sexp.Atom s | Sexp.Quoted s -> s
          | Sexp.List _ -> raise (Malformed "fun name must be an atom")
        in
        parse_fun t decls name entries
      | _ -> raise (Malformed "model entry must be (sort ...) | (const ...) | (fun ...)"))
    entries;
  t
;;

let of_file decls path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr ic)
    (fun () ->
      let len = in_channel_length ic in
      of_string decls (really_input_string ic len))
;;

let lookup_const t sym = Hashtbl.find_opt t.consts (Symbol.name sym)
let lookup_fun t sym = Hashtbl.find_opt t.funs (Symbol.name sym)
let sort_card t name = Hashtbl.find_opt t.sort_card name
