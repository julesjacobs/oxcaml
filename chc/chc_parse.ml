(* HORN front end: parse an SMT-LIB2 [(set-logic HORN)] document into a {!Chc_ast.system}.

   Reuses the shared lexer + s-expression layer from the test-only SMT-LIB parser
   ({!Oxsmt_smtlib_parser.Sexp}) for tokenizing and paren nesting, then interprets the
   HORN-specific command/clause grammar itself. Predicate declarations and forall-
   implication clauses over LIA are supported; anything outside that fragment raises
   {!Unsupported} (so the driver reports a sound [unknown]) or {!Malformed} (a genuinely
   ill-formed input). *)

module Sexp = Oxsmt_smtlib_parser.Sexp
module Lexer = Oxsmt_lexical.Lexer
module Sort = Oxsmt_core.Sort
module Bigint = Oxsmt_core.Bigint
open Chc_ast

exception Malformed of string
exception Unsupported of string

let malformed fmt = Printf.ksprintf (fun s -> raise (Malformed s)) fmt
let unsupported fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

(* Strip SMT-LIB leading-zero-lenient numerals down to the strict {!Bigint.of_string}
   grammar (no leading zeros; "0" for zero). *)
let bigint_of_numeral (s : string) : Bigint.t =
  let n = String.length s in
  let i = ref 0 in
  while !i < n - 1 && s.[!i] = '0' do
    incr i
  done;
  Bigint.of_string (String.sub s !i (n - !i))
;;

let atom_text = function
  | Sexp.Atom (Lexer.Symbol { text; _ }) -> Some text
  | Sexp.Atom (Lexer.Reserved r) -> Some r
  | _ -> None
;;

(* A binding [(name Sort)] as it appears in forall binders and declare-fun signatures. *)
let parse_sort (sx : Sexp.t) : Sort.t =
  match atom_text sx with
  | Some "Int" -> Sort.int
  | Some "Bool" -> Sort.bool
  | Some other -> unsupported "unsupported sort: %s" other
  | None -> malformed "malformed sort: %s" (Sexp.to_string sx)
;;

let parse_binder (sx : Sexp.t) : string * Sort.t =
  match sx with
  | Sexp.List [ name; sort ] ->
    (match atom_text name with
     | Some n -> n, parse_sort sort
     | None -> malformed "malformed binder name: %s" (Sexp.to_string name))
  | _ -> malformed "malformed binder: %s" (Sexp.to_string sx)
;;

(* A let-environment maps a bound name to the already-parsed [expr] captured at its
   binding site (lexical, capture-avoiding by construction). *)
module Env = Map.Make (String)

let rec parse_expr (lenv : expr Env.t) (sx : Sexp.t) : expr =
  match sx with
  | Sexp.Atom tok -> parse_atom lenv tok
  | Sexp.List (head :: args) ->
    (match atom_text head with
     | Some "let" -> parse_let lenv args
     | Some op -> parse_app lenv op args
     | None -> malformed "malformed application head: %s" (Sexp.to_string head))
  | Sexp.List [] -> malformed "empty list expression"

and parse_atom lenv tok =
  match tok with
  | Lexer.Numeral n -> Int_lit (bigint_of_numeral n)
  | Lexer.Symbol { text = "true"; _ } | Lexer.Reserved "true" -> Bool_lit true
  | Lexer.Symbol { text = "false"; _ } | Lexer.Reserved "false" -> Bool_lit false
  | Lexer.Symbol { text; _ } ->
    (match Env.find_opt text lenv with
     | Some e -> e
     | None -> Var text)
  | Lexer.Decimal _ -> unsupported "decimal literal (real arithmetic)"
  | _ -> malformed "unexpected atom: %s" (Sexp.to_string (Sexp.Atom tok))

and parse_let lenv args =
  match args with
  | [ Sexp.List bindings; body ] ->
    let lenv' =
      List.fold_left
        (fun acc b ->
          match b with
          | Sexp.List [ name; value ] ->
            (match atom_text name with
             | Some n -> Env.add n (parse_expr lenv value) acc
             | None -> malformed "malformed let binding name")
          | _ -> malformed "malformed let binding: %s" (Sexp.to_string b))
        lenv
        bindings
    in
    parse_expr lenv' body
  | _ -> malformed "malformed let"

and parse_app lenv op args =
  let e = parse_expr lenv in
  let es () = List.map e args in
  match op, args with
  | "-", [ a ] -> Neg (e a)
  | "-", _ -> Sub (es ())
  | "+", _ -> Add (es ())
  | "*", [ a; b ] -> Mul (e a, e b)
  | ("div" | "/"), [ a; b ] -> Div (e a, e b)
  | "mod", [ a; b ] -> Mod (e a, e b)
  | "=", [ a; b ] -> Eq (e a, e b)
  | "<=", [ a; b ] -> Le (e a, e b)
  | "<", [ a; b ] -> Lt (e a, e b)
  | ">=", [ a; b ] -> Ge (e a, e b)
  | ">", [ a; b ] -> Gt (e a, e b)
  | "not", [ a ] -> Not (e a)
  | "and", _ -> And (es ())
  | "or", _ -> Or (es ())
  | "=>", [ a; b ] -> Implies (e a, e b)
  | "=>", _ ->
    (* n-ary [(=> a b c)] = [(=> a (=> b c))] *)
    let rec chain = function
      | [ x ] -> e x
      | x :: rest -> Implies (e x, chain rest)
      | [] -> malformed "empty =>"
    in
    chain args
  | "ite", [ c; t; f ] -> Ite (e c, e t, e f)
  | "distinct", _ -> Distinct (es ())
  | "xor", [ a; b ] -> Not (Iff (e a, e b))
  | "*", _ -> unsupported "n-ary multiplication"
  | _ ->
    (* An application of a user symbol: a predicate application (uninterpreted). *)
    Pred_app (op, es ())
;;

(* ------------------------------------------------------------------ *)
(* Normalizing a forall body into a clause. *)
(* ------------------------------------------------------------------ *)

(* Flatten the top-level [and] of the antecedent into conjuncts. *)
let rec flatten_and (e : expr) : expr list =
  match e with
  | And es -> List.concat_map flatten_and es
  | Bool_lit true -> []
  | _ -> [ e ]
;;

(* Split antecedent conjuncts into predicate applications vs interpreted constraints. *)
let split_antecedent (conjuncts : expr list) : app list * expr list =
  List.fold_right
    (fun e (apps, constrs) ->
      match e with
      | Pred_app (name, args) -> { pred = name; args } :: apps, constrs
      | _ -> apps, e :: constrs)
    conjuncts
    ([], [])
;;

(* Normalize [antecedent => consequent] into a {!Chc_ast.clause}. A consequent that is a
   predicate application is the head; [false] gives a query clause; any other
   (interpreted) consequent [C] is folded in as [antecedent /\ not C => false]
   (equisatisfiable). *)
let clause_of_impl ~vars ~antecedent ~consequent : clause =
  let body_apps, constr = split_antecedent (flatten_and antecedent) in
  match consequent with
  | Bool_lit false -> { vars; body_apps; constr; head = H_false }
  | Pred_app (name, args) ->
    { vars; body_apps; constr; head = H_pred { pred = name; args } }
  | other -> { vars; body_apps; constr = Not other :: constr; head = H_false }
;;

(* Parse one asserted formula into a clause. Handles [forall]-quantified implications,
   bare facts, and (as a fallback) a top-level [or] read as a CNF Horn clause. *)
let clause_of_assert (sx : Sexp.t) : clause =
  let rec strip_forall vars sx =
    match sx with
    | Sexp.List [ head; Sexp.List binders; body ] when atom_text head = Some "forall" ->
      let vs = List.map parse_binder binders in
      strip_forall (vars @ vs) body
    | _ -> vars, sx
  in
  let vars, body = strip_forall [] sx in
  let e = parse_expr Env.empty body in
  match e with
  | Implies (a, c) -> clause_of_impl ~vars ~antecedent:a ~consequent:c
  | Or disjuncts ->
    (* CNF form: negative pred-app literals + negated interpreted literals form the body;
       a single positive literal is the head. *)
    let heads, body_lits =
      List.partition_map
        (function
          | Not (Pred_app _ as p) -> Right p
          | Not other -> Right (Not other) (* becomes a positive antecedent constraint *)
          | positive -> Left positive)
        disjuncts
    in
    let antecedent =
      And
        (List.map
           (function
             | Not x -> x
             | x -> Not x)
           body_lits)
    in
    (match heads with
     | [] -> clause_of_impl ~vars ~antecedent ~consequent:(Bool_lit false)
     | [ h ] -> clause_of_impl ~vars ~antecedent ~consequent:h
     | _ -> unsupported "non-Horn clause (multiple positive literals)")
  | Pred_app _ -> clause_of_impl ~vars ~antecedent:(Bool_lit true) ~consequent:e
  | Bool_lit false -> clause_of_impl ~vars ~antecedent:(Bool_lit true) ~consequent:e
  | Not (Pred_app _ as p) ->
    clause_of_impl ~vars ~antecedent:p ~consequent:(Bool_lit false)
  | other -> clause_of_impl ~vars ~antecedent:(Bool_lit true) ~consequent:other
;;

(* ------------------------------------------------------------------ *)
(* Top-level document parse. *)
(* ------------------------------------------------------------------ *)

let parse (src : string) : system =
  let env = Oxsmt_core.Env.create () in
  let ctx = Oxsmt_core.Context.create env in
  let sexps =
    try Sexp.parse_many src with
    | Sexp.Malformed m -> raise (Malformed m)
  in
  let preds = ref [] in
  let clauses = ref [] in
  List.iter
    (fun sx ->
      match sx with
      | Sexp.List (head :: rest) ->
        (match atom_text head, rest with
         | Some "set-logic", _
         | Some "set-info", _
         | Some "set-option", _
         | Some "check-sat", _
         | Some "exit", _
         | Some "get-model", _
         | Some "get-info", _ -> ()
         | Some ("declare-fun" | "declare-rel"), name :: Sexp.List argsorts :: codomain ->
           let n =
             match atom_text name with
             | Some n -> n
             | None -> malformed "malformed predicate name"
           in
           (* HORN predicates have Bool codomain (declare-rel omits it). *)
           (match codomain with
            | [] | [ Sexp.Atom (Lexer.Symbol { text = "Bool"; _ }) ] -> ()
            | [ other ] ->
              unsupported "non-predicate declare-fun codomain: %s" (Sexp.to_string other)
            | _ -> malformed "malformed declare-fun");
           let arg_sorts = List.map parse_sort argsorts in
           preds := { name = n; arg_sorts } :: !preds
         | Some "assert", [ body ] -> clauses := clause_of_assert body :: !clauses
         | Some cmd, _ -> unsupported "unsupported command: %s" cmd
         | None, _ -> malformed "malformed command: %s" (Sexp.to_string sx))
      | _ -> malformed "malformed top-level form: %s" (Sexp.to_string sx))
    sexps;
  { env; ctx; preds = List.rev !preds; clauses = List.rev !clauses }
;;
