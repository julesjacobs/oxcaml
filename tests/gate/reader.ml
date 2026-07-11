(* SMT-LIB2 -> [Ast.query] for the QF_UFLIA subset.

   Two failure modes, deliberately distinct (the caller maps them to different exit codes,
   DESIGN task):
   - [Malformed]: broken input the reader cannot make sense of as a query (bad s-expr,
     unknown command shape, ill-sorted term, undeclared symbol).
   - [Unsupported]: well-formed but outside our subset (a logic we don't model, a theory
     symbol we don't implement, nonlinear multiplication, quantifiers).

   Supported commands: set-logic, set-info, declare-sort (arity 0), declare-fun,
   declare-const, assert, check-sat, exit. Supported terms: true false, and or not =>,
   ite, = distinct, <= < >= >, + - *, integer numerals, let, plus declared symbols. [=]
   over Bool operands is iff: [normalize] rewrites it to the [Iff] node (chains desugar
   pairwise); [distinct] over Bool is pairwise [<>].

   TCB hardening (codex G1-G4, see NOTES.md): token KIND from [Sexp] is honoured — a
   [Quoted] |sym| is ALWAYS a plain symbol (never a numeral/keyword/operator), a [Str]
   "..." is inert data (never a command/term). A single [check-sat] bounds the query:
   asserts after it, or a second check-sat, are UNSUPPORTED (no silent union).
   [div]/[mod]/ [abs] are recognised but UNSUPPORTED (loud) — need encoder elimination
   (M4). *)

open Ast

exception Malformed of string
exception Unsupported of string

let malformedf fmt = Printf.ksprintf (fun s -> raise (Malformed s)) fmt
let unsupportedf fmt = Printf.ksprintf (fun s -> raise (Unsupported s)) fmt

(* ---- environment built up while reading declarations ---- *)

type env =
  { mutable sorts : string list
  ; mutable funs : (string * sort list * sort) list
  ; scope : (string * term) list (* let-bound substitutions, innermost first *)
  }

let sort_of_sexp env (s : Sexp.t) : sort =
  match s with
  | Sexp.Atom "Bool" -> Bool
  | Sexp.Atom "Int" -> Int
  | Sexp.Atom name ->
    if List.mem name env.sorts then Usort name else malformedf "unknown sort: %s" name
  | Sexp.Quoted name ->
    (* A |quoted| sort name is a literal symbol — even |Int|/|Bool| are user sorts, not
       the builtins (codex G1). *)
    if List.mem name env.sorts then Usort name else malformedf "unknown sort: |%s|" name
  | Sexp.Str _ -> malformedf "string literal is not a sort"
  | Sexp.List _ ->
    unsupportedf "parametric/compound sorts are not supported: %s" (Sexp.to_string s)
;;

let lookup_fun env name = List.find_opt (fun (n, _, _) -> String.equal n name) env.funs

(* A symbol reference by name (declared 0-ary const or function head): reused by both
   [Atom] (unquoted) and [Quoted] paths. A [Quoted] token reaches here ONLY as a symbol,
   never as a numeral/keyword/operator. *)
let read_symbol env name =
  match List.assoc_opt name env.scope with
  | Some t -> t
  | None ->
    (match lookup_fun env name with
     | Some (_, [], _) -> Const name
     | Some (_, _ :: _, _) -> malformedf "function %s used without arguments" name
     | None -> malformedf "undeclared symbol: %s" name)
;;

(* ---- numerals ---- *)

let is_numeral s = String.length s > 0 && String.for_all (fun c -> c >= '0' && c <= '9') s

(* ---- term reader (with sort checking) ---- *)

let rec read_term env (s : Sexp.t) : term =
  match s with
  | Sexp.Atom "true" -> True
  | Sexp.Atom "false" -> False
  | Sexp.Atom a when is_numeral a -> Int_lit a
  | Sexp.Atom a -> read_symbol env a
  (* A |quoted| token is a symbol ONLY — never a numeral/keyword/operator (codex G1). *)
  | Sexp.Quoted a -> read_symbol env a
  | Sexp.Str _ -> malformedf "string literal in term position"
  | Sexp.List (Sexp.Atom "let" :: rest) -> read_let env rest
  | Sexp.List (Sexp.Atom op :: args) -> read_app env op args s
  (* A quoted head is a symbol, so it can only be an uninterpreted-function application —
     never a built-in operator (codex G1: |ite|/|+| are symbols, not operators). *)
  | Sexp.List (Sexp.Quoted f :: args) -> read_fun_app env f args
  | Sexp.List (Sexp.Str _ :: _) -> malformedf "string literal in operator position"
  | Sexp.List [] -> malformedf "empty application ()"
  | Sexp.List (hd :: _) ->
    unsupportedf "higher-order / non-symbol application head: %s" (Sexp.to_string hd)

and read_fun_app env name args =
  match lookup_fun env name with
  | Some (_, params, _) ->
    let n_expect = List.length params
    and n_got = List.length args in
    if n_expect <> n_got
    then malformedf "%s applied to %d args, expected %d" name n_got n_expect;
    App (name, List.map (read_term env) args)
  | None -> malformedf "undeclared function or unknown operator: %s" name

and read_let env rest =
  match rest with
  | [ Sexp.List bindings; body ] ->
    let binding_name = function
      | Sexp.Atom name | Sexp.Quoted name -> name
      | b -> malformedf "malformed let binding variable: %s" (Sexp.to_string b)
    in
    let new_scope =
      List.map
        (fun b ->
           match b with
           | Sexp.List [ nm; def ] -> binding_name nm, read_term env def
           | _ -> malformedf "malformed let binding: %s" (Sexp.to_string b))
        bindings
    in
    (* parallel let: bindings see the outer scope, then extend it *)
    read_term { env with scope = new_scope @ env.scope } body
  | _ -> malformedf "malformed let (expected (let (bindings) body))"

and read_app env op args _orig =
  let t () = List.map (read_term env) args in
  match op, args with
  | "not", [ a ] -> Not (read_term env a)
  | "not", _ -> malformedf "not expects 1 argument"
  | "and", _ :: _ :: _ -> And (t ())
  | "or", _ :: _ :: _ -> Or (t ())
  | "=>", _ :: _ :: _ -> read_implies env args
  | "ite", [ c; th; el ] -> Ite (read_term env c, read_term env th, read_term env el)
  | "ite", _ -> malformedf "ite expects 3 arguments"
  | "=", [ a; b ] -> Eq (read_term env a, read_term env b)
  | "=", _ -> read_chain_eq env args
  | "distinct", _ :: _ :: _ -> Distinct (t ())
  | "<=", _ :: _ :: _ -> read_chain env (fun a b -> Le (a, b)) args
  | "<", _ :: _ :: _ -> read_chain env (fun a b -> Lt (a, b)) args
  | ">=", _ :: _ :: _ -> read_chain env (fun a b -> Ge (a, b)) args
  | ">", _ :: _ :: _ -> read_chain env (fun a b -> Gt (a, b)) args
  | "+", _ :: _ -> Add (t ())
  | "*", _ :: _ -> Mul (t ())
  | "-", [ a ] -> Neg (read_term env a)
  | "-", _ :: _ :: _ -> Sub (t ())
  | ("forall" | "exists"), _ -> unsupportedf "quantifiers are not supported (QF only)"
  | ("div" | "mod" | "abs"), _ ->
    (* Recognised LIA operators the gate cannot certify TODAY (codex G4): grind does not
       reason about Lean's Euclidean [Int.ediv]/[Int.emod] (verified by experiment — it
       treats them as opaque), so emitting them would only ever yield INCONCLUSIVE; [abs]
       is the same class (codex). Real support needs the encoder eliminations (div/mod:
       fresh q,r + side constraints; abs: ite(x>=0,x,-x)), a separate TCB feature tracked
       for M4 LIA. Classify LOUD + distinct (UNSUPPORTED, not MALFORMED) so the coverage
       gap is visible in the digest/quarantine rather than a silent MALFORMED-green. *)
    unsupportedf
      "%s not yet certifiable by the gate (grind lacks Euclidean ediv/emod reasoning; \
       abs needs ite elimination); needs encoder elimination — tracked for M4 LIA"
      op
  | _ ->
    (* uninterpreted function application (unquoted non-operator head) *)
    read_fun_app env op args

(* [(=> a b c)] desugars to [a => (b => c)] (right associative). *)
and read_implies env args =
  match List.rev_map (read_term env) args with
  | last :: rest -> List.fold_left (fun acc a -> Implies (a, acc)) last rest
  | [] -> malformedf "=> expects arguments"

(* [(= a b c)] means all equal: [a=b and b=c]. *)
and read_chain_eq env args =
  let ts = List.map (read_term env) args in
  build_chain (fun a b -> Eq (a, b)) ts

and read_chain env mk args =
  let ts = List.map (read_term env) args in
  build_chain mk ts

and build_chain mk = function
  | a :: (_ :: _ as rest) ->
    let pairs =
      let rec loop = function
        | x :: (y :: _ as tl) -> mk x y :: loop tl
        | _ -> []
      in
      loop (a :: rest)
    in
    (match pairs with
     | [ one ] -> one
     | many -> And many)
  | _ -> malformedf "chained relation needs >= 2 arguments"
;;

(* ---- sort inference / checking on the built term ---- *)

let rec sort_of env (t : term) : sort =
  match t with
  | True | False -> Bool
  | Int_lit _ -> Int
  | Neg a ->
    expect env Int a;
    Int
  | Const name ->
    (match lookup_fun env name with
     | Some (_, [], ret) -> ret
     | _ -> malformedf "internal: const %s not 0-ary" name)
  | App (name, args) ->
    (match lookup_fun env name with
     | Some (_, params, ret) ->
       List.iter2 (fun p a -> expect env p a) params args;
       ret
     | None -> malformedf "internal: undeclared %s" name)
  | Not a ->
    expect env Bool a;
    Bool
  | And xs | Or xs ->
    List.iter (expect env Bool) xs;
    Bool
  | Implies (a, b) ->
    expect env Bool a;
    expect env Bool b;
    Bool
  | Ite (c, th, el) ->
    expect env Bool c;
    let s = sort_of env th in
    expect env s el;
    s
  | Eq (a, b) ->
    (* Bool-sorted [=] is rewritten to [Iff] by [normalize] before this runs; a residual
       Bool [Eq] would be Lean Prop-equality, so guard against it. *)
    let s = sort_of env a in
    (match s with
     | Bool -> malformedf "internal: Bool-sorted Eq should have been normalized to Iff"
     | _ -> ());
    expect env s b;
    Bool
  | Iff (a, b) ->
    expect env Bool a;
    expect env Bool b;
    Bool
  | Distinct xs ->
    (match xs with
     | [] | [ _ ] -> malformedf "distinct needs >= 2 arguments"
     | hd :: tl ->
       let s = sort_of env hd in
       List.iter (expect env s) tl;
       Bool)
  | Le (a, b) | Lt (a, b) | Ge (a, b) | Gt (a, b) ->
    expect env Int a;
    expect env Int b;
    Bool
  | Add xs | Sub xs ->
    List.iter (expect env Int) xs;
    Int
  | Mul xs ->
    List.iter (expect env Int) xs;
    check_linear env xs;
    Int

and expect env (s : sort) (t : term) : unit =
  let s' = sort_of env t in
  if not (sort_equal s s')
  then
    malformedf
      "sort mismatch: expected %s but got %s in %s"
      (sort_to_string s)
      (sort_to_string s')
      (describe t)

and sort_equal a b =
  match a, b with
  | Bool, Bool | Int, Int -> true
  | Usort x, Usort y -> String.equal x y
  | _ -> false

(* Linearity: at most one factor may be non-constant (DESIGN §1: LIA is linear). A factor
   is "constant" if it is a literal or a negation of a constant. *)
and check_linear env xs =
  let rec is_const = function
    | Int_lit _ -> true
    | Neg a -> is_const a
    | _ -> false
  in
  ignore env;
  let nonconst = List.filter (fun x -> not (is_const x)) xs in
  match nonconst with
  | [] | [ _ ] -> ()
  | _ -> unsupportedf "nonlinear multiplication (>= 2 non-constant factors)"

and describe t =
  match t with
  | Const n -> Printf.sprintf "symbol %s" n
  | App (n, _) -> Printf.sprintf "application of %s" n
  | Int_lit n -> Printf.sprintf "numeral %s" n
  | _ -> "subterm"
;;

(* Rewrite Bool-sorted [Eq] to [Iff] (SMT [=] over Bool is iff). Runs after [read_term]
   and before the assert-level sort check; recurses fully because a Bool equality can be
   nested (e.g. inside [and]/[ite]). Post-order so the [sort_of] probe on an [Eq]'s
   operand sees already-rewritten children. *)
let rec normalize env (t : term) : term =
  match t with
  | True | False | Int_lit _ | Const _ -> t
  | App (f, args) -> App (f, List.map (normalize env) args)
  | Not a -> Not (normalize env a)
  | And xs -> And (List.map (normalize env) xs)
  | Or xs -> Or (List.map (normalize env) xs)
  | Implies (a, b) -> Implies (normalize env a, normalize env b)
  | Ite (c, th, el) -> Ite (normalize env c, normalize env th, normalize env el)
  | Eq (a, b) ->
    let a = normalize env a
    and b = normalize env b in
    (match sort_of env a with
     | Bool -> Iff (a, b)
     | _ -> Eq (a, b))
  | Iff (a, b) -> Iff (normalize env a, normalize env b)
  | Distinct xs -> Distinct (List.map (normalize env) xs)
  | Le (a, b) -> Le (normalize env a, normalize env b)
  | Lt (a, b) -> Lt (normalize env a, normalize env b)
  | Ge (a, b) -> Ge (normalize env a, normalize env b)
  | Gt (a, b) -> Gt (normalize env a, normalize env b)
  | Add xs -> Add (List.map (normalize env) xs)
  | Sub xs -> Sub (List.map (normalize env) xs)
  | Neg a -> Neg (normalize env a)
  | Mul xs -> Mul (List.map (normalize env) xs)
;;

(* ---- command reader ---- *)

let arity_zero_sort_decl name = function
  | [ Sexp.Atom "0" ] -> name
  | _ -> unsupportedf "declare-sort %s with nonzero arity" name
;;

let read_signature env (params : Sexp.t) (ret : Sexp.t) =
  let params =
    match params with
    | Sexp.List ps -> List.map (sort_of_sexp env) ps
    | _ -> malformedf "declare-fun parameter list must be a list"
  in
  params, sort_of_sexp env ret
;;

let read_status = function
  | "sat" -> Some Sat
  | "unsat" -> Some Unsat
  | "unknown" -> Some Unknown
  | other -> malformedf "unknown :status value: %s" other
;;

let of_string (src : string) : query =
  let sexps =
    try Sexp.parse_many src with
    | Sexp.Malformed m -> raise (Malformed ("s-expression: " ^ m))
  in
  let env = { sorts = []; funs = []; scope = [] } in
  let logic = ref None in
  let asserts = ref [] in
  let status = ref None in
  (* Single-query model (codex G3): the gate certifies ONE theorem — the conjunction of
     the asserts up to a single [check-sat]. Asserts after a check-sat, or a second
     check-sat, are rejected LOUDLY (Unsupported) rather than silently unioned into the
     theorem. *)
  let checked = ref false in
  (* A declared name may be an unquoted [Atom] or a [Quoted] symbol (e.g. |0|). *)
  let decl_name = function
    | Sexp.Atom name | Sexp.Quoted name -> name
    | other -> malformedf "expected a symbol name, got %s" (Sexp.to_string other)
  in
  let declare_fun name params ret =
    if Option.is_some (lookup_fun env name)
    then malformedf "redeclaration of symbol %s" name;
    env.funs <- (name, params, ret) :: env.funs
  in
  List.iter
    (fun cmd ->
       match cmd with
       | Sexp.List [ Sexp.Atom "set-logic"; Sexp.Atom l ] ->
         (match l with
          | "QF_UFLIA" | "QF_UF" | "QF_LIA" | "QF_IDL" | "QF_RDL" -> logic := Some l
          | _ -> unsupportedf "unsupported logic: %s (need QF_UF/QF_LIA/QF_UFLIA)" l)
       | Sexp.List (Sexp.Atom "set-info" :: rest) ->
         (match rest with
          | [ Sexp.Atom ":status"; Sexp.Atom v ] -> status := read_status v
          | _ -> () (* ignore other :info (values, incl. "strings", are inert) *))
       | Sexp.List [ Sexp.Atom "declare-sort"; nm; arity ] ->
         let name = arity_zero_sort_decl (decl_name nm) [ arity ] in
         if List.mem name env.sorts then malformedf "redeclaration of sort %s" name;
         env.sorts <- name :: env.sorts
       | Sexp.List [ Sexp.Atom "declare-const"; nm; ret ] ->
         declare_fun (decl_name nm) [] (sort_of_sexp env ret)
       | Sexp.List [ Sexp.Atom "declare-fun"; nm; params; ret ] ->
         let params, ret = read_signature env params ret in
         declare_fun (decl_name nm) params ret
       | Sexp.List [ Sexp.Atom "assert"; body ] ->
         if !checked
         then
           unsupportedf
             "assert after check-sat: the gate certifies a single query (no incremental \
              asserts)";
         let term = normalize env (read_term env body) in
         (* sort-check: assertions must be Bool *)
         (match sort_of env term with
          | Bool -> ()
          | other -> malformedf "assertion is not Bool (got %s)" (sort_to_string other));
         asserts := term :: !asserts
       | Sexp.List (Sexp.Atom "check-sat" :: _) ->
         if !checked
         then
           unsupportedf "multiple check-sat commands: the gate certifies a single query";
         checked := true
       | Sexp.List (Sexp.Atom "exit" :: _) -> ()
       | Sexp.List (Sexp.Atom "push" :: _) | Sexp.List (Sexp.Atom "pop" :: _) ->
         unsupportedf "incremental push/pop not supported by the gate reader"
       | Sexp.List (Sexp.Atom ("get-model" | "get-value" | "get-unsat-core") :: _) -> ()
       | Sexp.List (Sexp.Atom "set-option" :: _) -> ()
       | Sexp.List (Sexp.Atom "define-fun" :: _) ->
         unsupportedf "define-fun (macros) not supported by the gate reader"
       | Sexp.Atom a -> malformedf "unexpected top-level atom: %s" a
       | Sexp.Quoted a -> malformedf "unexpected top-level |quoted| token: %s" a
       | Sexp.Str _ -> malformedf "unexpected top-level string literal"
       | Sexp.List (Sexp.Atom other :: _) -> unsupportedf "unsupported command: %s" other
       | Sexp.List (Sexp.Quoted _ :: _) | Sexp.List (Sexp.Str _ :: _) ->
         malformedf "command head must be a bare keyword, not a quoted symbol or string"
       | Sexp.List _ -> malformedf "malformed command: %s" (Sexp.to_string cmd))
    sexps;
  { logic = !logic
  ; sort_decls = List.rev env.sorts
  ; fun_decls = List.rev env.funs
  ; asserts = List.rev !asserts
  ; status = !status
  }
;;

(* Recompute an env from a finished query, for callers (encoder) that need [sort_of] /
   [lookup_fun] outside the reader. *)
let env_of_query (q : query) : env =
  { sorts = q.sort_decls; funs = List.rev q.fun_decls; scope = [] }
;;
