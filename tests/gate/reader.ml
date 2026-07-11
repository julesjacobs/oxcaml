(* SMT-LIB2 -> [Ast.query] for the QF_UFLIA subset.

   Two failure modes, deliberately distinct (the caller maps them to different exit codes,
   DESIGN task):
   - [Malformed]: broken input the reader cannot make sense of as a query (bad s-expr,
     unknown command shape, ill-sorted term, undeclared symbol).
   - [Unsupported]: well-formed but outside our subset (a logic we don't model, a theory
     symbol we don't implement, nonlinear multiplication, quantifiers).

   Supported commands: set-logic, set-info, declare-sort (arity 0), declare-fun,
   declare-const, assert, check-sat, exit. Supported terms: true false, and or not =>,
   ite, = distinct, <= < >= >, + - *, integer numerals, let, plus declared symbols. *)

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
  | Sexp.List _ ->
    unsupportedf "parametric/compound sorts are not supported: %s" (Sexp.to_string s)
;;

let lookup_fun env name = List.find_opt (fun (n, _, _) -> String.equal n name) env.funs

(* ---- numerals ---- *)

let is_numeral s = String.length s > 0 && String.for_all (fun c -> c >= '0' && c <= '9') s

(* ---- term reader (with sort checking) ---- *)

let rec read_term env (s : Sexp.t) : term =
  match s with
  | Sexp.Atom "true" -> True
  | Sexp.Atom "false" -> False
  | Sexp.Atom a when is_numeral a -> Int_lit a
  | Sexp.Atom a ->
    (* let-bound? then declared symbol? *)
    (match List.assoc_opt a env.scope with
     | Some t -> t
     | None ->
       (match lookup_fun env a with
        | Some (_, [], _) -> Const a
        | Some (_, _ :: _, _) -> malformedf "function %s used without arguments" a
        | None -> malformedf "undeclared symbol: %s" a))
  | Sexp.List (Sexp.Atom "let" :: rest) -> read_let env rest
  | Sexp.List (Sexp.Atom op :: args) -> read_app env op args s
  | Sexp.List [] -> malformedf "empty application ()"
  | Sexp.List (hd :: _) ->
    unsupportedf "higher-order / non-symbol application head: %s" (Sexp.to_string hd)

and read_let env rest =
  match rest with
  | [ Sexp.List bindings; body ] ->
    let new_scope =
      List.map
        (fun b ->
           match b with
           | Sexp.List [ Sexp.Atom name; def ] -> name, read_term env def
           | _ -> malformedf "malformed let binding: %s" (Sexp.to_string b))
        bindings
    in
    (* parallel let: bindings see the outer scope, then extend it *)
    read_term { env with scope = new_scope @ env.scope } body
  | _ -> malformedf "malformed let (expected (let (bindings) body))"

and read_app env op args orig =
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
  | _ ->
    (* uninterpreted function application *)
    (match lookup_fun env op with
     | Some (_, params, _) ->
       let n_expect = List.length params
       and n_got = List.length args in
       if n_expect <> n_got
       then malformedf "%s applied to %d args, expected %d" op n_got n_expect;
       App (op, t ())
     | None ->
       malformedf "undeclared function or unknown operator: %s" (Sexp.to_string orig))

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
    let s = sort_of env a in
    if
      match s with
      | Bool -> true
      | _ -> false
    then
      unsupportedf "equality between Bool terms; use <=>/= at formula level differently";
    expect env s b;
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
  let declare_fun name params ret =
    if Option.is_some (lookup_fun env name)
    then malformedf "redeclaration of symbol %s" name;
    env.funs <- (name, params, ret) :: env.funs
  in
  List.iter
    (fun cmd ->
       match cmd with
       | Sexp.List (Sexp.Atom "set-logic" :: [ Sexp.Atom l ]) ->
         (match l with
          | "QF_UFLIA" | "QF_UF" | "QF_LIA" | "QF_IDL" | "QF_RDL" -> logic := Some l
          | _ -> unsupportedf "unsupported logic: %s (need QF_UF/QF_LIA/QF_UFLIA)" l)
       | Sexp.List (Sexp.Atom "set-info" :: rest) ->
         (match rest with
          | [ Sexp.Atom ":status"; Sexp.Atom v ] -> status := read_status v
          | _ -> () (* ignore other :info *))
       | Sexp.List [ Sexp.Atom "declare-sort"; Sexp.Atom name; arity ] ->
         let name = arity_zero_sort_decl name [ arity ] in
         if List.mem name env.sorts then malformedf "redeclaration of sort %s" name;
         env.sorts <- name :: env.sorts
       | Sexp.List [ Sexp.Atom "declare-const"; Sexp.Atom name; ret ] ->
         declare_fun name [] (sort_of_sexp env ret)
       | Sexp.List [ Sexp.Atom "declare-fun"; Sexp.Atom name; params; ret ] ->
         let params, ret = read_signature env params ret in
         declare_fun name params ret
       | Sexp.List [ Sexp.Atom "assert"; body ] ->
         let term = read_term env body in
         (* sort-check: assertions must be Bool *)
         (match sort_of env term with
          | Bool -> ()
          | other -> malformedf "assertion is not Bool (got %s)" (sort_to_string other));
         asserts := term :: !asserts
       | Sexp.List (Sexp.Atom "check-sat" :: _) -> ()
       | Sexp.List (Sexp.Atom "exit" :: _) -> ()
       | Sexp.List (Sexp.Atom "push" :: _) | Sexp.List (Sexp.Atom "pop" :: _) ->
         unsupportedf "incremental push/pop not supported by the gate reader"
       | Sexp.List (Sexp.Atom ("get-model" | "get-value" | "get-unsat-core") :: _) -> ()
       | Sexp.List (Sexp.Atom "set-option" :: _) -> ()
       | Sexp.List (Sexp.Atom "define-fun" :: _) ->
         unsupportedf "define-fun (macros) not supported by the gate reader"
       | Sexp.Atom a -> malformedf "unexpected top-level atom: %s" a
       | Sexp.List (Sexp.Atom other :: _) -> unsupportedf "unsupported command: %s" other
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
