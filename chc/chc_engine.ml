(* CHC solving engine: BMC for the UNSAFE side, a Spacer/PDR (IC3) frame loop for the SAFE
   side, both discharged through the shipped oxsmt {!Oxsmt_interface.Session} as a QF_LIA
   oracle over the public API.

   {b Verdict convention} (CHC / SMT-LIB HORN): [Safe] = the clause set is satisfiable (an
   interpretation of the predicates exists — an inductive invariant) = SMT-LIB [sat];
   [Unsafe] = the clause set is unsatisfiable (a derivation of [false] exists) = SMT-LIB
   [unsat].

   {b Soundness bar (from day one).} Neither verdict is trusted on the engine's internal
   bookkeeping alone:
   - an [Unsafe] answer is emitted only after a BMC unrolling of the given depth is
     re-confirmed [sat] by a fresh Session (the concrete counterexample derivation);
   - a [Safe] answer is emitted only after the discovered candidate invariant [Inv] is
     re-verified independently on fresh Sessions: [init => Inv], [Inv /\ T => Inv'], and
     [Inv /\ bad] unsat. A bug in the PDR frame bookkeeping can therefore only ever cost a
     solve to [Unknown] or nontermination (bounded by the effort budget) — never a wrong
     [Safe]/[Unsafe].

   {b Scope of v1.} The engine handles single-predicate transition systems (init / trans /
   bad extracted from the one predicate's clauses) — the classic IC3/PDR shape and the
   bulk of the CHC-COMP LIA-lin set. Multi-predicate or nonlinear systems return [Unknown]
   (BMC/PDR generalization to per-predicate frames is a later stage). *)

module Session = Oxsmt_interface.Session
module Interpolation = Oxsmt_interface.Interpolation
module Context = Oxsmt_core.Context
module Sort = Oxsmt_core.Sort
module Bigint = Oxsmt_core.Bigint
module Term = Oxsmt_core.Term
module Iarr = Oxsmt_core.Iarr
module Symbol = Oxsmt_core.Symbol
open Chc_ast

type verdict =
  | Safe
  | Unsafe
  | Unknown of string

type result =
  { verdict : verdict
  ; detail : string (* human-readable provenance / evidence summary *)
  }

(* ------------------------------------------------------------------ *)
(* expr helpers: variable renaming and free-variable collection. *)
(* ------------------------------------------------------------------ *)

let rec rename (f : string -> string) (e : expr) : expr =
  let r = rename f in
  match e with
  | Var x -> Var (f x)
  | Int_lit _ | Bool_lit _ -> e
  | Neg a -> Neg (r a)
  | Add es -> Add (List.map r es)
  | Sub es -> Sub (List.map r es)
  | Mul (a, b) -> Mul (r a, r b)
  | Div (a, b) -> Div (r a, r b)
  | Mod (a, b) -> Mod (r a, r b)
  | Eq (a, b) -> Eq (r a, r b)
  | Le (a, b) -> Le (r a, r b)
  | Lt (a, b) -> Lt (r a, r b)
  | Ge (a, b) -> Ge (r a, r b)
  | Gt (a, b) -> Gt (r a, r b)
  | Not a -> Not (r a)
  | And es -> And (List.map r es)
  | Or es -> Or (List.map r es)
  | Implies (a, b) -> Implies (r a, r b)
  | Iff (a, b) -> Iff (r a, r b)
  | Ite (a, b, c) -> Ite (r a, r b, r c)
  | Distinct es -> Distinct (List.map r es)
  | Pred_app (n, es) -> Pred_app (n, List.map r es)
;;

module SS = Set.Make (String)

let rec free_vars_acc (acc : SS.t) (e : expr) : SS.t =
  match e with
  | Var x -> SS.add x acc
  | Int_lit _ | Bool_lit _ -> acc
  | Neg a | Not a -> free_vars_acc acc a
  | Add es | Sub es | And es | Or es | Distinct es -> List.fold_left free_vars_acc acc es
  | Mul (a, b)
  | Div (a, b)
  | Mod (a, b)
  | Eq (a, b)
  | Le (a, b)
  | Lt (a, b)
  | Ge (a, b)
  | Gt (a, b)
  | Implies (a, b)
  | Iff (a, b) -> free_vars_acc (free_vars_acc acc a) b
  | Ite (a, b, c) -> free_vars_acc (free_vars_acc (free_vars_acc acc a) b) c
  | Pred_app (_, es) -> List.fold_left free_vars_acc acc es
;;

let free_vars_list (es : expr list) : SS.t = List.fold_left free_vars_acc SS.empty es

(* ------------------------------------------------------------------ *)
(* Transition-system extraction from a single-predicate CHC system. *)
(* ------------------------------------------------------------------ *)

type value =
  | VInt of Bigint.t
  | VBool of bool

(* A relation applied to a state variable in a cube literal. [Req] is the equality a model
   yields; generalization relaxes an integer [Req] to the one-sided [Rle] ([x <= v]) or
   [Rge] ([x >= v]) so a blocked point-set widens into a half-space — the lever that lets
   PDR converge on numeric invariants over the (infinite) integers, where dropping the
   sole literal of an equality cube would never yield the [x >= 0]-style lemma an
   invariant needs. *)
type rel =
  | Req
  | Rle
  | Rge

(* A cube literal. [lin = []] is the template form: [jdx = None] is a single-variable
   literal [x_idx rel v]; [jdx = Some j] is a DIFFERENCE (octagon-style) literal
   [(x_idx - x_j) rel v], the template that lets generalization express a difference-bound
   RELATIONAL invariant (e.g. [x - y = 0], i.e. [x = y]).

   [lin = (k_0,c_0);...] (nonempty) is a GENERAL-LINEAR literal [Σ c_i · x_{k_i} rel v]
   (idx/jdx ignored) — the shape that carries a true {b Farkas interpolant} extracted from
   the [#106] [Session.last_farkas] certificate. This closes the invariant class neither
   interval nor difference-bound generalization can express (e.g. [x + y = 10], a two-var
   equality that is not a difference), the last remaining generalization-strength wall. *)
type lit =
  { idx : int
  ; jdx : int option
  ; rel : rel
  ; v : value
  ; lin : (int * Bigint.t) list
  }

(* A cube is a conjunction of literals over the state variables. *)
type cube = lit list

type ts =
  { arity : int
  ; sorts : (string, Sort.t) Hashtbl.t (* every canonical + aux var name -> sort *)
  ; init : expr (* over x0..x{n-1} *)
  ; trans : expr (* over x0..x{n-1} (pre) and y0..y{n-1} (post) *)
  ; bad : expr (* over x0..x{n-1} *)
  ; trivially_unsafe : expr list
  (* every fact-free "constr => false" body (an ACCUMULATOR, not a single slot: the system
     is UNSAFE if ANY of these constraints is satisfiable — overwriting would mask an
     earlier genuine query) *)
  }

let xname i = Printf.sprintf "x%d" i
let yname i = Printf.sprintf "y%d" i

exception Not_ts of string

(* Extract a transition system from a single-predicate system. Raises {!Not_ts} for a
   shape v1 does not model as a TS. *)
let extract_ts (sys : system) : ts =
  match sys.preds with
  | [] -> raise (Not_ts "no predicates")
  | _ :: _ :: _ -> raise (Not_ts "multiple predicates")
  | [ p ] ->
    let n = arity p in
    let sorts = Hashtbl.create 64 in
    for i = 0 to n - 1 do
      let s = List.nth p.arg_sorts i in
      Hashtbl.replace sorts (xname i) s;
      Hashtbl.replace sorts (yname i) s
    done;
    let clause_ctr = ref 0 in
    (* Rename a clause's own bound variables to globally-unique aux names and register
       their sorts; canonical x/y names never appear in clause exprs so they are safe. *)
    let freshen (c : clause) : string -> string =
      let cid = !clause_ctr in
      incr clause_ctr;
      let m = Hashtbl.create 16 in
      List.iter
        (fun (name, sort) ->
          let fresh = Printf.sprintf "a%d_%s" cid name in
          Hashtbl.replace m name fresh;
          Hashtbl.replace sorts fresh sort)
        c.vars;
      fun x ->
        match Hashtbl.find_opt m x with
        | Some f -> f
        | None -> x
    in
    let eq_bindings ~name args = List.mapi (fun i a -> Eq (Var (name i), a)) args in
    let inits = ref [] in
    let transs = ref [] in
    let bads = ref [] in
    let triv = ref [] in
    List.iter
      (fun (c : clause) ->
        (match c.body_apps with
         | [] | [ _ ] -> ()
         | _ -> raise (Not_ts "clause has multiple body predicates (nonlinear)"));
        let f = freshen c in
        let constr = List.map (rename f) c.constr in
        match c.body_apps, c.head with
        | [], H_pred { pred = _; args } ->
          let args = List.map (rename f) args in
          inits := And (constr @ eq_bindings ~name:xname args) :: !inits
        | [ { pred = _; args = bargs } ], H_pred { pred = _; args = hargs } ->
          let bargs = List.map (rename f) bargs in
          let hargs = List.map (rename f) hargs in
          transs
          := And (constr @ eq_bindings ~name:xname bargs @ eq_bindings ~name:yname hargs)
             :: !transs
        | [ { pred = _; args = bargs } ], H_false ->
          let bargs = List.map (rename f) bargs in
          bads := And (constr @ eq_bindings ~name:xname bargs) :: !bads
        | [], H_false ->
          (* A fact-free "constr => false": the system is unsafe iff [constr] is
             satisfiable (a derivation of false using no predicate). ACCUMULATE — multiple
             such queries must all be kept (overwriting would mask an earlier unsafe one). *)
          triv := And constr :: !triv
        | _ -> raise (Not_ts "unexpected clause shape"))
      sys.clauses;
    let disj = function
      | [] -> Bool_lit false
      | [ x ] -> x
      | xs -> Or xs
    in
    { arity = n
    ; sorts
    ; init = disj !inits
    ; trans = disj !transs
    ; bad = disj !bads
    ; trivially_unsafe = List.rev !triv
    }
;;

(* ------------------------------------------------------------------ *)
(* SMT oracle: build a fresh Session, declare free vars, assert, check. *)
(* ------------------------------------------------------------------ *)

type smt =
  | R_sat
  | R_unsat
  | R_unknown

let query_count = ref 0
let budget_ref = ref max_int

(* Per-[check_sat] effort cap threaded to every oracle Session (board #60 [max_effort]: a
   counted cutoff on SAT conflicts + decisions + seam Final-rounds). Without it a Session
   is UNBOUNDED and a diverging LIA search — e.g. an unbounded-quotient parity
   infeasibility like [2q = 2q'+1] — hangs [check_sat] forever, uninterruptible by our
   between-queries query-count budget. A finite cap makes such a query return [Unknown]
   (budget) so the PDR solve degrades to unknown instead of hanging (per-check,
   poison-free, re-runnable). *)
let effort_cap = ref 1_000_000

(* Raised to abandon a solve to [Unknown] (budget/limit/oracle-unknown); also used with
   the sentinel messages ["__unsafe_*"] to signal an early UNSAFE finding. *)
exception Give_up of string

let check_budget () =
  if !query_count > !budget_ref then raise (Give_up "effort budget exhausted")
;;

(* Declare every free variable of [asserts] as a session constant of its registered sort,
   build each expr through the session context, assert, and check. *)
let solve_exprs (ts : ts) (asserts : expr list) : smt * Session.t =
  incr query_count;
  check_budget ();
  let sess = Session.create ~max_effort:!effort_cap () in
  let ctx = Session.context sess in
  let fvs = free_vars_list asserts in
  let vmap = Hashtbl.create 64 in
  SS.iter
    (fun name ->
      let sort =
        match Hashtbl.find_opt ts.sorts name with
        | Some s -> s
        | None -> Sort.int (* aux vars all registered; default Int defensively *)
      in
      let sym = Session.declare_const sess name sort in
      Hashtbl.replace vmap name (Context.const ctx sym))
    fvs;
  let venv name =
    match Hashtbl.find_opt vmap name with
    | Some t -> t
    | None -> failwith ("unbound variable in build: " ^ name)
  in
  (* Term construction runs OUTSIDE the Session firewall; degrade any ill-sorted /
     unsupported / overflow build on this query to [R_unknown] rather than crashing. *)
  let r =
    match
      List.iter (fun e -> Session.assert_term sess (Chc_ast.build ctx venv e)) asserts
    with
    | () ->
      (match Session.check_sat sess with
       | Session.Sat -> R_sat
       | Session.Unsat -> R_unsat
       | Session.Unknown -> R_unknown)
    | exception Oxsmt_core.Term.Sort_error _ -> R_unknown
    | exception Oxsmt_core.Term.Unsupported _ -> R_unknown
    | exception Oxsmt_core.Term.Overflow -> R_unknown
    | exception Chc_ast.Build_error _ -> R_unknown
  in
  r, sess
;;

let check_exprs ts asserts = fst (solve_exprs ts asserts)

(* Read the state-variable values (x0..x{n-1}) from the most recent Sat model. *)
let model_cube (ts : ts) (sess : Session.t) : cube =
  match Session.get_model sess with
  | None -> []
  | Some (_cards, bindings) ->
    let tbl = Hashtbl.create 16 in
    List.iter
      (function
        | Session.Const (name, v) ->
          (match v with
           | Session.VInt n -> Hashtbl.replace tbl name (VInt n)
           | Session.VBool b -> Hashtbl.replace tbl name (VBool b)
           | Session.VReal _ -> ()
           | Session.VUninterp _ -> ())
        | Session.Fun _ -> ())
      bindings;
    (* Single-variable literals x_i = v_i (present in the model). *)
    let ints = ref [] in
    let acc = ref [] in
    for i = ts.arity - 1 downto 0 do
      match Hashtbl.find_opt tbl (xname i) with
      | Some v ->
        acc := { idx = i; jdx = None; rel = Req; v; lin = [] } :: !acc;
        (match v with
         | VInt n -> ints := (i, n) :: !ints
         | VBool _ -> ())
      | None -> ()
    done;
    (* Difference literals x_i - x_j = v_i - v_j for each ordered int pair (octagon
       template): the seed for relational-invariant generalization. Generalization drops
       or relaxes these; a surviving [x_i - x_j = 0] lemma expresses [x_i = x_j]. *)
    let diffs = ref [] in
    List.iter
      (fun (i, vi) ->
        List.iter
          (fun (j, vj) ->
            if i < j
            then
              diffs
              := { idx = i
                 ; jdx = Some j
                 ; rel = Req
                 ; v = VInt (Bigint.sub vi vj)
                 ; lin = []
                 }
                 :: !diffs)
          !ints)
      !ints;
    !acc @ !diffs
;;

(* ------------------------------------------------------------------ *)
(* Farkas interpolation (task #88), via the #106 Session.last_farkas API. *)
(* ------------------------------------------------------------------ *)

(* Per-solve interpolation statistics (reset in {!solve}): [attempts] predecessor-unsat
   block points where an interpolant was tried; [farkas] attempts where
   {!Session.last_farkas} returned a certificate; [verified] candidate interpolants that
   passed BOTH independent re-checks (A |= I and I /\ B unsat) on fresh Sessions; [used]
   verified interpolants that were also admissible and admitted as a lemma. *)
let interp_attempts = ref 0
let interp_farkas = ref 0
let interp_eq_farkas = ref 0
let interp_verified = ref 0
let interp_used = ref 0

let reset_interp_stats () =
  interp_attempts := 0;
  interp_farkas := 0;
  interp_eq_farkas := 0;
  interp_verified := 0;
  interp_used := 0
;;

let interp_stats () = !interp_attempts, !interp_farkas, !interp_verified, !interp_used
let interp_eq_farkas_count () = !interp_eq_farkas

(* Farkas-interpolant generalization is ON by default; [OXSMT_CHC_INTERP=0] disables it
   (falls back to template generalization only) — an A/B lever for measuring its impact. *)
let interp_on =
  match Sys.getenv_opt "OXSMT_CHC_INTERP" with
  | Some ("0" | "false" | "no" | "off") -> false
  | _ -> true
;;

(* Direct equality-premise interpolation is deliberately dark by default. The public
   interpolation module handles signed equality coefficients, but retaining the legacy
   Eq-to-Le query shape keeps the shipped CHC solve path unchanged. The dedicated consumer
   test enables this flag and proves the generalized path reaches a checked, admitted
   lemma. *)
let interp_eq_on =
  match Sys.getenv_opt "OXSMT_CHC_INTERP_EQ" with
  | Some ("1" | "true" | "yes" | "on") -> true
  | _ -> false
;;

(* Recover the post-state index [k] of a shared variable named ["y{k}"]; [None] for a
   pre-state ([x]) or auxiliary variable — those are A-LOCAL, so a valid interpolant over
   the shared vocabulary must not carry them with a nonzero coefficient. *)
let post_index_of_var (v : Term.t) : int option =
  match v.Term.node with
  | Term.App (sym, args) when Iarr.to_list args = [] ->
    let nm = Symbol.name sym in
    if String.length nm >= 2 && nm.[0] = 'y'
    then (
      try Some (int_of_string (String.sub nm 1 (String.length nm - 1))) with
      | _ -> None)
    else None
  | _ -> None
;;

let rec expr_is_int (sorts : (string, Sort.t) Hashtbl.t) (e : expr) : bool =
  match e with
  | Int_lit _ | Add _ | Sub _ | Mul _ | Neg _ | Mod _ | Div _ -> true
  | Var x ->
    (match Hashtbl.find_opt sorts x with
     | Some s -> Sort.equal s Sort.int
     | None -> true)
  (* An [Ite] is Int-sorted exactly when its branches are; both branches share a sort, so
     either being provably Int classifies the whole conditional. *)
  | Ite (_, a, b) -> expr_is_int sorts a || expr_is_int sorts b
  | _ -> false
;;

(* The legacy CHC evidence query split Int equalities into their two half-planes. Keep it
   for the default path; [OXSMT_CHC_INTERP_EQ=1] bypasses this workaround and exercises
   the public signed-equality evidence directly. *)
let rec split_int_eqs (sorts : (string, Sort.t) Hashtbl.t) (e : expr) : expr =
  let recurse = split_int_eqs sorts in
  match e with
  | Eq (a, b) when expr_is_int sorts a || expr_is_int sorts b ->
    And [ Le (recurse a, recurse b); Le (recurse b, recurse a) ]
  | Eq (a, b) -> Eq (recurse a, recurse b)
  | Var _ | Int_lit _ | Bool_lit _ -> e
  | Neg a -> Neg (recurse a)
  | Not a -> Not (recurse a)
  | Add expressions -> Add (List.map recurse expressions)
  | Sub expressions -> Sub (List.map recurse expressions)
  | And expressions -> And (List.map recurse expressions)
  | Or expressions -> Or (List.map recurse expressions)
  | Distinct expressions -> Distinct (List.map recurse expressions)
  | Mul (a, b) -> Mul (recurse a, recurse b)
  | Div (a, b) -> Div (recurse a, recurse b)
  | Mod (a, b) -> Mod (recurse a, recurse b)
  | Le (a, b) -> Le (recurse a, recurse b)
  | Lt (a, b) -> Lt (recurse a, recurse b)
  | Ge (a, b) -> Ge (recurse a, recurse b)
  | Gt (a, b) -> Gt (recurse a, recurse b)
  | Implies (a, b) -> Implies (recurse a, recurse b)
  | Iff (a, b) -> Iff (recurse a, recurse b)
  | Ite (a, b, c) -> Ite (recurse a, recurse b, recurse c)
  | Pred_app (name, expressions) -> Pred_app (name, List.map recurse expressions)
;;

let interp_environment (ts : ts) session exprs =
  let context = Session.context session in
  let variables = Hashtbl.create 64 in
  SS.iter
    (fun name ->
      let sort =
        match Hashtbl.find_opt ts.sorts name with
        | Some sort -> sort
        | None -> Sort.int
      in
      let symbol = Session.declare_const session name sort in
      Hashtbl.replace variables name (Context.const context symbol))
    (free_vars_list exprs);
  let resolve name =
    match Hashtbl.find_opt variables name with
    | Some term -> term
    | None -> failwith ("unbound variable in interpolation build: " ^ name)
  in
  context, resolve
;;

let build_interp_replay ts ~a_exprs ~b_exprs side session =
  let expressions =
    match side with
    | Interpolation.A -> a_exprs
    | Interpolation.B -> b_exprs
  in
  let context, resolve = interp_environment ts session expressions in
  { Interpolation.assertions = List.map (Chc_ast.build context resolve) expressions
  ; resolve = (fun index -> resolve (yname index))
  }
;;

(* Run the interpolation query [A /\ B] on a fresh Session, returning its verdict, the
   solved session, and the set of B-side atom terms used to partition its evidence.
   [b_exprs] are asserted as INDIVIDUAL atoms so each is a partitionable premise; the
   exact interned [Term.t] of each is collected (hash-consing makes [Term.equal] against a
   returned premise reliable). With [OXSMT_CHC_INTERP_EQ=1], Int equalities are left
   intact for the public interpolation seam's signed equation multipliers; the default
   retains the legacy A-side split. *)
let interp_query (ts : ts) ~(a_exprs : expr list) ~(b_exprs : expr list)
  : smt * (Session.t * Term.Set.t) option
  =
  incr query_count;
  check_budget ();
  let sess = Session.create ~max_effort:!effort_cap () in
  let a_exprs =
    if interp_eq_on then a_exprs else List.map (split_int_eqs ts.sorts) a_exprs
  in
  let ctx, venv = interp_environment ts sess (a_exprs @ b_exprs) in
  let b_atoms = ref Term.Set.empty in
  match
    List.iter (fun e -> Session.assert_term sess (Chc_ast.build ctx venv e)) a_exprs;
    List.iter
      (fun e ->
        let t = Chc_ast.build ctx venv e in
        b_atoms := Term.Set.add t !b_atoms;
        Session.assert_term sess t)
      b_exprs
  with
  | () ->
    let r =
      match Session.check_sat sess with
      | Session.Sat -> R_sat
      | Session.Unsat -> R_unsat
      | Session.Unknown -> R_unknown
    in
    r, Some (sess, !b_atoms)
  | exception Oxsmt_core.Term.Sort_error _ -> R_unknown, None
  | exception Oxsmt_core.Term.Unsupported _ -> R_unknown, None
  | exception Oxsmt_core.Term.Overflow -> R_unknown, None
  | exception Chc_ast.Build_error _ -> R_unknown, None
;;

(* ------------------------------------------------------------------ *)
(* Cube / lemma expression construction. *)
(* ------------------------------------------------------------------ *)

let lit_expr ~prime (l : lit) : expr =
  let nm k = if prime then yname k else xname k in
  (* term the relation constrains: a general linear form [Σ c_i·x_{k_i}] for an
     interpolant literal, else [x_i] / [(x_i - x_j)] for the template literals. *)
  let lhs =
    match l.lin with
    | _ :: _ -> Add (List.map (fun (k, c) -> Mul (Int_lit c, Var (nm k))) l.lin)
    | [] ->
      (match l.jdx with
       | None -> Var (nm l.idx)
       | Some j -> Sub [ Var (nm l.idx); Var (nm j) ])
  in
  match l.v, l.rel with
  | VInt n, Req -> Eq (lhs, Int_lit n)
  | VInt n, Rle -> Le (lhs, Int_lit n)
  | VInt n, Rge -> Ge (lhs, Int_lit n)
  | VBool true, _ -> lhs
  | VBool false, _ -> Not lhs
;;

(* [cube_expr ~prime s] is the conjunction describing the states in cube [s]. *)
let cube_expr ~prime (s : cube) : expr = And (List.map (lit_expr ~prime) s)

(* [clause_expr s] is the negation of cube [s] (over the unprimed state), i.e. the lemma
   "exclude every state in [s]". Empty cube -> [false] (excludes nothing is meaningless;
   an empty CTI cube means the model pinned no state var, handled by the caller). *)
let clause_expr (s : cube) : expr = Not (cube_expr ~prime:false s)

(* Legacy B-side rendering for byte-stable default queries: an equality cube literal is
   asserted as its two [Le] half-planes. The direct equality form is selected only by
   [OXSMT_CHC_INTERP_EQ=1]. *)
let cube_lit_ineqs ~prime (l : lit) : expr list =
  let nm k = if prime then yname k else xname k in
  let lhs =
    match l.lin with
    | _ :: _ -> Add (List.map (fun (k, c) -> Mul (Int_lit c, Var (nm k))) l.lin)
    | [] ->
      (match l.jdx with
       | None -> Var (nm l.idx)
       | Some j -> Sub [ Var (nm l.idx); Var (nm j) ])
  in
  match l.v, l.rel with
  | VInt n, Req -> [ Le (lhs, Int_lit n); Le (Int_lit n, lhs) ]
  | VInt n, Rle -> [ Le (lhs, Int_lit n) ]
  | VInt n, Rge -> [ Le (Int_lit n, lhs) ]
  | VBool _, _ -> []
;;

(* ------------------------------------------------------------------ *)
(* PDR (IC3) frame loop. *)
(* ------------------------------------------------------------------ *)

type pdr =
  { ts : ts
  ; mutable frontier : int
  ; (* lemmas.(i) holds the cubes blocked at level i (their negations are the clauses of
       frame i). A clause valid at level i is valid at all levels < i, so the frame-[i]
       formula is init (i=0) else the conjunction of clause_expr over all cubes at levels
       >= i. *)
    mutable lemmas : cube list array
  }

let frame_formula (p : pdr) (i : int) : expr list =
  if i = 0
  then [ p.ts.init ]
  else (
    let acc = ref [] in
    for j = i to p.frontier do
      List.iter (fun s -> acc := clause_expr s :: !acc) p.lemmas.(j)
    done;
    !acc)
;;

let ensure_frontier (p : pdr) (n : int) =
  if n > p.frontier
  then (
    let a = Array.make (n + 2) [] in
    Array.blit p.lemmas 0 a 0 (Array.length p.lemmas);
    p.lemmas <- a;
    p.frontier <- n)
;;

let add_lemma (p : pdr) (i : int) (s : cube) =
  (* Record at level i; monotonicity is realized by frame_formula scanning levels >= i. *)
  p.lemmas.(i) <- s :: p.lemmas.(i)
;;

let smt_or_giveup = function
  | R_sat -> true
  | R_unsat -> false
  | R_unknown -> raise (Give_up "LIA oracle returned unknown")
;;

(* Does cube [s] intersect the initial states? *)
let intersects_init (p : pdr) (s : cube) : bool =
  smt_or_giveup (check_exprs p.ts [ p.ts.init; cube_expr ~prime:false s ])
;;

(* A candidate generalized cube [cand] is admissible as a blocked cube at level [i] iff it
   is disjoint from init and has no predecessor in frame [i-1] (so its negation is a sound
   lemma to add — independently re-verified at the end regardless). *)
let admissible (p : pdr) (i : int) (cand : cube) : bool =
  cand <> []
  && (not (smt_or_giveup (check_exprs p.ts [ p.ts.init; cube_expr ~prime:false cand ])))
  && not
       (smt_or_giveup
          (check_exprs
             p.ts
             (frame_formula p (i - 1)
              @ [ clause_expr cand; p.ts.trans; cube_expr ~prime:true cand ])))
;;

(* Generalize cube [s] (blocked at level [i]). For each literal we try, in order, the
   widest still-admissible replacement: drop it entirely; else — for an integer equality —
   relax it to the one-sided [x <= v] or [x >= v]. Relaxing equalities to half-spaces is
   what lets the negated lemma become an inequality (e.g. [x >= 0]) and PDR converge on a
   numeric invariant, rather than enumerating integer points forever. *)
let generalize (p : pdr) (i : int) (s : cube) : cube =
  let step cur (l : lit) =
    check_budget ();
    let without = List.filter (fun x -> x != l) cur in
    let replace nl = List.map (fun x -> if x == l then nl else x) cur in
    let candidates =
      match l.v, l.rel with
      | VInt _, Req ->
        [ without; replace { l with rel = Rle }; replace { l with rel = Rge } ]
      | _ -> [ without ]
    in
    match List.find_opt (admissible p i) candidates with
    | Some better -> better
    | None -> cur
  in
  List.fold_left step s s
;;

(* Attempt a Farkas-interpolant lemma to block cube [s] at level [i], a strictly stronger
   replacement for the template {!generalize} that can express invariants neither interval
   nor difference-bound generalization reaches (e.g. [x + y = 10]). The blocking CTI query
   is split A = [R_{i-1} /\ T], B = [s'] (the goal cube over the post-state). The A-side
   of the returned Farkas certificate sums to a McMillan interpolant [I], and [¬I] is the
   generalized blocked cube.

   FAIL-CLOSED at every step (→ [None], caller falls back to {!generalize}): no Farkas
   certificate, a malformed certificate, a trivial split, an interpolant mentioning an
   A-local variable, or a failed independent re-verification of [A |= I] and [I /\ B]
   unsat. {!Interpolation.interpolate} owns all of those checks and creates a fresh
   Session for each obligation. The final invariant is re-verified independently too. *)
let interpolant_lemma (p : pdr) (i : int) (s : cube) : cube option =
  if List.exists
       (fun l ->
         match l.v with
         | VBool _ -> true
         | _ -> false)
       s
  then None
  else (
    incr interp_attempts;
    let a_exprs = frame_formula p (i - 1) @ [ p.ts.trans ] in
    (* Interpolate against only the TRUE single-variable state literals of the CTI point.
       The octagon difference literals ([jdx = Some _]) drive the Farkas conflict onto a
       difference bound ([x_i - x_j]) that diverges instead of capturing the relational
       invariant; excluding them lets the certificate combine the genuine variable bounds
       (e.g. into [x + y <= 10]). *)
    let b_lits = List.filter (fun l -> l.jdx = None && l.lin = []) s in
    let b_exprs =
      if interp_eq_on
      then List.map (lit_expr ~prime:true) b_lits
      else List.concat_map (cube_lit_ineqs ~prime:true) b_lits
    in
    match interp_query p.ts ~a_exprs ~b_exprs with
    | R_unsat, Some (session, b_atoms) ->
      (match Session.last_farkas session with
       | None -> None
       | Some certificate ->
         if List.exists
              (fun (coefficient, (atom, polarity)) ->
                (not (Oxsmt_lia.Rational.is_zero coefficient))
                && polarity
                &&
                match atom.Term.node with
                | Term.Eq (left, right) ->
                  Sort.equal left.Term.sort Sort.int
                  && Sort.equal right.Term.sort Sort.int
                | _ -> false)
              certificate
         then incr interp_eq_farkas;
         incr interp_farkas;
         let create () =
           incr query_count;
           check_budget ();
           Session.create ~max_effort:!effort_cap ()
         in
         let candidate =
           Interpolation.interpolate
             session
             ~side_of:(fun (atom, _) ->
               if Term.Set.mem atom b_atoms
               then Some Interpolation.B
               else Some Interpolation.A)
             ~project_shared:post_index_of_var
             ~create
             ~build:(build_interp_replay p.ts ~a_exprs ~b_exprs)
             ~is_shared:(fun index -> index >= 0 && index < p.ts.arity)
         in
         (match candidate with
          | None -> None
          | Some interpolant ->
            incr interp_verified;
            Some
              [ { idx = 0
                ; jdx = None
                ; rel = Rge
                ; v = VInt (Bigint.sub Bigint.one interpolant.constant)
                ; lin = interpolant.coefficients
                }
              ]))
    | _ -> None)
;;

(* Recursively block cube [s] at level [i]. Returns [true] if blocked (added a lemma),
   [false] if [s] is reachable from init (a genuine counterexample chain exists). *)
let rec rec_block (p : pdr) (s : cube) (i : int) : bool =
  if i = 0
  then not (intersects_init p s) (* level 0 = init; if s hits init, cex found *)
  else (
    (* Is there a predecessor of s in frame i-1?  R_{i-1}(x) /\ T(x,y) /\ s(y) *)
    let rec loop () =
      check_budget ();
      let r, sess =
        solve_exprs
          p.ts
          (frame_formula p (i - 1) @ [ p.ts.trans; cube_expr ~prime:true s ])
      in
      match r with
      | R_unknown -> raise (Give_up "predecessor query unknown")
      | R_unsat ->
        (* No predecessor: s is unreachable at level i. Prefer a verified Farkas
           interpolant (relational/general-linear); fall back to template generalization. *)
        let g =
          if s = []
          then s
          else (
            match if interp_on then interpolant_lemma p i s else None with
            | Some interp when admissible p i interp ->
              incr interp_used;
              interp
            | _ -> generalize p i s)
        in
        add_lemma p i g;
        true
      | R_sat ->
        let pred = model_cube p.ts sess in
        if pred = []
        then
          (* Model pinned no state var: cannot form a predecessor cube; be conservative. *)
          raise (Give_up "empty predecessor cube")
        else if rec_block p pred (i - 1)
        then loop () (* predecessor blocked; re-check for another predecessor *)
        else false (* predecessor reaches init: counterexample *)
    in
    loop ())
;;

(* Propagate clauses forward; return [Some i] if a fixpoint frame is found (R_i =
   R_[{i+1}]), i.e. an inductive invariant. *)
let propagate (p : pdr) : int option =
  let found = ref None in
  let i = ref 1 in
  while !found = None && !i <= p.frontier do
    let level = !i in
    let pushed = ref [] in
    let kept = ref [] in
    List.iter
      (fun s ->
        (* clause ~s at level; does it hold at level+1? R_level /\ T /\ s' unsat *)
        let holds =
          not
            (smt_or_giveup
               (check_exprs
                  p.ts
                  (frame_formula p level @ [ p.ts.trans; cube_expr ~prime:true s ])))
        in
        if holds then pushed := s :: !pushed else kept := s :: !kept)
      p.lemmas.(level);
    p.lemmas.(level) <- !kept;
    List.iter (fun s -> add_lemma p (level + 1) s) !pushed;
    if p.lemmas.(level) = [] && level < p.frontier then found := Some level;
    incr i
  done;
  !found
;;

(* The candidate invariant discovered at fixpoint frame [i]: the conjunction of all lemmas
   at levels >= i. *)
let invariant_exprs (p : pdr) (i : int) : expr list = frame_formula p i

(* ------------------------------------------------------------------ *)
(* BMC: bounded unrolling of the transition system (UNSAFE side). *)
(* ------------------------------------------------------------------ *)

(* Build the unrolling to depth [k]: init at copy 0, trans between consecutive copies, bad
   at copy [k]. Copy [c] uses variable names "s[{c}]_x[{i}]"; aux vars are per-copy
   freshened so different steps do not share the transition relation's auxiliaries. *)
let bmc_at (ts : ts) (k : int) : smt * Session.t =
  incr query_count;
  let sess = Session.create ~max_effort:!effort_cap () in
  let ctx = Session.context sess in
  let vmap = Hashtbl.create 256 in
  let get_sort name =
    match Hashtbl.find_opt ts.sorts name with
    | Some s -> s
    | None -> Sort.int
  in
  let decl name =
    match Hashtbl.find_opt vmap name with
    | Some t -> t
    | None ->
      let sym = Session.declare_const sess name (get_sort name) in
      let t = Context.const ctx sym in
      Hashtbl.replace vmap name t;
      t
  in
  (* Rename canonical x{i}->copy c state, y{i}->copy c+1 state, aux a..->per-copy unique. *)
  let copy_rename c name =
    if String.length name >= 1 && name.[0] = 'x'
    then Printf.sprintf "s%d_%s" c name
    else if String.length name >= 1 && name.[0] = 'y'
    then Printf.sprintf "s%d_x%s" (c + 1) (String.sub name 1 (String.length name - 1))
    else Printf.sprintf "b%d_%s" c name (* aux var, per-copy unique *)
  in
  (* Register sorts for the copy-renamed names as we go, then build. *)
  let register e c =
    SS.iter
      (fun name ->
        (* recover the sort of the original canonical/aux name *)
        let orig =
          if String.length name > 2 && name.[0] = 's'
          then (
            (* s{c}_x{i} -> x{i} *)
            match String.index_opt name '_' with
            | Some idx -> String.sub name (idx + 1) (String.length name - idx - 1)
            | None -> name)
          else if String.length name > 2 && name.[0] = 'b'
          then (
            match String.index_opt name '_' with
            | Some idx -> String.sub name (idx + 1) (String.length name - idx - 1)
            | None -> name)
          else name
        in
        ignore c;
        if not (Hashtbl.mem ts.sorts name)
        then Hashtbl.replace ts.sorts name (get_sort orig))
      (free_vars_acc SS.empty e)
  in
  let assert_at c e =
    let e' = rename (copy_rename c) e in
    register e' c;
    let venv name = decl name in
    Session.assert_term sess (Chc_ast.build ctx venv e')
  in
  (* Term construction runs OUTSIDE the Session firewall; degrade any ill-sorted /
     unsupported / overflow build on this unrolling to [R_unknown] rather than crashing
     (mirrors {!solve_exprs}) — e.g. a nonlinear single-predicate transition reaches BMC
     and would otherwise raise out of the process. *)
  let r =
    match
      assert_at 0 ts.init;
      for c = 0 to k - 1 do
        assert_at c ts.trans
      done;
      assert_at k ts.bad
    with
    | () ->
      (match Session.check_sat sess with
       | Session.Sat -> R_sat
       | Session.Unsat -> R_unsat
       | Session.Unknown -> R_unknown)
    | exception Oxsmt_core.Term.Sort_error _ -> R_unknown
    | exception Oxsmt_core.Term.Unsupported _ -> R_unknown
    | exception Oxsmt_core.Term.Overflow -> R_unknown
    | exception Chc_ast.Build_error _ -> R_unknown
  in
  r, sess
;;

(* Run BMC up to [max_depth]; [Some k] iff unsafe at depth k (sat), [None] if all depths
   0..max_depth are unsat (bounded-safe), raising {!Give_up} on an unknown query. *)
let bmc (ts : ts) ~(max_depth : int) : int option =
  let rec go k =
    if k > max_depth
    then None
    else (
      match fst (bmc_at ts k) with
      | R_sat -> Some k
      | R_unsat -> go (k + 1)
      | R_unknown -> raise (Give_up "BMC query unknown"))
  in
  go 0
;;

(* ------------------------------------------------------------------ *)
(* Independent verification of a candidate inductive invariant. *)
(* ------------------------------------------------------------------ *)

(* Confirm [inv] is a genuine safe inductive invariant: init => inv, inv /\ T => inv', inv
   /\ bad unsat. Returns [true] only if all three discharge. *)
let verify_invariant (ts : ts) (inv : expr list) : bool =
  let prime_inv =
    List.map
      (rename (fun x ->
         if String.length x >= 1 && x.[0] = 'x'
         then yname (int_of_string (String.sub x 1 (String.length x - 1)))
         else x))
      inv
  in
  (* init => inv : init /\ ~inv unsat, i.e. for each conjunct c, init /\ ~c unsat *)
  let init_implies =
    List.for_all (fun c -> check_exprs ts [ ts.init; Not c ] = R_unsat) inv
  in
  (* inv /\ T => inv' : inv /\ T /\ ~inv' unsat *)
  let consec =
    List.for_all
      (fun c' -> check_exprs ts (inv @ [ ts.trans; Not c' ]) = R_unsat)
      prime_inv
  in
  let safe = check_exprs ts (inv @ [ ts.bad ]) = R_unsat in
  init_implies && consec && safe
;;

(* ------------------------------------------------------------------ *)
(* Modular-residue invariant template (task #88 lever, unblocked by the #128 gcd cut). *)
(* ------------------------------------------------------------------ *)

(* Modular-congruence template is ON by default; [OXSMT_CHC_MODULAR=0] disables it. *)
let modular_on =
  match Sys.getenv_opt "OXSMT_CHC_MODULAR" with
  | Some ("0" | "false" | "no" | "off") -> false
  | _ -> true
;;

(* Read the Int-valued constants of a Sat model into a name -> value table. *)
let model_ints (sess : Session.t) : (string, Bigint.t) Hashtbl.t =
  let tbl = Hashtbl.create 16 in
  (match Session.get_model sess with
   | Some (_, bindings) ->
     List.iter
       (function
         | Session.Const (nm, Session.VInt n) -> Hashtbl.replace tbl nm n
         | _ -> ())
       bindings
   | None -> ());
  tbl
;;

(* Normalize [v mod k] into the residue range [0, k) (k > 0). *)
let residue (v : Bigint.t) (k : Bigint.t) : Bigint.t =
  let _, m = Bigint.divmod v k in
  if Bigint.sign m < 0 then Bigint.add m k else m
;;

(* Attempt a MODULAR-RESIDUE inductive invariant [⋀_i x_i ≡ r_i (mod k_i)] — the class
   neither interval nor difference-bound generalization can express (e.g. "x is even").
   The moduli/residues are GUESSED by sampling an init state and one successor (per int
   var, [k_i] = |successor − init| is the observed step, [r_i = init mod k_i]); the guess
   is then discharged by the standard three inductiveness obligations, each on a fresh
   Session, so a wrong guess only ever returns [false] (SOUND — the invariant is checked,
   never trusted).

   Congruences are encoded MOD-FREE so the reserved [mod]/[div] symbols never reach the
   oracle: a positive [x ≡ r (mod k)] as [x = k*q + r] (fresh [q]); its negation as
   [x = k*q + s ∧ 0 ≤ s < k ∧ s ≠ r]. Every obligation reduces to a quantifier-free linear
   query — a parity/lattice infeasibility (e.g. [2q = 2q'+1]) that the #128 gcd cut
   (enabled process-wide by the CLI) discharges instead of diverging. Returns [true] iff a
   nonempty congruence invariant verifies. *)
(* Does this system involve modular reasoning? The parser's mod/div elimination
   ({!Chc_ast.elim_moddiv_clause}) introduces fresh quotient/remainder variables prefixed
   ["chcmd_"] (further renamed with a per-clause prefix by {!extract_ts}), so their
   presence in the registered sorts is the signature that the ORIGINAL problem used
   [mod]/[div] — the only place a modular-residue invariant is ever needed. Gating on it
   keeps the modular template OFF for the vast majority of files, where a spurious
   congruence guess (e.g. [x ≡ 2 (mod 3)] for a plain interval system) would produce a
   verification query whose LIA search diverges in propagations (unbounded by the
   conflict/decision effort cap). *)
let has_moddiv (ts : ts) : bool =
  Hashtbl.fold
    (fun name _ acc ->
      acc
      ||
      let re = "chcmd_" in
      let n = String.length name
      and m = String.length re in
      let rec scan i = i + m <= n && (String.sub name i m = re || scan (i + 1)) in
      scan 0)
    ts.sorts
    false
;;

let modular_invariant_inner (ts : ts) : bool =
  if not (has_moddiv ts)
  then false
  else (
    let int_idxs =
      List.filter
        (fun i ->
          match Hashtbl.find_opt ts.sorts (xname i) with
          | Some s -> Sort.equal s Sort.int
          | None -> false)
        (List.init ts.arity (fun i -> i))
    in
    if int_idxs = []
    then false
    else (
      match solve_exprs ts [ ts.init ], solve_exprs ts [ ts.init; ts.trans ] with
      | (R_sat, s0), (R_sat, s1) ->
        let m0 = model_ints s0
        and m1 = model_ints s1 in
        (* (idx, modulus k, residue r) for each int var with an observed step > 1. *)
        let congs =
          List.filter_map
            (fun i ->
              match Hashtbl.find_opt m0 (xname i), Hashtbl.find_opt m1 (yname i) with
              | Some v0, Some v1 ->
                let k = Bigint.abs (Bigint.sub v1 v0) in
                (* step 0 gives no congruence; step 1 is the trivial [mod 1] tautology. *)
                if Bigint.is_zero k || Bigint.equal k Bigint.one
                then None
                else Some (i, k, residue v0 k)
              | _ -> None)
            int_idxs
        in
        if congs = []
        then false
        else (
          let pos ~name ~tag (i, k, r) =
            Eq
              ( Var (name i)
              , Add
                  [ Mul (Int_lit k, Var (Printf.sprintf "modq_%s_%d" tag i)); Int_lit r ]
              )
          in
          let neg ~name ~tag (i, k, r) =
            let s = Printf.sprintf "modns_%s_%d" tag i in
            And
              [ Eq
                  ( Var (name i)
                  , Add
                      [ Mul (Int_lit k, Var (Printf.sprintf "modnq_%s_%d" tag i)); Var s ]
                  )
              ; Ge (Var s, Int_lit Bigint.zero)
              ; Lt (Var s, Int_lit k)
              ; Not (Eq (Var s, Int_lit r))
              ]
          in
          let pos_all = List.map (pos ~name:xname ~tag:"c") congs in
          (* (1) init ⊨ inv : init ∧ ¬(x_i ≡ r_i) unsat, per congruence. *)
          let init_ok =
            List.for_all
              (fun c ->
                check_exprs ts [ ts.init; neg ~name:xname ~tag:"init" c ] = R_unsat)
              congs
          in
          (* (2) inv ∧ T ⊨ inv' : inv ∧ T ∧ ¬(y_i ≡ r_i) unsat, per congruence. *)
          let consec_ok =
            init_ok
            && List.for_all
                 (fun c ->
                   check_exprs ts (pos_all @ [ ts.trans; neg ~name:yname ~tag:"cons" c ])
                   = R_unsat)
                 congs
          in
          (* (3) inv ∧ bad unsat. *)
          let safe_ok = consec_ok && check_exprs ts (pos_all @ [ ts.bad ]) = R_unsat in
          init_ok && consec_ok && safe_ok)
      | _ -> false))
;;

(* The modular attempt runs on EVERY single-predicate solve before PDR, so it must be
   cheap: it caps the per-query effort tightly. A genuine congruence obligation discharges
   in ~1 conflict (the gcd cut fires immediately); a non-modular guess whose obligation
   the raw LIA search cannot close fast then bails to [Unknown] (→ [false]) instead of
   burning the wall — the failure mode that regressed non-modular files at the full 1M
   cap. *)
let modular_effort_cap = 20_000

let modular_invariant (ts : ts) : bool =
  let saved = !effort_cap in
  effort_cap := min saved modular_effort_cap;
  Fun.protect
    ~finally:(fun () -> effort_cap := saved)
    (fun () ->
      try modular_invariant_inner ts with
      | _ -> false)
;;

(* ------------------------------------------------------------------ *)
(* Top-level solve. *)
(* ------------------------------------------------------------------ *)

let default_budget = 200_000

let solve
  ?(max_bmc = 40)
  ?(max_frames = 60)
  ?(budget = default_budget)
  ?(max_effort = 1_000_000)
  (sys : system)
  : result
  =
  query_count := 0;
  reset_interp_stats ();
  budget_ref := budget;
  effort_cap := max_effort;
  let over_budget () = !query_count > budget in
  match extract_ts sys with
  | exception Not_ts reason ->
    { verdict = Unknown ("not a single-predicate transition system: " ^ reason)
    ; detail = reason
    }
  | ts ->
    (try
       (* Depth-0 trivial unsafety: UNSAFE if ANY fact-free "constr => false" body has a
          satisfiable constraint (accumulator — every such query is checked). *)
       if List.exists (fun c -> check_exprs ts [ c ] = R_sat) ts.trivially_unsafe
       then raise (Give_up "__unsafe_trivial");
       (* Depth-0: init /\ bad. *)
       if check_exprs ts [ ts.init; ts.bad ] = R_sat then raise (Give_up "__unsafe_0");
       (* Modular-residue invariant (independently verified): a cheap early SAFE for the
          "x is even"-style invariants PDR's numeric templates cannot reach. *)
       if modular_on && modular_invariant ts then raise (Give_up "__safe_modular");
       (* PDR frame loop with interleaved BMC confirmation of any counterexample. *)
       let p = { ts; frontier = 1; lemmas = Array.make 4 [] } in
       let result = ref None in
       while !result = None do
         if over_budget () then raise (Give_up "effort budget exhausted");
         if p.frontier > max_frames then raise (Give_up "frame limit reached");
         ensure_frontier p (p.frontier + 1);
         (* Block all bad states reachable at the frontier. *)
         let blocked_ok = ref true in
         (try
            let continue = ref true in
            while !continue do
              let r, sess =
                solve_exprs p.ts (frame_formula p p.frontier @ [ p.ts.bad ])
              in
              match r with
              | R_unknown -> raise (Give_up "frontier bad-query unknown")
              | R_unsat -> continue := false
              | R_sat ->
                let s = model_cube p.ts sess in
                if s = [] then raise (Give_up "empty bad cube");
                if not (rec_block p s p.frontier)
                then (
                  blocked_ok := false;
                  continue := false);
                if over_budget () then raise (Give_up "effort budget exhausted")
            done
          with
          | Give_up "__cex" -> blocked_ok := false);
         if not !blocked_ok
         then (
           (* PDR found a counterexample chain: confirm independently by BMC. *)
           match bmc ts ~max_depth:(p.frontier + 2) with
           | Some _ -> result := Some Unsafe
           | None -> raise (Give_up "PDR cex not confirmed by BMC"))
         else (
           (* Propagate; a fixpoint frame is an inductive invariant. *)
           match propagate p with
           | Some i ->
             let inv = invariant_exprs p i in
             if verify_invariant ts inv
             then result := Some Safe
             else raise (Give_up "candidate invariant failed independent verification")
           | None -> p.frontier <- p.frontier + 1)
       done;
       match !result with
       | Some Safe -> { verdict = Safe; detail = "PDR inductive invariant, verified" }
       | Some Unsafe ->
         { verdict = Unsafe; detail = "PDR + BMC-confirmed counterexample" }
       | Some (Unknown m) -> { verdict = Unknown m; detail = m }
       | None -> { verdict = Unknown "no result"; detail = "" }
     with
     | Give_up "__safe_modular" ->
       { verdict = Safe; detail = "modular-residue inductive invariant, verified" }
     | Give_up "__unsafe_trivial" ->
       { verdict = Unsafe; detail = "trivial (constraint-only) counterexample" }
     | Give_up "__unsafe_0" ->
       (match bmc ts ~max_depth:0 with
        | Some _ ->
          { verdict = Unsafe; detail = "BMC-confirmed counterexample at depth 0" }
        | None -> { verdict = Unknown "depth-0 unsafe not confirmed"; detail = "" })
     | Give_up reason ->
       (* Before giving up to unknown, make a bounded BMC attempt for unsafety. *)
       (match
          try bmc ts ~max_depth:max_bmc with
          | Give_up _ -> None
        with
        | Some k ->
          { verdict = Unsafe
          ; detail = Printf.sprintf "BMC-confirmed counterexample at depth %d" k
          }
        | None -> { verdict = Unknown reason; detail = reason })
     | Chc_ast.Build_error m -> { verdict = Unknown ("build: " ^ m); detail = m })
;;

let verdict_to_smtlib = function
  | Safe -> "sat"
  | Unsafe -> "unsat"
  | Unknown _ -> "unknown"
;;
