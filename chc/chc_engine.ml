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
module Context = Oxsmt_core.Context
module Sort = Oxsmt_core.Sort
module Bigint = Oxsmt_core.Bigint
module Term = Oxsmt_core.Term
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

(* A cube literal. [jdx = None] is a single-variable literal [x_idx rel v]; [jdx = Some j]
   is a DIFFERENCE (octagon-style) literal [(x_idx - x_j) rel v], the template that lets
   generalization express a RELATIONAL invariant (e.g. [x - y = 0], i.e. [x = y]) — the
   class one-sided single-variable interval widening cannot reach, and the proxy for true
   Farkas interpolants (the public Session API exposes no theory unsat-core / LP dual, so
   real interpolant extraction needs a main-solver API addition; see logs report). *)
type lit =
  { idx : int
  ; jdx : int option
  ; rel : rel
  ; v : value
  }

(* A cube is a conjunction of literals over the state variables. *)
type cube = lit list

type ts =
  { arity : int
  ; sorts : (string, Sort.t) Hashtbl.t (* every canonical + aux var name -> sort *)
  ; init : expr (* over x0..x{n-1} *)
  ; trans : expr (* over x0..x{n-1} (pre) and y0..y{n-1} (post) *)
  ; bad : expr (* over x0..x{n-1} *)
  ; trivially_unsafe : expr option (* a fact-free "constr => false" body, if any *)
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
    let triv = ref None in
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
          (* A fact-free "constr => false": the system is unsatisfiable iff [constr] is
             satisfiable (a derivation of false using no predicate). *)
          triv := Some (And constr)
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
    ; trivially_unsafe = !triv
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
           | Session.VUninterp _ -> ())
        | Session.Fun _ -> ())
      bindings;
    (* Single-variable literals x_i = v_i (present in the model). *)
    let ints = ref [] in
    let acc = ref [] in
    for i = ts.arity - 1 downto 0 do
      match Hashtbl.find_opt tbl (xname i) with
      | Some v ->
        acc := { idx = i; jdx = None; rel = Req; v } :: !acc;
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
              := { idx = i; jdx = Some j; rel = Req; v = VInt (Bigint.sub vi vj) }
                 :: !diffs)
          !ints)
      !ints;
    !acc @ !diffs
;;

(* ------------------------------------------------------------------ *)
(* Cube / lemma expression construction. *)
(* ------------------------------------------------------------------ *)

let lit_expr ~prime (l : lit) : expr =
  let nm k = if prime then yname k else xname k in
  (* term the relation constrains: x_i, or (x_i - x_j) for a difference literal *)
  let lhs =
    match l.jdx with
    | None -> Var (nm l.idx)
    | Some j -> Sub [ Var (nm l.idx); Var (nm j) ]
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
        (* No predecessor: s is unreachable at level i. Generalize and record. *)
        let g = if s = [] then s else generalize p i s in
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
  assert_at 0 ts.init;
  for c = 0 to k - 1 do
    assert_at c ts.trans
  done;
  assert_at k ts.bad;
  let r =
    match Session.check_sat sess with
    | Session.Sat -> R_sat
    | Session.Unsat -> R_unsat
    | Session.Unknown -> R_unknown
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
       (* Depth-0 trivial unsafety: a fact-free "constr => false" with satisfiable constr. *)
       (match ts.trivially_unsafe with
        | Some c when check_exprs ts [ c ] = R_sat -> raise (Give_up "__unsafe_trivial")
        | _ -> ());
       (* Depth-0: init /\ bad. *)
       if check_exprs ts [ ts.init; ts.bad ] = R_sat then raise (Give_up "__unsafe_0");
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
