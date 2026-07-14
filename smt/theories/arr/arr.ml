(* The arrays theory as an e-graph client. See arr.mli for the contract.

   The theory owns a [prem Euf.t] engine: select/store are App nodes, so the engine gives
   congruence closure over them for free. On top of that substrate this module enforces
   the read-over-write and extensionality axioms. The select/store symbol classification
   (which App head is a select vs a store, over which index/element sorts) is read from
   {!Oxsmt_core.Array_defs}; the theory grows its own copy when it mints a fresh [select]
   for a ROW step.

   THEORY boilerplate (atom<->term bookkeeping, watches, propagation-reason caching,
   pop-frame reason dropping) mirrors {!Oxsmt_dt.Dt} / Euf_adapter. *)

open Oxsmt_core
module Euf = Oxsmt_euf.Euf
module Defs = Array_defs

(* Weak-equivalence decision procedure (ADR-weakeq / DESIGN.md A12), gated OXSMT_ARR_WEQ,
   default OFF. W0 (this stage) is DARK: it maintains the {!Weq_graph} off the Euf merge
   stream and (under OXSMT_ARR_WEQ_SELFCHECK) cross-checks its connectivity against the
   engine, but emits NO lemmas and changes NO verdict — with the gate unset AND set the
   solver is byte-identical. The L1/L2 generators that consume the graph land at W1/W2. *)
let env_flag name =
  match Sys.getenv_opt name with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false
;;

(* W0.5 (ADR-weakeq §2a): the DARK store-chain analyzer — the storecomm arbiter. Read-only
   (no lemmas, no verdict change): at Final it takes each asserted array diseq's canonical
   L2 path and symbolically closes every path-index read down both store chains using ONLY
   ROW1 + entailed off-diagonal diseqs, counting the UNRESOLVED index tests. Green-lights
   the W2-storecomm expectation only if ~all paths close with zero open tests (linear
   closure). Implies the graph is on. *)
let weq_analyze = env_flag "OXSMT_ARR_WEQ_ANALYZE"

(* The graph is maintained whenever the decision procedure is enabled OR the dark analyzer
   needs it. OXSMT_ARR_WEQ turns on the W1 L1 read-over-weak-equivalence rule (added to
   [row_split], not yet replacing it). *)
let weq_on = env_flag "OXSMT_ARR_WEQ" || weq_analyze
let weq_l1_on = env_flag "OXSMT_ARR_WEQ"
let weq_selfcheck = env_flag "OXSMT_ARR_WEQ_SELFCHECK"

(* O7 fuel: a hard cap on L1 clause emissions per solve. On exhaustion L1 stops emitting
   and the sound [row_split] backstop continues (W1), so a pathological input degrades to
   the engine's split budget (-> unknown), never a wrong verdict. Deterministic and load-
   independent. *)
let weq_fuel_cap =
  match Sys.getenv_opt "OXSMT_ARR_WEQ_FUEL" with
  | Some s ->
    (try int_of_string s with
     | _ -> 200_000)
  | None -> 200_000
;;

(* TEST-ONLY (default OFF): drop the O1' class-collapse guards from the emitted L1 clause,
   turning the sound rule into the deliberately-WRONG pre-erratum variant. Exists solely
   so the RED tests can verify BY EXECUTION that each guard is load-bearing (a guarded run
   is sound where this unguarded run wrong-refutes). NEVER set in production; the shipped
   default emits the fully-guarded tautology. *)
let weq_unsafe_noguard = env_flag "OXSMT_ARR_WEQ_UNSAFE_NOGUARD"

(* The engine's ['p]. A real assertion carries [P_lit]; the standing [true <> false]
   disequality carries [P_axiom]; a ROW-derived equality carries [P_derived reasons] — the
   trail literals whose conjunction entails the trigger. [P_axiom] is dropped when
   building an Explanation so a returned premise set holds only asserted literals. *)
type prem =
  | P_lit of Lit.t
  | P_axiom
  | P_derived of Lit.t list

type kind =
  | K_eq of Term.t * Term.t
  | K_bool
  | K_foreign

type info =
  { term : Term.t
  ; kind : kind
  }

type t =
  { ctx : Context.t
  ; env : Env.t
  ; cap : Env.reserved_cap
    (* ADR-0012 R1 reserved-minting capability for [env], threaded from the session (the
         sole holder). The arrays theory is a legitimate reserved-symbol minter — like
         preprocessing and the lemma tier — because its extensionality rule introduces
         FRESH witness indices mid-solve, which MUST be unforgeable (see [witness_index]). *)
  ; mutable reg : Defs.t (* select/store classification; grows as we mint fresh selects *)
  ; engine : prem Euf.t
  ; true_const : Term.t
  ; false_const : Term.t
  ; atoms : info Atom.Table.t
  ; watched : Atom.t Term.Table.t (* watched Eq/predicate term -> atom, for propagation *)
  ; mutable atom_terms : Term.t list (* registration order; model enumeration *)
  ; seen_cat : unit Term.Table.t
  ; mutable store_terms : Term.t list (* registered store applications, rev order *)
  ; mutable select_terms : Term.t list (* registered select applications, rev order *)
  ; select_syms :
      (string, Symbol.t) Hashtbl.t (* minted select symbols, by canonical name *)
  ; ensured_reads : (int * int, unit) Hashtbl.t
    (* (store term tag, index term tag) pairs for which [select store index] has already
         been materialized by the upward read-propagation rule ([ensure_store_reads]).
         Guards termination: each (store, index) pair triggers at most one fresh select,
         and a fresh select's index is drawn from an existing select, so no new index is
         ever created. NOT backtracked on pop, and that is safe — NOT because e-graph
         nodes persist (they do not: [Euf.pop] truncates enodes to the push watermark),
         but because [select_terms]/[store_terms] are themselves monotonic ([Arr.pop]
         never truncates them) and every ROW consumer reaches a term through
         [Euf.are_equal]/[Euf.class_of], which lazily re-register an enode truncated by a
         pop. So a memoized pair whose select was dropped by [Euf.pop] is simply
         re-materialized on demand the next time it is read; the memo only ever suppresses
         a redundant [build_select], never a needed one. *)
  ; ext_witness : (int * int, Term.t) Hashtbl.t
    (* fresh extensionality witness index per asserted array-diseq pair (by term tags,
         orientation-normalized), reused so re-assertion after a pop is stable *)
  ; mutable fresh_counter : int
  ; mutable explain_cache : Explanation.t Lit.Map.t
  ; mutable frames : Lit.t list list
  ; weq : (Weq_graph.t * Euf.merge_cursor) option
    (* the dark weak-equivalence graph + its merge cursor, [Some] iff OXSMT_ARR_WEQ
         (W0). [None] keeps the OFF path byte-identical (no merge recording, no graph). *)
  ; mutable an_diseqs : (Term.t * Term.t) list
    (* Asserted disequalities (both array and index sorted), appended when a negative
         [Eq] is asserted; populated when the graph is on (W0.5 analyzer reads all of them
         via [an_distinct]; W1 L1 reads the array-sorted ones as its diseq-reachable
         triggers). NOT backtracked: a stale pair only risks re-attempting an L1 whose
         emission is memoized ([weq_emitted]) and whose clause is an unconditional
         term-level tautology (sound regardless of whether the diseq is still asserted);
         the analyzer likewise only over-counts a candidate. Empty unless the graph is on. *)
  ; mutable an_done : bool (* W0.5 analyzer: emit the per-solve summary at most once *)
  ; weq_emitted : (int * int, unit) Hashtbl.t
    (* W1 L1: identity of already-emitted read-over-weakeq clauses, keyed by the
         orientation-normalized (read1 tag, read2 tag) pair. PERMANENT (not trailed): an
         L1 clause is a permanent term-level tautology in the SAT core, so re-emitting it
         after a backtrack would prevent ever reaching Sat — the memo suppresses that
         forever (obligation 1: trigger dedup + bounded re-emission across backtrack;
         termination, not soundness). *)
  ; mutable weq_fuel : int (* W1 L1: remaining emission fuel this solve (O7) *)
  }

let max_iters = 1_000_000

let rec lits_of_prem = function
  | P_lit l -> [ l ]
  | P_axiom -> []
  | P_derived ls -> ls

and lits_of_prems ps = List.concat_map lits_of_prem ps

let dedup_lits (ls : Lit.t list) : Lit.t list =
  let seen = ref Lit.Map.empty in
  List.filter
    (fun l ->
       if Lit.Map.mem l !seen
       then false
       else (
         seen := Lit.Map.add l () !seen;
         true))
    ls
;;

(* The abstract e-graph view (O6) the weak-equivalence graph reads through — standalone,
   it closes over this theory's private Euf engine; on the fabric hub (W3) it is re-bound. *)
let weq_view engine : Weq_graph.egraph_view =
  { Weq_graph.class_of = (fun tm -> Euf.class_of engine tm)
  ; are_equal = (fun a b -> Euf.are_equal engine a b)
  ; explain_equal =
      (fun a b -> dedup_lits (lits_of_prems (Euf.explain engine a b)))
      (* premise literals entailing [a = b]; dark in W0, consumed by the W1 guards *)
  }
;;

let create ctx env cap reg =
  let engine = Euf.create ctx in
  let true_const = Context.bool_const ctx true in
  let false_const = Context.bool_const ctx false in
  Euf.register_term engine true_const;
  Euf.register_term engine false_const;
  Euf.assert_neq engine ~premise:P_axiom true_const false_const;
  let weq =
    if weq_on
    then (
      Euf.set_record_merges engine true;
      Some (Weq_graph.create (weq_view engine), Euf.add_merge_consumer engine))
    else None
  in
  { ctx
  ; env
  ; cap
  ; reg
  ; engine
  ; true_const
  ; false_const
  ; atoms = Atom.Table.create 64
  ; watched = Term.Table.create 64
  ; atom_terms = []
  ; seen_cat = Term.Table.create 128
  ; store_terms = []
  ; select_terms = []
  ; select_syms = Hashtbl.create 16
  ; ensured_reads = Hashtbl.create 64
  ; ext_witness = Hashtbl.create 32
  ; fresh_counter = 0
  ; explain_cache = Lit.Map.empty
  ; frames = [ [] ]
  ; weq
  ; an_diseqs = []
  ; an_done = false
  ; weq_emitted = Hashtbl.create 64
  ; weq_fuel = weq_fuel_cap
  }
;;

(* --- small helpers --- *)

let head_args (term : Term.t) : (Symbol.t * Term.t array) option =
  match term.Term.node with
  | Term.App (s, args) -> Some (s, Array.of_list (Iarr.to_list args))
  | _ -> None
;;

let role_of t (term : Term.t) : Defs.entry option =
  match term.Term.node with
  | Term.App (sym, _) -> Defs.role_of_sym t.reg sym
  | _ -> None
;;

let array_sort (sort : Sort.t) : (Sort.t * Sort.t) option =
  match sort with
  | Sort.Array (i, e) -> Some (i, e)
  | Sort.Bool | Sort.Int _ | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.BitVec _ ->
    None
;;

(* --- minting fresh terms (select for a ROW step, a witness index for extensionality) --- *)

(* Get-or-mint the [select] symbol for one (index, element) instantiation. Uses the SAME
   canonical name the parser uses ({!Array_defs.op_symbol_name}), so a fresh [select] the
   theory builds hash-conses with any [select] already in the problem; records it in [reg]
   so [role_of] classifies it. *)
let select_sym t ~index ~element : Symbol.t =
  let name = Defs.op_symbol_name Defs.Select ~index ~element in
  match Hashtbl.find_opt t.select_syms name with
  | Some sym -> sym
  | None ->
    let arr = Sort.array_ ~index ~element in
    (* board #58: the op name is a reserved [.oxsmt.arr.*] symbol with [|] sort-key
       separators, so it must be minted through the cap door (the same [cap] the
       ext-witness Skolem uses), not [Env.declare_fun] — the public door rejects both the
       prefix and the byte class. *)
    let sym =
      Env.declare_reserved t.cap t.env name (Rank.create [ arr; index ] element)
    in
    Hashtbl.replace t.select_syms name sym;
    t.reg <- Defs.add t.reg sym Defs.Select ~index ~element;
    sym
;;

(* --- registration: catalog select/store while registering into the e-graph --- *)

let rec catalog t (term : Term.t) =
  if not (Term.Table.mem t.seen_cat term)
  then (
    Term.Table.replace t.seen_cat term ();
    match head_args term with
    | Some (_, args) ->
      (match role_of t term with
       | Some { Defs.role = Defs.Store; _ } ->
         t.store_terms <- term :: t.store_terms;
         (* record the permanent store edge in the weak-equivalence graph (W0, dark). The
            [Array.length args = 3] guard is load-bearing exactly as in the ROW consuming
            sites: a store-role symbol can be registered at a non-3 arity via the public
            API ([[arr-arity-guard-load-bearing]]), and indexing args.(0)/args.(1)
            unguarded would forge an edge over an extended-arity uninterpreted function. *)
         (match t.weq with
          | Some (g, _) when Array.length args = 3 ->
            Weq_graph.add_store_edge g ~store_term:term ~base:args.(0) ~index:args.(1)
          | _ -> ())
       | Some { Defs.role = Defs.Select; _ } -> t.select_terms <- term :: t.select_terms
       | None -> ());
      Array.iter (catalog t) args
    | None ->
      (match term.Term.node with
       | Term.Eq (a, b) ->
         catalog t a;
         catalog t b
       | Term.Not a | Term.Le a -> catalog t a
       | Term.And xs | Term.Or xs -> List.iter (catalog t) (Iarr.to_list xs)
       | Term.Ite (a, b, c) ->
         catalog t a;
         catalog t b;
         catalog t c
       | Term.Arith lin ->
         List.iter (fun (c, _) -> catalog t c) (Iarr.to_list lin.Term.coeffs)
       | Term.Bool_const _ | Term.Int_const _ -> ()
       | Term.App _ -> ()))
;;

(* [build_select t base j] is the (registered, cataloged) term [select base j] for a ROW
   step, where [base] has an array sort. *)
let build_select t (base : Term.t) (j : Term.t) : Term.t option =
  match array_sort base.Term.sort with
  | None -> None
  | Some (index, element) ->
    let sym = select_sym t ~index ~element in
    let sel = Context.app t.ctx sym [ base; j ] in
    Euf.register_term t.engine sel;
    catalog t sel;
    Some sel
;;

let classify (term : Term.t) : kind =
  if not (Theory_view.is_atom term)
  then invalid_arg "Arr.register_atom: term is not a theory atom";
  match Theory_view.atom term with
  | Theory_view.Equality (a, b) -> K_eq (a, b)
  | Theory_view.Predicate (_, _) | Theory_view.Bool_lit _ -> K_bool
  | Theory_view.Le_zero _ -> K_foreign
;;

let register_atom t atom term =
  Euf.register_term t.engine term;
  catalog t term;
  if not (Atom.Table.mem t.atoms atom)
  then (
    let kind = classify term in
    Atom.Table.replace t.atoms atom { term; kind };
    t.atom_terms <- term :: t.atom_terms;
    match kind with
    | K_eq _ -> Term.Table.replace t.watched term atom
    | K_bool ->
      (match term.Term.node with
       | Term.App (_, args) when Iarr.length args >= 1 ->
         Term.Table.replace t.watched term atom;
         Euf.rearm_watch t.engine term
       | _ -> ())
    | K_foreign -> ())
;;

(* --- extensionality: a <> b (arrays) => fresh k, select a k <> select b k --- *)

let witness_index t (a : Term.t) (b : Term.t) (index : Sort.t) : Term.t =
  let ta = a.Term.tag
  and tb = b.Term.tag in
  let key = if ta <= tb then ta, tb else tb, ta in
  match Hashtbl.find_opt t.ext_witness key with
  | Some w -> w
  | None ->
    (* A fresh nullary index constant for the extensionality Skolem. It MUST be
       unforgeable: if the same symbol could be named elsewhere, an equality asserted at
       this very index would turn our [select a k <> select b k] into a false conflict — a
       wrong-UNSAT. The reserved [".oxsmt.arr.ext."] namespace closes the EXTERNAL door
       (parsed input / public [declare_fun] / raw [Env] can never produce a [.oxsmt.*]
       symbol with a rank), NOT lexical illegality — [@arr.ext.N] would have been
       forgeable since [@]/[.] are legal simple-symbol characters.

       board #58 (codex critical, freshness by CONSTRUCTION not by counter trust): the
       minting door [Session.internal_minter] is PUBLIC, so a trusted in-process caller
       could pre-mint [.oxsmt.arr.ext.N] (giving that name a rank) and then our counter,
       trusting itself, would hand the extensionality rule the CALLER's symbol -> capture
       -> wrong verdict. So advance past any index whose name already carries a rank in
       [env] (i.e. is already declared/minted), and only then mint. This makes the witness
       fresh against the actual env state, not merely against our own counter. The counter
       also keeps distinct diseq pairs on distinct witnesses. *)
    let rec fresh_name () =
      let name = Printf.sprintf ".oxsmt.arr.ext.%d" t.fresh_counter in
      t.fresh_counter <- t.fresh_counter + 1;
      match Env.rank t.env (Symbol.intern name) with
      | (_ : Rank.t) -> fresh_name () (* already declared/minted — skip it *)
      | exception Not_found -> name
    in
    let name = fresh_name () in
    let sym = Env.declare_reserved t.cap t.env name (Rank.create [] index) in
    let w = Context.const t.ctx sym in
    Euf.register_term t.engine w;
    Hashtbl.replace t.ext_witness key w;
    w
;;

let extensionality t lit (a : Term.t) (b : Term.t) =
  match array_sort a.Term.sort with
  | None -> ()
  | Some (index, _element) ->
    let k = witness_index t a b index in
    (match build_select t a k, build_select t b k with
     | Some sa, Some sb -> Euf.assert_neq t.engine ~premise:(P_lit lit) sa sb
     | _ -> ())
;;

let assert_lit t lit =
  let atom = Lit.atom lit in
  let positive = Lit.sign lit in
  match Atom.Table.find_opt t.atoms atom with
  | None -> invalid_arg "Arr.assert_lit: atom was not registered"
  | Some { kind = K_eq (a, b); _ } ->
    if positive
    then Euf.assert_eq t.engine ~premise:(P_lit lit) a b
    else (
      Euf.assert_neq t.engine ~premise:(P_lit lit) a b;
      (* An array disequality demands an extensionality witness: a <> b is satisfiable
         only if a and b differ at some index, so introduce a fresh one. Sound
         (Skolemization); it drives ROW to close read-over-write refutations. *)
      extensionality t lit a b;
      (* Record the asserted diseq (array + index) for the graph-based rules: the W0.5
         analyzer's store-chain closure and the W1 L1 diseq-reachable triggers. *)
      if weq_on then t.an_diseqs <- (a, b) :: t.an_diseqs)
  | Some { kind = K_bool; term } ->
    let target = if positive then t.true_const else t.false_const in
    Euf.assert_eq t.engine ~premise:(P_lit lit) term target
  | Some { kind = K_foreign; _ } ->
    invalid_arg "Arr.assert_lit: a foreign (non-array/EUF) atom must not be asserted"
;;

(* --- read-over-write saturation --- *)

(* Upward read propagation. The read-over-write rules ([row_round] / [row_split]) relate a
   read of a STORE, [select (store base i v) j], to a read of its BASE, [select base j].
   But they only fire once BOTH reads exist as terms, and the only term-introduction they
   do is the base read ([build_select ... base]) given a store read. Nothing introduces
   the store read given only a base read — so a read of an array [a] never reaches the
   stores built over [a]. Two symptoms, one cause:

   - the storeinv shape (store(a,i,select(b,i)) = store(b,i,select(a,i)), a<>b): unsat,
     but the disequality's witness reads [select a k]/[select b k] never reach the two
     stores, so the store equality is never exercised and the engine wrongly saturates
     (unsat -> unknown);
   - satisfiable queries whose model the self-check REJECTS: the engine saturates on an
     under-constrained arrangement (reads on the declared arrays were never propagated up
     to the store terms mentioned by the assertions), so the extracted model does not in
     fact satisfy the assertions.

   This closes both: for every store [st = store base i v] and every read [select arr j]
   whose array [arr] is congruent to the store's [base], materialize [select st j]. The
   existing round/split then relates it to [select base j] (i = j entailed => select st j
   = v, else the guarded [row_split]).

   Cost is controlled by WHEN this runs, not by restricting which indices it fires at:
   [check] calls it only at [Final], once ordinary ROW saturation is otherwise clean and
   quiet (see [final_introduce_reads]). A query that refutes during propagation never
   reaches that point, so its fast refutation is untouched; the reads (and the
   [row_split]s they spawn) are paid for only on a branch that survives to a full,
   otherwise-consistent assignment — exactly where a store equality would otherwise go
   unexercised. Running this eagerly inside [saturate] at every effort instead was
   measured to flood deep store chains (swap/storecomm: ~22 nested stores) with case
   splits and net out to a wash.

   Termination: each (store term, index term) pair fires at most once ([ensured_reads]); a
   fresh read's index [j] is an existing index term, so no new index — hence no new
   (store,index) pair — is ever created. The introduction walks up a store chain across
   rounds: reading a base introduces a read of the store over it, which is itself the base
   of the next store, and so on. *)
(* Per-call index: array e-class id -> the registered selects whose array argument is in
   that class. The ROW passes below relate a select [select arr j] to a store
   [store base i v] exactly when [arr] is congruent to the store's own array ([row_round])
   or to its base ([ensure_store_reads]) — i.e. when [class_of arr] equals [class_of st]
   resp. [class_of base]. Grouping the selects by [class_of arr] once (O(selects)) lets
   each store look up only the relevant selects instead of scanning the full selects x
   stores product, which is the dominant per-decision cost on deep store chains (~130
   selects x ~22 stores). Rebuilt each call because e-classes change as the search merges
   classes. *)
let selects_by_arr_class t : (int, Term.t) Hashtbl.t =
  let idx = Hashtbl.create 64 in
  List.iter
    (fun sel ->
       (* [Iarr.length a = 2] is load-bearing, NOT a redundant sanity check: a select-role
         symbol can be REGISTERED at a non-2 arity via the public API
         (Session.parse_minter admits any canonical [.oxsmt.arr.*] name with a
         caller-supplied rank; Array_defs classifies by name, not rank). The old
         [head_args] match on [[| arr; _j |]] silently skipped such a mis-ranked App;
         without this length guard the ROW rules would treat an extended-arity
         uninterpreted function as a select and derive a wrong verdict. Every consuming
         site below re-checks arity for the same reason. *)
       match sel.Term.node with
       | Term.App (_, a) when Iarr.length a = 2 && role_of t sel <> None ->
         Hashtbl.add idx (Euf.class_of t.engine (Iarr.get a 0)) sel
       | _ -> ())
    t.select_terms;
  idx
;;

let ensure_store_reads t ~changed =
  let idx = selects_by_arr_class t in
  List.iter
    (fun st ->
       match st.Term.node with
       | Term.App (_, a)
         when Iarr.length a = 3
              &&
              match role_of t st with
              | Some { Defs.role = Defs.Store; _ } -> true
              | _ -> false ->
         let base = Iarr.get a 0 in
         (* selects on an array congruent to this store's base — [Euf.are_equal arr base]
           is [class_of arr = class_of base], which is exactly the index bucket. *)
         List.iter
           (fun sel ->
              match sel.Term.node with
              | Term.App (_, sa) when Iarr.length sa = 2 ->
                let j = Iarr.get sa 1 in
                let key = st.Term.tag, j.Term.tag in
                if not (Hashtbl.mem t.ensured_reads key)
                then (
                  Hashtbl.replace t.ensured_reads key ();
                  match build_select t st j with
                  | Some _ -> changed := true
                  | None -> ())
              | _ -> ())
           (Hashtbl.find_all idx (Euf.class_of t.engine base))
       | _ -> ())
    t.store_terms
;;

(* One ROW pass: for each store [st = store base i v] and each select [sel = select arr j]
   with [arr] congruent to [st], propagate the ENTAILED ROW equality. Only the definite
   direction ([i = j] known ⇒ sel = v) is propagated here; the open case is deferred to a
   [Split] at [Final] ([row_split]). Sets [changed] on a propagation. Never reports a
   conflict directly (Euf.check does).

   Iterating stores-outer over the [selects_by_arr_class] index (instead of the old
   selects x stores product) is order-insensitive and fixpoint-preserving: [saturate]
   re-runs to a fixpoint on [changed], so a propagation that a within-pass merge would
   newly enable is caught on the next pass (which rebuilds the index); and a pass that
   propagates nothing did so against a valid index (no merge happened), i.e. it has
   genuinely converged. The set of ROW consequences at the fixpoint is unchanged. *)
let row_round t ~changed =
  let idx = selects_by_arr_class t in
  List.iter
    (fun st ->
       match st.Term.node with
       | Term.App (_, a)
         when Iarr.length a = 3
              &&
              match role_of t st with
              | Some { Defs.role = Defs.Store; _ } -> true
              | _ -> false ->
         let i = Iarr.get a 1
         and v = Iarr.get a 2 in
         List.iter
           (fun sel ->
              match sel.Term.node with
              | Term.App (_, sa) when Iarr.length sa = 2 ->
                let arr = Iarr.get sa 0
                and j = Iarr.get sa 1 in
                (* definite ROW1: i = j entailed ⇒ sel = v *)
                if Euf.are_equal t.engine i j && not (Euf.are_equal t.engine sel v)
                then (
                  let prem =
                    P_derived
                      (dedup_lits
                         (lits_of_prems
                            (Euf.explain t.engine arr st @ Euf.explain t.engine i j)))
                  in
                  Euf.assert_eq t.engine ~premise:prem sel v;
                  changed := true)
              | _ -> ())
           (Hashtbl.find_all idx (Euf.class_of t.engine st))
       | _ -> ())
    t.store_terms
;;

(* Saturate ROW definite propagations to a fixpoint, interleaving [Euf.check] (a merge can
   expose a diseq violation or a new congruence). [Some prems] on the first conflict. *)
let saturate t : prem list option =
  let rec loop n =
    if n > max_iters then failwith "Arr.saturate: fixpoint did not converge (bug)";
    match Euf.check t.engine with
    | Euf.Conflict prems -> Some prems
    | Euf.Consistent ->
      let changed = ref false in
      row_round t ~changed;
      if !changed then loop (n + 1) else None
  in
  loop 0
;;

(* At [Final]: find the tag-least open ROW pair (select arr j, store base i v) with
   [arr ~ st], [i = j] not entailed, and [sel] not already equal to [select base j], and
   return the theory-valid split disjunction. Building [select base j] introduces it into
   the e-graph so the split's second disjunct is a real atom. The disjunction:

   (guard ⇒) i = j ∨ select (store base i v) j = select base j

   where [guard] is [arr <> st] when [arr] is only congruent to (not syntactically) [st]
   (so the clause is unconditionally array-valid). Two/three distinct atoms, not a
   propositional tautology, so the SAT core keeps it. *)
(* Per-call index: array e-class id -> the registered stores in that class. Mirror of
   [selects_by_arr_class] for the store side, so [row_split] can enumerate the stores
   congruent to a select's array ([Euf.are_equal arr st] = [class_of arr = class_of st])
   without scanning every store. *)
let stores_by_class t : (int, Term.t) Hashtbl.t =
  let idx = Hashtbl.create 64 in
  List.iter
    (fun st ->
       match role_of t st with
       | Some { Defs.role = Defs.Store; _ } ->
         Hashtbl.add idx (Euf.class_of t.engine st) st
       | _ -> ())
    t.store_terms;
  idx
;;

let row_split t : Term.t list option =
  let cand = ref None in
  let by_tag a b = compare a.Term.tag b.Term.tag in
  let sidx = stores_by_class t in
  List.iter
    (fun sel ->
       if !cand = None
       then (
         match sel.Term.node with
         | Term.App (_, sa) when Iarr.length sa = 2 && role_of t sel <> None ->
           let arr = Iarr.get sa 0
           and j = Iarr.get sa 1 in
           (* stores congruent to [arr], in tag order — same set and order the old
             full-scan visited, so the tag-least open pair chosen is identical. *)
           let stores =
             List.sort by_tag (Hashtbl.find_all sidx (Euf.class_of t.engine arr))
           in
           List.iter
             (fun st ->
                if !cand = None
                then (
                  match st.Term.node with
                  | Term.App (_, a)
                    when Iarr.length a = 3
                         && not (Euf.are_equal t.engine (Iarr.get a 1) j) ->
                    let base = Iarr.get a 0
                    and i = Iarr.get a 1 in
                    (match build_select t base j with
                     | Some selbase when not (Euf.are_equal t.engine sel selbase) ->
                       let idx_eq = Context.eq t.ctx i j in
                       let read_eq = Context.eq t.ctx sel selbase in
                       let disjuncts =
                         if Term.equal arr st
                         then [ idx_eq; read_eq ]
                         else
                           [ Context.not_ t.ctx (Context.eq t.ctx arr st)
                           ; idx_eq
                           ; read_eq
                           ]
                       in
                       cand := Some disjuncts
                     | _ -> ())
                  | _ -> ()))
             stores
         | _ -> ()))
    (List.sort by_tag t.select_terms);
  !cand
;;

(* --- propagation reason caching (mirrors Dt / Euf_adapter) --- *)

let reason_of_implied t (imp : Euf.implied) : Explanation.t =
  let premises = dedup_lits (lits_of_prems (Euf.explain_implied t.engine imp)) in
  let rule =
    if premises = []
    then Explanation.Rule_tag.Trivial
    else Explanation.Rule_tag.Euf_congruence
  in
  { Explanation.premises; rule }
;;

let cache_reason t lit expl =
  if not (Lit.Map.mem lit t.explain_cache)
  then (
    t.explain_cache <- Lit.Map.add lit expl t.explain_cache;
    match t.frames with
    | fr :: rest -> t.frames <- (lit :: fr) :: rest
    | [] -> t.frames <- [ [ lit ] ])
;;

let collect_propagations t : Lit.t list =
  List.filter_map
    (fun (imp : Euf.implied) ->
       match Term.Table.find_opt t.watched imp.Euf.atom with
       | None -> None
       | Some atom ->
         let lit = Lit.make atom imp.Euf.value in
         cache_reason t lit (reason_of_implied t imp);
         Some lit)
    (Euf.propagate t.engine)
;;

let conflict_of prems =
  let premises = dedup_lits (lits_of_prems prems) in
  if premises = [] then failwith "Arr: empty conflict premise set (unsound) [tripwire]";
  Theory.Conflict { Explanation.premises; rule = Explanation.Rule_tag.Euf_congruence }
;;

(* At [Final] only, after ordinary ROW saturation is clean and quiet: introduce the upward
   witness reads ([ensure_store_reads]) and re-saturate. Deferring the introduction to
   [Final] — rather than running it eagerly in [saturate] at every effort — is the
   difference between +completeness with no regression and a net wash: a query that
   refutes during propagation never reaches here, so its fast refutation is untouched; the
   witness reads (and the [row_split]s they spawn) are paid for only on a branch that
   survives to a full, otherwise-consistent assignment, which is exactly where the store
   equality would otherwise go unexercised. Returns a re-saturation verdict (conflict /
   propagations) if the new reads forced one; otherwise [None] to fall through to
   [row_split]/[Sat]. *)
let final_introduce_reads t : Theory.check_result option =
  let changed = ref false in
  ensure_store_reads t ~changed;
  if not !changed
  then None
  else (
    match saturate t with
    | Some prems -> Some (conflict_of prems)
    | None ->
      (match collect_propagations t with
       | _ :: _ as lits -> Some (Theory.Propagations lits)
       | [] -> None))
;;

(* --- weak-equivalence graph maintenance (W0, dark) --- *)

(* Drain the Euf merge log into the graph. Called at the top of every {!check} and before
   every {!push}/{!pop}, so between any two drains no push/pop intervened and the whole
   drained batch belongs to the CURRENT frame — the edges are then trailed in the frame
   they actually occurred in, and {!pop} removes exactly the right ones. (The engine
   clears the log on its own [pop], so draining before ours is also what stops a
   surviving-frame merge from being lost.) A no-op with the gate OFF (the log is empty; no
   cursor). *)
let weq_sync t =
  match t.weq with
  | None -> ()
  | Some (g, cursor) ->
    List.iter
      (fun (ev : Euf.merge_event) -> Weq_graph.on_merge g ev.Fabric.kept ev.Fabric.merged)
      (Euf.drain_merges t.engine cursor)
;;

(* The admissible (stably-infinite-index) array terms the self-check samples: store terms,
   their bases, and select arrays — the domain the weak-equivalence graph reasons over. *)
let weq_array_sample t : Term.t list =
  let acc = ref [] in
  let add tm = if array_sort tm.Term.sort <> None then acc := tm :: !acc in
  List.iter
    (fun st ->
       add st;
       match st.Term.node with
       | Term.App (_, a) when Iarr.length a = 3 -> add (Iarr.get a 0)
       | _ -> ())
    t.store_terms;
  List.iter
    (fun sel ->
       match sel.Term.node with
       | Term.App (_, a) when Iarr.length a = 2 -> add (Iarr.get a 0)
       | _ -> ())
    t.select_terms;
  !acc
;;

(* At Final under OXSMT_ARR_WEQ_SELFCHECK: re-drain (to fold this check's own saturation
   merges) then cross-check graph connectivity against the engine. Raises on divergence
   (test/CI only). Dark otherwise. *)
let weq_final_self_check t =
  match t.weq with
  | Some (g, _) when weq_selfcheck ->
    weq_sync t;
    Weq_graph.self_check g (weq_array_sample t)
  | _ -> ()
;;

(* --- W0.5 dark store-chain analyzer (storecomm arbiter, §2a) --- *)

(* [i <> j] entailed by an asserted index disequality, modulo current congruence: some
   recorded asserted diseq (x, y) has [i ~ x] and [j ~ y] (or swapped). O(#diseqs) per
   test — analysis-only. Array-sorted recorded diseqs never match an index test
   (cross-sort terms are never congruent). *)
let an_distinct t (i : Term.t) (j : Term.t) : bool =
  List.exists
    (fun (x, y) ->
       (Euf.are_equal t.engine i x && Euf.are_equal t.engine j y)
       || (Euf.are_equal t.engine i y && Euf.are_equal t.engine j x))
    t.an_diseqs
;;

(* Symbolically normalize the read [select(x0, j)] down [x0]'s store chain using ONLY ROW1
   (i ~ j -> the store value) and entailed off-diagonal diseqs (i <> j -> recurse the
   base), counting the UNRESOLVED index tests. Returns [(terminal, opens, reads)]:
   - [terminal] is [`Value v] when ROW1 fired at a store on [j], else [`Leaf x] for the
     array the walk stopped at (root, cycle, or an unresolved test) — used to compare the
     two sides' reductions;
   - [opens] counts index tests (j vs a store index) that were neither entailed-equal nor
     entailed-distinct — the metric that decides linear closure;
   - [reads] counts store hops walked. Terminates via a visited-class set (congruence
     cycles), bounded by the store count. *)
let an_normalize t sidx (x0 : Term.t) (j : Term.t)
  : [ `Value of Term.t | `Leaf of Term.t ] * int * int
  =
  let visited = Hashtbl.create 16 in
  let opens = ref 0
  and reads = ref 0 in
  let rec go (x : Term.t) =
    let cx = Euf.class_of t.engine x in
    if Hashtbl.mem visited cx
    then `Leaf x
    else (
      Hashtbl.replace visited cx ();
      let stores =
        List.sort (fun a b -> compare a.Term.tag b.Term.tag) (Hashtbl.find_all sidx cx)
      in
      match stores with
      | [] -> `Leaf x
      | st :: _ ->
        (match st.Term.node with
         | Term.App (_, a) when Iarr.length a = 3 ->
           let base = Iarr.get a 0
           and i = Iarr.get a 1
           and v = Iarr.get a 2 in
           incr reads;
           if Euf.are_equal t.engine i j
           then `Value v (* ROW1: select(store(_,j,v), j) = v *)
           else if an_distinct t i j
           then go base (* off-diagonal: value unchanged at j *)
           else (
             incr opens;
             `Leaf x)
           (* unresolved test *)
         | _ -> `Leaf x))
  in
  let terminal = go x0 in
  terminal, !opens, !reads
;;

(* Two read reductions are forced equal (the L2 disjunct [select(a,j) <> select(b,j)] is
   refuted) iff both fired ROW1 to congruent values, or both bottomed out at a congruent
   base leaf. A Value-vs-Leaf pair is not decidable by this closure. *)
let an_terminals_equal t (ta : [ `Value of Term.t | `Leaf of Term.t ]) tb : bool =
  match ta, tb with
  | `Value va, `Value vb -> Euf.are_equal t.engine va vb
  | `Leaf la, `Leaf lb -> Euf.are_equal t.engine la lb
  | _ -> false
;;

(* Run once at the first Final under OXSMT_ARR_WEQ_ANALYZE: for each asserted array diseq
   that is weakly equivalent, take its canonical L2 path and symbolically close every path
   store index; report the open-test distribution to stderr (one tab-separated line per
   solve, so a per-file corpus run aggregates). Emits no lemmas, changes no verdict. *)
let weq_analyze_final t =
  match t.weq with
  | Some (g, _) when weq_analyze && not t.an_done ->
    t.an_done <- true;
    weq_sync t;
    let sidx = stores_by_class t in
    let n_diseqs = ref 0
    and n_weq = ref 0
    and n_path_idx = ref 0
    and n_open = ref 0
    and max_open = ref 0
    and n_reads = ref 0
    and n_eqguards = ref 0
    and n_closed = ref 0 in
    List.iter
      (fun (a, b) ->
         if array_sort a.Term.sort <> None
         then (
           incr n_diseqs;
           match Weq_graph.find_path g a b with
           | None -> ()
           | Some edges ->
             incr n_weq;
             (* distinct store indices on the path (term identity), and the eq-guard count *)
             let seen = Hashtbl.create 16 in
             let idxs = ref [] in
             List.iter
               (fun (e : Weq_graph.edge) ->
                  match e with
                  | Weq_graph.Store { index; _ } ->
                    if not (Hashtbl.mem seen index.Term.tag)
                    then (
                      Hashtbl.replace seen index.Term.tag ();
                      idxs := index :: !idxs)
                  | Weq_graph.Equality _ -> incr n_eqguards)
               edges;
             let diseq_open = ref 0
             and diseq_closed = ref true in
             List.iter
               (fun jl ->
                  incr n_path_idx;
                  let ta, oa, ra = an_normalize t sidx a jl in
                  let tb, ob, rb = an_normalize t sidx b jl in
                  n_reads := !n_reads + ra + rb;
                  diseq_open := !diseq_open + oa + ob;
                  if oa + ob > 0 || not (an_terminals_equal t ta tb)
                  then diseq_closed := false)
               !idxs;
             n_open := !n_open + !diseq_open;
             if !diseq_open > !max_open then max_open := !diseq_open;
             if !diseq_closed then incr n_closed)
         else ())
      t.an_diseqs;
    Printf.eprintf
      "ARR_WEQ_ANALYZE\tarr_diseqs=%d\tweq=%d\tpath_idx=%d\teqguards=%d\topen=%d\tmax_open=%d\treads=%d\tclosed=%d\n\
       %!"
      !n_diseqs
      !n_weq
      !n_path_idx
      !n_eqguards
      !n_open
      !max_open
      !n_reads
      !n_closed
  | _ -> ()
;;

(* --- W1: L1 read-over-weak-equivalence (ADR §4), gated OXSMT_ARR_WEQ, ADDS to row_split
   --- *)

(* The (array, index) of a genuine arity-2 Select term, or [None]. The
   [Iarr.length a = 2] + role guard is the load-bearing arity contract
   ([[arr-arity-guard-load-bearing]]). *)
let weq_read_parts t (sel : Term.t) : (Term.t * Term.t) option =
  match sel.Term.node with
  | Term.App (_, a) when Iarr.length a = 2 ->
    (match role_of t sel with
     | Some { Defs.role = Defs.Select; _ } -> Some (Iarr.get a 0, Iarr.get a 1)
     | _ -> None)
  | _ -> None
;;

(* Build the L1 clause for reads [r1 = select(x1, i1)] and [r2 = select(x2, i2)] (i1 ~ i2)
   over the weak-equivalence [path] from x1 to x2 (O1', ADR §4):

   select(x1,i1) = select(x2,i2) (* head *) ∨ ¬(i1 = i2) (* O1'(i): the reads' own index
   terms, if distinct *) ∨ ⋁ ¬(u = v) over Equality edges (* O1'(ii)/(iii): every path
   class collapse *) ∨ ⋁ (i1 = jₗ) over Store indices (* store-index avoidance,
   term-identity deduped (O2) *)

   Every equality the telescoping ROW argument relies on appears negated, so the clause is
   an unconditional array-theory tautology over TERMS — valid in every model regardless of
   the current e-graph, hence safe as a permanent memoized clause across backtracking.
   Endpoint and branch-local-array-merge collapses (RED #7) need no special case: the
   graph is over term nodes, so such a collapse IS an Equality edge on the path and is
   guarded like any other. The [weq_unsafe_noguard] branch is the deliberately-WRONG
   pre-erratum variant (test-only) that drops the guards, so the RED tests can show each
   is load-bearing. *)
let weq_clause
      t
      (r1 : Term.t)
      (i1 : Term.t)
      (r2 : Term.t)
      (i2 : Term.t)
      (path : Weq_graph.edge list)
  : Term.t list
  =
  let neg u v = Context.not_ t.ctx (Context.eq t.ctx u v) in
  let head = Context.eq t.ctx r1 r2 in
  let store_seen = Hashtbl.create 16 in
  let store_idx = ref []
  and eq_guards = ref [] in
  (* key store indices by TERM identity (sound, O2) unless the unsafe variant is testing
     the by-e-class dedup hole (drops a congruent-but-distinct store index ->
     wrong-unsat). *)
  let store_key (index : Term.t) =
    if weq_unsafe_noguard then Euf.class_of t.engine index else index.Term.tag
  in
  List.iter
    (fun (e : Weq_graph.edge) ->
       match e with
       | Weq_graph.Store { index; _ } ->
         let k = store_key index in
         if not (Hashtbl.mem store_seen k)
         then (
           Hashtbl.replace store_seen k ();
           store_idx := index :: !store_idx)
       | Weq_graph.Equality { u; v } -> eq_guards := neg u v :: !eq_guards)
    path;
  let idx_avoid = List.map (fun j -> Context.eq t.ctx i1 j) !store_idx in
  if weq_unsafe_noguard
  then
    head :: idx_avoid (* WRONG (test-only): no ¬(i1=i2), no eq-guards, by-class dedup *)
  else (
    let idx_guard = if Term.equal i1 i2 then [] else [ neg i1 i2 ] in
    (head :: idx_guard) @ !eq_guards @ idx_avoid)
;;

(* Emit (at most) one L1 clause: the first un-emitted, currently-unequal, congruent-index
   read pair whose arrays are both weakly equivalent to an asserted array diseq endpoint
   (OQ2 lazy diseq-reachable scope), taken over the deterministic BFS path between them
   (OQ1 selected path). Permanent memo + fuel (obligation 1 / O7). [None] when nothing new
   fires or fuel is spent, so [row_split] backstops (W1 adds; it does not yet replace). *)
let weq_l1 t : Term.t list option =
  match t.weq with
  | Some (g, _) when weq_l1_on && t.weq_fuel > 0 ->
    let endpoints =
      List.filter
        (fun (a, b) ->
           array_sort a.Term.sort <> None && Weq_graph.weakly_equivalent g a b)
        t.an_diseqs
    in
    if endpoints = []
    then None
    else (
      (* OQ2 lazy diseq-reachable scope, tightened: a read triggers L1 only if its array
         is in the SAME e-class as an asserted-diseq endpoint (i.e. reads ON the diseq
         arrays a or b, congruent forms included) — not every read on the whole
         weak-equivalence component. The wider component scope floods the mostly-sat
         swap/storecomm neighbours with O(reads^2) wide disjunctions; the full read
         closure is the W2 obligation (O11), gated behind row_split's retirement. *)
      let is_reachable x =
        List.exists
          (fun (a, b) -> Euf.are_equal t.engine x a || Euf.are_equal t.engine x b)
          endpoints
      in
      let reads =
        List.filter_map
          (fun sel ->
             match weq_read_parts t sel with
             | Some (x, i) when array_sort x.Term.sort <> None && is_reachable x ->
               Some (sel, x, i)
             | _ -> None)
          t.select_terms
      in
      let reads =
        List.sort (fun (s1, _, _) (s2, _, _) -> compare s1.Term.tag s2.Term.tag) reads
      in
      let result = ref None in
      let rec go = function
        | [] -> ()
        | (r1, x1, i1) :: rest ->
          List.iter
            (fun (r2, x2, i2) ->
               if
                 !result = None
                 && (not (Euf.are_equal t.engine r1 r2))
                 && Euf.are_equal t.engine i1 i2
               then (
                 let key =
                   if r1.Term.tag <= r2.Term.tag
                   then r1.Term.tag, r2.Term.tag
                   else r2.Term.tag, r1.Term.tag
                 in
                 if not (Hashtbl.mem t.weq_emitted key)
                 then (
                   match Weq_graph.find_path g x1 x2 with
                   | Some path ->
                     let clause = weq_clause t r1 i1 r2 i2 path in
                     if List.length clause >= 2
                     then (
                       Hashtbl.replace t.weq_emitted key ();
                       t.weq_fuel <- t.weq_fuel - 1;
                       result := Some clause)
                   | None -> ())))
            rest;
          if !result = None then go rest
      in
      go reads;
      !result)
  | _ -> None
;;

let check t effort =
  weq_sync t;
  match saturate t with
  | Some prems -> conflict_of prems
  | None ->
    let lits = collect_propagations t in
    (match lits, effort with
     | _ :: _, _ -> Theory.Propagations lits
     | [], Theory.Propagate -> Theory.Propagations []
     | [], Theory.Final ->
       weq_analyze_final t;
       (match final_introduce_reads t with
        | Some result -> result
        | None ->
          weq_final_self_check t;
          (* W1: try the L1 read-over-weakeq clause first, then fall back to the (still
             present) blind row_split; W2 will retire row_split. *)
          (match weq_l1 t with
           | Some terms -> Theory.Split terms
           | None ->
             (match row_split t with
              | Some terms -> Theory.Split terms
              | None -> Theory.Sat))))
;;

let explain t lit =
  match Lit.Map.find_opt lit t.explain_cache with
  | Some expl -> expl
  | None ->
    failwith
      "Arr.explain: no cached reason for literal (not theory-propagated, or its frame \
       was popped)"
;;

let push t =
  weq_sync t;
  (* fold pending merges into the current frame before it becomes a parent *)
  (match t.weq with
   | Some (g, _) -> Weq_graph.push g
   | None -> ());
  Euf.push t.engine;
  t.frames <- [] :: t.frames
;;

let pop t n =
  weq_sync t;
  (* fold pending (current-frame) merges before the engine clears its log on pop, then
     unwind the graph's equality edges for the popped frames in lockstep with Euf *)
  (match t.weq with
   | Some (g, _) -> Weq_graph.pop g n
   | None -> ());
  Euf.pop t.engine n;
  let rec drop k frames =
    if k = 0
    then frames
    else (
      match frames with
      | fr :: rest ->
        List.iter (fun l -> t.explain_cache <- Lit.Map.remove l t.explain_cache) fr;
        drop (k - 1) rest
      | [] -> [])
  in
  t.frames
  <- (match drop n t.frames with
      | [] -> [ [] ]
      | fs -> fs)
;;

(* --- models (never trusted for a client verdict: array sat degrades to unknown) --- *)

let children (term : Term.t) : Term.t list =
  match term.Term.node with
  | Term.Bool_const _ | Term.Int_const _ -> []
  | Term.App (_, args) -> Iarr.to_list args
  | Term.Arith { coeffs; _ } -> List.map fst (Iarr.to_list coeffs)
  | Term.Le a | Term.Not a -> [ a ]
  | Term.Eq (a, b) -> [ a; b ]
  | Term.And a | Term.Or a -> Iarr.to_list a
  | Term.Ite (c, a, b) -> [ c; a; b ]
;;

let model t =
  let seen = Term.Table.create 64 in
  let acc = ref [] in
  let rec walk (term : Term.t) =
    if not (Term.Table.mem seen term)
    then (
      Term.Table.replace seen term ();
      let v =
        match term.Term.node with
        | Term.Eq (a, b) -> Model.Bool (Euf.are_equal t.engine a b)
        | _ ->
          if Sort.equal term.Term.sort Sort.bool
          then
            if Euf.are_equal t.engine term t.true_const
            then Model.Bool true
            else if Euf.are_equal t.engine term t.false_const
            then Model.Bool false
            else Model.Uninterp (Euf.class_of t.engine term)
          else Model.Uninterp (Euf.class_of t.engine term)
      in
      acc := (term, v) :: !acc;
      List.iter walk (children term))
  in
  List.iter walk (List.rev t.atom_terms);
  Model.of_alist !acc
;;

(* --- array sat models (§8 self-check via Array_model_check) --- *)

type value =
  | Scalar of Model.value
  | Array of
      { entries : (value * value) list
      ; default : value
      }

let array_model t : (Term.t * value) list option =
  (* A per-e-class scalar for a NON-array leaf (an index/element value). Uninterpreted and
     Int classes take a per-class-distinct witness (distinct classes => distinct values,
     so an index/element disequality holds); Bool is a bounded 2-value domain (like the DT
     Bool completion); an Int class carrying a literal takes it. *)
  let bool_memo : (int, bool) Hashtbl.t = Hashtbl.create 16 in
  let bool_next = ref 0 in
  let scalar_of (x : Term.t) : Model.value =
    if Sort.equal x.Term.sort Sort.bool
    then
      if Euf.are_equal t.engine x t.true_const
      then Model.Bool true
      else if Euf.are_equal t.engine x t.false_const
      then Model.Bool false
      else (
        let k = Euf.class_of t.engine x in
        match Hashtbl.find_opt bool_memo k with
        | Some b -> Model.Bool b
        | None ->
          let b = !bool_next = 1 in
          incr bool_next;
          Hashtbl.replace bool_memo k b;
          Model.Bool b)
    else (
      match x.Term.sort with
      | Sort.Int _ ->
        let members = Euf.class_members t.engine x in
        (match
           List.find_map
             (fun (m : Term.t) ->
                match m.Term.node with
                | Term.Int_const n -> Some n
                | _ -> None)
             members
         with
         | Some n ->
           (match Bigint.to_int_opt n with
            | Some i -> Model.Int i
            | None -> Model.Uninterp (Euf.class_of t.engine x))
         | None -> Model.Uninterp (Euf.class_of t.engine x))
      | _ -> Model.Uninterp (Euf.class_of t.engine x))
  in
  (* Group every registered [select b j] by the e-class of its array argument [b], so an
     array class's model map lists (value of j -> value of that select) for every read of
     a congruent array. This is the finite part of the weak-array value; unread indices
     take the element sort's base default. *)
  let by_class : (int, (Term.t * Term.t) list) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun sel ->
       match head_args sel, role_of t sel with
       | Some (_, [| arr; j |]), Some { Defs.role = Defs.Select; _ } ->
         let k = Euf.class_of t.engine arr in
         Hashtbl.replace
           by_class
           k
           ((j, sel) :: Option.value (Hashtbl.find_opt by_class k) ~default:[])
       | _ -> ())
    (List.rev t.select_terms);
  (* A representative store term per e-class that contains one (tag-least, for
     determinism). An array class that equals a [store base i v] must take that store's
     VALUE — its base's value with [(i,v)] overlaid — not an independent map reconstructed
     from its own reads: the two disagree at any index read on one side of the store
     equality but not the other, and the self-check (which recomputes [store]
     structurally) then rejects the whole model. This is the storecomm shape (a_k = store
     a_j .. chains): deriving each array structurally over the store DAG makes the
     [a_k = store ..] equalities hold by construction. *)
  let store_of_class : (int, Term.t) Hashtbl.t = Hashtbl.create 64 in
  List.iter
    (fun st ->
       match head_args st, role_of t st with
       | Some (_, [| _b; _i; _v |]), Some { Defs.role = Defs.Store; _ } ->
         let k = Euf.class_of t.engine st in
         (match Hashtbl.find_opt store_of_class k with
          | Some prev when prev.Term.tag <= st.Term.tag -> ()
          | _ -> Hashtbl.replace store_of_class k st)
       | _ -> ())
    t.store_terms;
  let memo : (int, value) Hashtbl.t = Hashtbl.create 64 in
  let rec default_of (element : Sort.t) (depth : int) : value =
    if depth > 1_000
    then Scalar (Model.Uninterp 0)
    else (
      match element with
      | Sort.Array (_, e2) -> Array { entries = []; default = default_of e2 (depth + 1) }
      | Sort.Bool -> Scalar (Model.Bool false)
      | Sort.Int _ -> Scalar (Model.Int 0)
      | Sort.Uninterpreted _ | Sort.Datatype _ | Sort.BitVec _ ->
        Scalar (Model.Uninterp 0))
  and value_of (x : Term.t) (depth : int) : value =
    if depth > 1_000
    then Scalar (Model.Uninterp 0)
    else (
      match x.Term.sort with
      | Sort.Array (_index, element) ->
        let k = Euf.class_of t.engine x in
        (match Hashtbl.find_opt memo k with
         | Some v -> v
         | None ->
           (* seed a base value first (cycle guard; array sorts are non-recursive so this
              is only defensive) *)
           Hashtbl.replace
             memo
             k
             (Array { entries = []; default = default_of element (depth + 1) });
           let v =
             match Hashtbl.find_opt store_of_class k with
             | Some { Term.node = Term.App (_, args); _ } when Iarr.length args = 3 ->
               (* store-respecting: this class equals [store base i v]; take base's value
                  with [(i, v)] overlaid, exactly as the checker computes [store base i v]
                  (prepend (i-value, v-value); keep base's default), so the
                  [a_k = store ..] equality holds by construction. *)
               let a = Array.of_list (Iarr.to_list args) in
               (match value_of a.(0) (depth + 1) with
                | Array base ->
                  Array
                    { entries =
                        (value_of a.(1) (depth + 1), value_of a.(2) (depth + 1))
                        :: base.entries
                    ; default = base.default
                    }
                | Scalar _ as s -> s (* base must be an array; defensive *))
             | _ ->
               (* root array (no store in its class): the finite map from its own reads. *)
               let sels = Option.value (Hashtbl.find_opt by_class k) ~default:[] in
               let entries =
                 List.map
                   (fun (j, sel) -> value_of j (depth + 1), value_of sel (depth + 1))
                   sels
               in
               Array { entries; default = default_of element (depth + 1) }
           in
           Hashtbl.replace memo k v;
           v)
      | _ -> Scalar (scalar_of x))
  in
  (* one value per registered subterm of an asserted atom (array leaves + index/element
     leaves; the checker computes select/store/equality itself) *)
  let seen = Term.Table.create 128 in
  let acc = ref [] in
  let rec collect (term : Term.t) =
    if not (Term.Table.mem seen term)
    then (
      Term.Table.replace seen term ();
      acc := (term, value_of term 0) :: !acc;
      List.iter collect (children term))
  in
  List.iter collect (List.rev t.atom_terms);
  Some !acc
;;
