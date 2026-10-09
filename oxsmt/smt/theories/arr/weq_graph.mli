(** The weak-equivalence graph (Christ & Hoenicke, "Weakly Equivalent Arrays", 2015) — the
    W0 dark substrate for the QF_AX weak-equivalence decision procedure (ADR-weakeq /
    DESIGN.md A12).

    Two array terms are {b weakly equivalent} when they are connected by a path of
    - {b store edges}: [store(base, i, v)] is weakly equivalent to [base] (they differ
      only at index [i]); one syntactic edge per store term, labelled by the index term;
      and
    - {b equality edges}: two array terms in the same congruence class.

    This module MAINTAINS that graph incrementally and answers term-level path queries; it
    emits no lemmas (W0 is dark). The L1/L2 rules that consume the paths — with a negated
    equality guard at every class-collapse point (O1') — arrive at W1/W2.

    {b Abstract e-graph view (O6, OQ4).} The graph reads class identity / equality /
    justification ONLY through {!egraph_view}; it never calls {!Oxsmt_euf.Euf} directly,
    so migrating onto the ADR-0014 fabric hub (W3) is a re-binding of the view, not a
    rewrite.

    {b Backtracking (§5).} Store edges are PERMANENT and untrailed: a store term is weakly
    equivalent to its base in every state, and the theory's [store_terms] is monotonic.
    Equality edges are folded from the {!Oxsmt_euf.Euf} merge stream and removed on {!pop}
    via an internal {!Oxsmt_core.Trail}. Because the volatile (equality) edges are
    separated from the permanent (store) edges and connectivity is by breadth-first search
    rather than a re-rooting union-find, there is no permanent-vs-trailed union coupling
    to get wrong.

    {b Determinism.} [find_path] visits neighbours in ascending term-tag order, so the
    discovered path is a deterministic function of the assertion/registration sequence
    (INVARIANTS.md I6). *)

open Oxsmt_core

(** The abstract array e-graph the weak-equivalence graph reads through (O6). Standalone,
    these close over the theory's private [Euf.t]; on the fabric they close over the hub. *)
type egraph_view =
  { class_of : Term.t -> int
  ; are_equal : Term.t -> Term.t -> bool
  ; explain_equal : Term.t -> Term.t -> Lit.t list
    (** The justification handle for an equality edge: the premise literals whose
      conjunction entails [a = b] (for the W1 guard's explanation set). Its precondition
      is [are_equal a b]. Unused in W0 (dark); carried for W1. *)
  }

(** A term-level edge on a weak-equivalence path, oriented [u -> v] in traversal order. A
    [Store] edge additionally names the [store_term]/[base] roles and the store [index]
    (for the W1 [i = index] avoidance disjuncts and the W2 read closure); [u]/[v] are
    those two terms in traversal order. An [Equality] edge's guard is [¬(u = v)], its
    justification {!egraph_view.explain_equal} [u v]. *)
type edge =
  | Store of
      { u : Term.t
      ; v : Term.t
      ; store_term : Term.t
      ; base : Term.t
      ; index : Term.t
      }
  | Equality of
      { u : Term.t
      ; v : Term.t
      }

type t

(** [create view] is an empty graph reading the e-graph through [view]. *)
val create : egraph_view -> t

(** [index_sort_stably_infinite s] is [true] for the index sorts over which the rules are
    sound to fire (O9): mathematical [Int] and [Uninterpreted] sorts (stably infinite). It
    is [false] for finite sorts ([Bool], [BitVec]) and for sorts that may be finite
    ([Datatype] enumerations) or exotic ([Array]-sorted indices) — over a finite index
    domain the model-value validator's "default element for unlisted indices" assumption
    is unsound (a wrong-SAT vector), so the rules must not fire. *)
val index_sort_stably_infinite : Sort.t -> bool

(** [array_sort_admissible s] is [true] when [s] is an array sort whose index sort is
    stably infinite (O9). An inadmissible array is given NO graph tracking — its
    store/equality edges are dropped — so no path is ever found over it and the W1/W2
    rules never fire. *)
val array_sort_admissible : Sort.t -> bool

(** [add_store_edge t ~store_term ~base ~index] records the permanent store edge for
    [store_term = store(base, index, _)]. Idempotent per store term; a no-op if
    [store_term]'s sort is inadmissible (O9). Untrailed (§5). *)
val add_store_edge : t -> store_term:Term.t -> base:Term.t -> index:Term.t -> unit

(** [on_merge t a b] folds one class union of two array terms into the equality adjacency.
    Idempotent per orientation-normalized pair within a frame; a no-op unless both sorts
    are admissible (O9). Trailed: removed on {!pop} of the frame it was added in. The
    caller (the arrays theory) drains {!Oxsmt_euf.Euf.drain_merges} and calls this per
    array-sorted merge event. *)
val on_merge : t -> Term.t -> Term.t -> unit

(** [push t] opens a backtrack frame (pair with {!pop}). *)
val push : t -> unit

(** [pop t n] discards the [n] newest frames, removing every equality edge folded in them
    (store edges are permanent and survive). *)
val pop : t -> int -> unit

(** [weakly_equivalent t a b] is [true] iff a weak-equivalence path connects [a] and [b]
    in the maintained graph. *)
val weakly_equivalent : t -> Term.t -> Term.t -> bool

(** [find_path t a b] is the deterministic canonical weak-equivalence path from [a] to [b]
    (oriented edges), [Some []] when [a] and [b] are the same term, or [None] when they
    are not weakly equivalent. This is the O6 term-carrying query the W1 L1/L2 rules
    consume. QUERY-SIDE O9 (W1 obligation 2): returns [None] when either sort is
    inadmissible — BEFORE the reflexive [Some []] shortcut — so no rule can fire over a
    finite-index array via a zero-length path, the reflexive case included. *)
val find_path : t -> Term.t -> Term.t -> edge list option

(** [self_check t array_terms] raises [Failure] if two terms in [array_terms] that the
    view reports in one e-class are not weakly equivalent in the graph (the equality edges
    failed to span a class — a merge-stream mis-fold). Incompleteness-only, never an
    unsoundness check; fail-loud for test/CI ([OXSMT_ARR_WEQ_SELFCHECK]). Quadratic in the
    sample. *)
val self_check : t -> Term.t list -> unit
