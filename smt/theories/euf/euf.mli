(** Proof-producing congruence closure for EUF (equality + uninterpreted functions),
    following Nieuwenhuis–Oliveras, "Proof-producing Congruence Closure" (2005/2007): a
    union-find with an {e explanation forest}, a congruence table, and a pending merge
    queue (DESIGN.md §6; INVARIANTS.md I4).

    {b Engine-independent core.} This module is the theory {e engine}, not the M1
    {!Oxsmt_core}-level THEORY adapter (ADR-0005, which freezes at M1-end). It is
    deliberately built against the frozen [core] only ([Term]/[Context]/[Theory_view]).
    The adapter is a thin later layer: it maps ADR-0005's [Atom.t]/[Lit.t] to the opaque
    premise token ['p] below and forwards [register_atom]/[assert_lit]/[check]/
    [explain]/[push]/[pop]. Nothing here depends on [solver], [lit], or [explanation]
    (which do not exist yet).

    {b Premise tokens ('p).} Every asserted (dis)equality carries a caller-supplied,
    opaque justification token of type ['p]. [check]/[explain] return premise tokens only.
    At adapter time ['p] is instantiated to ADR-0005's [Lit.t] and the returned lists
    become the [Explanation.premises] of a conflict / a lazy propagation reason (ADR-0005
    D7). The engine never inspects a token; it only stores and returns it.

    {b What EUF congruence-closes.} Per ADR-0003's theory-dispatch split, only [App] nodes
    are congruent; every non-[App] subterm ([Arith]/[Le]/[Ite]/constants/…) is an
    {e opaque leaf} — it gets a union-find node so equalities may be asserted over it
    (e.g. a Nelson–Oppen shared [Arith] variable), but it is never looked inside for
    congruence. This is exactly what lets EUF and LIA share terms later.

    {b Determinism (ADR-0005 C1–C8, INVARIANTS.md I6).} Every observable output —
    propagation order, the chosen conflict, and each explanation's premise order — is a
    deterministic function of the registration/assertion sequence. Internally, iteration
    that can reach output is ordered by e-node id (registration order) or [Term] tag,
    never by [Hashtbl] traversal. A fixed input sequence yields byte-identical results
    across runs.

    {b Single-Context contract (ADR-0003).} All [Term.t]s passed in must come from the one
    [Context] handed to {!create}; mixing contexts is undefined behaviour. *)

open Oxsmt_core

(** An EUF state parameterised by the premise-token type ['p]. Mutable; backtracking is
    via {!push}/{!pop} (trail-based, level-granular). *)
type 'p t

(** [create ctx] is an empty state bound to session [ctx] (ADR-0003 Decision 6 / ADR-0005
    D6: one [Context] per session). [ctx] is retained for future mid-solve term
    construction; v1 EUF builds no new terms itself. *)
val create : Context.t -> 'p t

(** [register_term t term] internalises [term] and {e every} subterm below it
    (CONTRACT-REG-1/2, post-order): each distinct subterm gets an e-node keyed on its
    [Term] tag (O(1)); each [App] additionally joins the congruence table (so it can be
    detected congruent to a peer). A registered non-Bool [Eq(a,b)] atom is {e watched} for
    {!propagate} (its sides are registered too). Idempotent (C7): re-registering a term
    perturbs neither state nor ids. Registration is monotone within a frame and is undone
    by {!pop} of the frame that introduced it. *)
val register_term : 'p t -> Term.t -> unit

(** [assert_eq t ~premise a b] asserts [a = b] justified by [premise]; [a]/[b] (and their
    subterms) are registered if new. Side-effect only: it merges classes and
    congruence-closes but never reports a conflict — consistency is decided by {!check}
    (ADR-0005 D3). *)
val assert_eq : 'p t -> premise:'p -> Term.t -> Term.t -> unit

(** [assert_neq t ~premise a b] asserts [a <> b] justified by [premise]. Side-effect only;
    a violated disequality surfaces at {!check}. *)
val assert_neq : 'p t -> premise:'p -> Term.t -> Term.t -> unit

type 'p check_result =
  | Consistent
  | Conflict of 'p list
  (** The asserted set is EUF-inconsistent: some asserted [a <> b] now has [a] and [b] in
      one class. The list is a premise-token subset that alone entails the contradiction —
      the disequality's token plus the equality chain explaining [a = b]
      (Nieuwenhuis–Oliveras). Small, not guaranteed minimal. *)

(** [check t] scans the asserted disequalities in assertion order and returns the first
    that is violated (C3: deterministic choice), else [Consistent]. In debug/test builds
    the returned premise set is re-verified by independent replay (see below). *)
val check : 'p t -> 'p check_result

(** [explain t a b] returns a premise-token subset whose asserted equalities alone entail
    [a = b] (the Nieuwenhuis–Oliveras explanation: given-equality and congruence chains,
    congruences expanded to their argument equalities). Requires [are_equal t a b]; raises
    [Invalid_argument] otherwise.

    {b Ordering / precedence obligation (ADR-0005 CONTRACT-EX).} The explanation forest
    stores the {e original} union edges in assertion order and never rewrites an edge once
    a class-connecting edge exists (a redundant merge is skipped). Hence every returned
    premise was asserted no later than the merge that first connected [a] and [b]: the
    reason is valid {e at that merge's time}, not merely "some currently- asserted set."
    The adapter relies on this for a well-formed 1UIP reason clause; the obligation is
    documented here because under zero human review the contract is the spec. The premise
    list is deterministic (C2): same state, same order every call. *)
val explain : 'p t -> Term.t -> Term.t -> 'p list

(** An equality atom EUF has newly {e implied} since the previous {!propagate}. *)
type implied =
  { atom : Term.t (** a watched non-Bool [Eq(a,b)] registered via {!register_term} *)
  ; value : bool
  (** [true]: [a] and [b] are now in one class ([a = b] entailed). [false]: [a] and [b]
      are now provably distinct (an asserted disequality separates their classes) —
      [¬(a = b)] entailed. *)
  }

(** [propagate t] returns the watched equality atoms whose entailed truth-value changed
    since the last [propagate]/backtrack (theory propagation for the future adapter and
    Nelson–Oppen sharing; DESIGN.md §6). Explanations are lazy — call {!explain_implied}.
    Deterministic order (registration order, C1). *)
val propagate : 'p t -> implied list

(** [explain_implied t imp] is the premise-token subset entailing [imp] (lazy, C2). For
    [value = true] it is {!explain} of the two sides; for [value = false] it is the
    separating disequality's token plus the chains linking each side to that disequality's
    endpoints. *)
val explain_implied : 'p t -> implied -> 'p list

(** [are_equal t a b] holds iff [a] and [b] are currently in the same class (registers
    them if new). O(tree height). *)
val are_equal : 'p t -> Term.t -> Term.t -> bool

(** [class_of t term] is an opaque, per-run-stable representative id for [term]'s class
    (the union-find root's e-node id). Equal terms share it; it is deterministic across
    identical runs but not meaningful across states. For tests/introspection. *)
val class_of : 'p t -> Term.t -> int

(** [push t] opens a backtrack frame (a checkpoint). *)
val push : 'p t -> unit

(** [pop t n] discards the last [n] frames, restoring all state (union-find, explanation
    forest, congruence table, disequalities, watched-atom propagation flags, and e-nodes
    registered inside those frames) to the [n]-frames-ago checkpoint. Raises
    [Invalid_argument] if [n] exceeds the open frame count. *)
val pop : 'p t -> int -> unit

(** [num_terms t] is the count of currently-registered e-nodes (introspection/metrics). *)
val num_terms : 'p t -> int

(** [context t] is the session [Context] passed to {!create} (the adapter shares it for
    mid-solve term construction; v1 EUF builds no terms itself). *)
val context : 'p t -> Context.t

(** Debug/test controls. *)
module Debug : sig
  (** When [true] (the default), every explanation produced by {!check}/{!explain}/
      {!explain_implied} is re-verified the moment it is produced by replaying its
      premises' equalities into a {e fresh, independent} naive union-find and confirming
      they alone imply the claimed (dis)equality (DESIGN.md §7 self-check). A failed
      self-check raises [Failure]. Turn off only for a production/perf build. *)
  val self_check : bool ref
end
