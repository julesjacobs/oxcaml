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
    detected congruent to a peer). Two kinds of registered term are {e watched} for
    {!propagate}: a non-Bool [Eq(a,b)] atom (truth = [a ~ b], its sides are registered
    too) and a Bool-codomain predicate application [p(x…)] of arity >= 1 (truth =
    [p(x…) ~ true_const], watched against the hash-consed [true] constant). The predicate
    watch is the ⊤/⊥ bridge that surfaces [p(a), a = b |- p(b)] as a {!propagate} flip. A
    nullary Bool [App] (a bare Bool variable) is not watched — no congruence can entail
    its value, so a watch would only echo a direct assertion. Idempotent (C7):
    re-registering a term perturbs neither state nor ids. Registration is monotone within
    a frame and is undone by {!pop} of the frame that introduced it. *)
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

(** A watched atom EUF has newly {e implied} since the previous {!propagate}. *)
type implied =
  { atom : Term.t
    (** a watched atom registered via {!register_term}: a non-Bool [Eq(a,b)] (sides [a]/[b])
      or a Bool-codomain predicate [p(x…)] (implicitly watched against [true_const]). *)
  ; value : bool
    (** For an [Eq(a,b)] — [true]: [a],[b] now in one class ([a = b] entailed); [false]:
      [a], [b] now provably distinct ([¬(a = b)] entailed). For a predicate [p(x…)] —
      [true]: [p(x…) ~ true_const] ([p(x…)] entailed); [false]: [p(x…)] provably distinct
      from [true_const], i.e. [p(x…) ~ false_const] via the [true <> false] axiom
      ([¬p(x…)] entailed). *)
  }

(** [propagate t] returns the watched atoms whose entailed truth-value changed since the
    last [propagate]/backtrack (theory propagation for the adapter and Nelson–Oppen
    sharing; DESIGN.md §6): [Eq] atoms flipping (dis)equal and predicate atoms whose
    congruence class reached [true_const]/[false_const]. Explanations are lazy — call
    {!explain_implied}. Deterministic order (registration order, C1). *)
val propagate : 'p t -> implied list

(** [explain_implied t imp] is the premise-token subset entailing [imp] (lazy, C2). For
    [value = true] it is {!explain} of the two sides; for [value = false] it is the
    separating disequality's token plus the chains linking each side to that disequality's
    endpoints. *)
val explain_implied : 'p t -> implied -> 'p list

(** [rearm_watch t term] resets the watch on [term] (if any) so the next {!propagate}
    re-reports its currently-entailed truth: its last-reported value is cleared to unknown
    and its endpoints re-dirtied. Needed when an atom is bound to a predicate whose watch
    was created earlier by a boundary-only {!register_term} and whose one-shot flip was
    already consumed by a {!propagate} while it had no owner — the adapter calls this at
    the binding [register_atom] so a mid-solve predicate (e.g. a lemma instance) does not
    silently lose its theory propagation. No-op if [term] is not watched. Trailed, so a
    subsequent {!pop} restores the reset like any other watch state. *)
val rearm_watch : 'p t -> Term.t -> unit

(** [rearm_watches_if t pred] re-arms (same per-watch effect as {!rearm_watch}) every
    watch whose watched-atom term satisfies [pred], in a single O(#watches) pass. The
    adapter's pop-recovery for the predicate late-binding recurrence re-arms a whole set
    of bound predicate watches at once after a [pop] restored their trailed last-reported
    values; a per-term {!rearm_watch} loop would be O(#predicates x #watches). *)
val rearm_watches_if : 'p t -> (Term.t -> bool) -> unit

(** {2 Merge-notification log (ADR-0014 Stage 2, §A.3).}

    The reverse of {!assert_eq}: instead of a theory pushing an equality {e into} the hub,
    the hub records each class union so the combinator can notify an interested theory
    {e out} of the hub (z3's [new_eq] theory hook). Deliberately a pull log, not a
    synchronous callback: the engine calls no foreign code, so determinism and the
    non-reentrant cascade discipline (F5) are preserved — the combinator drains after each
    {!check} and reacts (asserting the bound-equality into LIA), never inside a merge. *)

(** [set_record_merges t on] toggles merge logging. Default [false] ⇒ {!merge} appends
    nothing and {!take_merges} is empty, so a direct-drive or fabric-off caller is
    byte-identical and pays no hot-path cost. Turning it off also clears the pending log. *)
val set_record_merges : 'p t -> bool -> unit

(** A class union reported to a client (ADR-0014 Stage 2/3). [kept]/[merged] are the two
    ORIGINAL endpoint terms of the union (no survivor semantics implied — for an asserted
    equality the asserted pair, for a congruence the two congruent [App] terms).
    [kept_tag] is the per-class tag ({!set_class_tag}) of [kept]'s class, [merged_tag] of
    [merged]'s, both CAPTURED AT MERGE TIME (before the surviving root inherits) — so a
    client sees both tags even when they collide. The merge reason is
    [explain t kept merged] (precedence- valid when drained right after the union).
    Defined in {!Oxsmt_core.Fabric} so the combinator (which drains for the LIA-notify
    path) reads it without depending on this engine. *)
type merge_event = Fabric.merge_event =
  { kept : Term.t
  ; merged : Term.t
  ; kept_tag : Term.t option
  ; merged_tag : Term.t option
  }

(** A per-client read position into the merge log. Multiple clients (datatypes, the
    LIA-notify path) each hold their own cursor so every client sees every merge. *)
type merge_cursor

(** [add_merge_consumer t] registers a new client cursor positioned at the current end of
    the log (it sees only merges from now on). *)
val add_merge_consumer : 'p t -> merge_cursor

(** [drain_merges t c] returns the merge events appended since [c] last drained (oldest
    first) and advances [c] to the current end. Order is the deterministic merge-queue
    order (I6). The log is cleared and all cursors reset by {!pop} (an undrained merge
    from a popped frame is dropped — completeness-safe, never unsound; a consumer's action
    on an already-drained merge unwinds via that consumer's own trailed state). *)
val drain_merges : 'p t -> merge_cursor -> merge_event list

(** [set_class_tag t term tag] attaches the witness [tag] to [term]'s class (datatypes:
    the class's representative constructor application). Trailed (restored by {!pop}); the
    surviving root inherits a tag on merge if it had none, and a merge of two tagged
    classes is surfaced via {!take_merges} (both tags) for the client to resolve.
    Registers [term]/[tag] if new. A re-set on a class overwrites. *)
val set_class_tag : 'p t -> Term.t -> Term.t -> unit

(** [class_tag t term] is the per-class tag of [term]'s class, or [None]. Registers [term]
    if new. *)
val class_tag : 'p t -> Term.t -> Term.t option

(** [are_equal t a b] holds iff [a] and [b] are currently in the same class (registers
    them if new). O(tree height). *)
val are_equal : 'p t -> Term.t -> Term.t -> bool

(** [class_of t term] is an opaque, per-run-stable representative id for [term]'s class
    (the union-find root's e-node id). Equal terms share it; it is deterministic across
    identical runs but not meaningful across states. For tests/introspection. *)
val class_of : 'p t -> Term.t -> int

(** {2 Read-only query API (ADR-0012 L2 / R6, tranche 2 E-matching).}

    Four {b genuinely non-registering} accessors the E-matcher reads the e-graph through.
    Unlike {!are_equal}/{!class_of} (which {e register-if-new}, growing the e-graph),
    these never call [register] and never mutate any structure — the matcher cannot
    perturb the e-graph, which the failure-direction analysis (ADR-0012 §3) requires.
    Every result is ordered by e-node id (= registration order), never by [Hashtbl]
    traversal (C8, I6). An absent (unregistered) term is treated as its own
    {b singleton class matched by tag-equality only}, so a stale/missing class yields
    either a valid universal instance or no instance, never a wrong refutation. *)

(** [app_terms_by_symbol t sym] is the registered ground [App] terms whose head is [sym],
    in registration order — the trigger-root candidates for the matcher (R-EM3). *)
val app_terms_by_symbol : 'p t -> Symbol.t -> Term.t list

(** [find_class_opt t term] is [term]'s class root iff [term] is already registered, else
    [None]. Never registers. *)
val find_class_opt : 'p t -> Term.t -> int option

(** [equal_if_registered t a b] is congruence-equality treating an unregistered term as
    its own singleton class: both registered ⇒ same root; else [Term.equal] (hash-cons tag
    equality). Never registers, never mutates. *)
val equal_if_registered : 'p t -> Term.t -> Term.t -> bool

(** [class_members t term] is the members of [term]'s congruence class in id order, for
    matching modulo EUF-congruence equalities. An unregistered term is the singleton
    [[term]]. Never registers. *)
val class_members : 'p t -> Term.t -> Term.t list

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
  (** When [true], every explanation produced by {!check}/{!explain}/{!explain_implied} is
      re-verified the moment it is produced by replaying its premises' equalities into a
      {e fresh, independent} naive union-find and confirming they alone imply the claimed
      (dis)equality (DESIGN.md §7 self-check). A failed self-check raises [Failure].
      Defaults to [false] (perf); opt in via the [OXSMT_EUF_SELF_CHECK] environment
      variable (read at module init), as the test/CI configuration does. *)
  val self_check : bool ref
end
