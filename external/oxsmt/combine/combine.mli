(** Nelson–Oppen model-based theory combination (DESIGN.md §6; ADR-0005 D4/D5).

    [Combine (R) (A) (B)] packages two child {!Oxsmt_core.Theory.THEORY} plugins as ONE
    THEORY the CDCL(T) engine drives — the multiplexer of DESIGN §6. The engine sees a
    single theory; this module routes each atom/literal to its owning child, merges their
    propagations and conflicts, forwards [push]/[pop] in lockstep, and — at [Final] —
    performs Z3-style {b model-based} combination over the INTERFACE SET: it reads the
    children's candidate models and, only when they {e disagree} on a shared interface
    node, emits a genuinely-constraining {!Oxsmt_core.Theory.Split} through the SAT core
    rather than eagerly propagating the equality (LIA over ℤ is non-convex, so eager
    arrangement propagation is unsound/incomplete; DESIGN §6).

    {b Internalization (the interface set).} Following the internalization ADR, each term
    node is its own proxy — no fresh variables, no defining equations. As each atom is
    registered, a total structural walk classifies every node by its head ([A]/congruence:
    uninterpreted applications + the Bool constants; [B]/arithmetic:
    [Arith]/[Le]/numerals; neutral: bare variables and Bool connectives) and records the
    INTERFACE SET: a node whose owner differs from a parent edge's owner (an arith term
    under an uninterpreted [f]; a numeral under [f]; an [App] inside a sum; an owned
    equality side), plus a bare variable used as an operand by BOTH an [A]-owned and a
    [B]-owned node (which includes an (dis)equality operand, since the congruence child
    decides equality). Sharedness is thus {b total by construction}, never a relevance
    filter — this is what dissolves the too-small (wrong-SAT) / too-large
    (non-terminating) approximation bug family. A boundary term that surfaces only inside
    [B]'s atom is made visible to the congruence child via
    {!CONGRUENCE_CHILD.internalize_term}. The disagreement search at [Final] ranges over
    the interface set, comparing only members BOTH child models currently value (a stale
    post-[pop] member is skipped, not split).

    {b PRECONDITION — preprocessed fragment.} The interface walk assumes every asserted
    term is in the preprocessed QF_UFLIA fragment: {b no Int-sorted [Ite]} and no reserved
    [div]/[mod] applications (ADR-0003 invariant 10; the [Term.Debug] [Pipeline] mode). An
    Int [Ite] is a neutral node whose Int branches under a neutral parent would take no
    use-bit, so it must be lifted by smt/preprocess before assertion — combination
    correctness depends on that lift. (A Bool [Ite] as a UF argument is handled: it
    degrades via {!Incomplete}, §3.6 case (ii).) The walk carries a debug assertion for a
    residual Int [Ite].

    {b Two distinct cross-theory paths.} (1) An equality a child ENTAILS (EUF's congruence
    deriving [f a = f b] after [a = b], or LIA a bound-implied equality) is returned by
    that child's {!Oxsmt_core.Theory.check} as a propagation; [Combine] forwards it, the
    SAT core puts it on the trail, and it is re-asserted to its owner(s) — a shared
    equality thereby reaches both children through NORMAL propagation forwarding, no split
    involved. (2) The {!Oxsmt_core.Theory.Split} is the mechanism for the other case:
    resolving a MODEL DISAGREEMENT, where neither child entails the equality but their
    candidate models pick incompatible arrangements — the split forces the SAT core to
    decide the shared equality atom. {b Termination} of the split loop rests on the seam's
    full-assignment-at-[Final] contract: at a full boolean model every registered atom —
    including each split's equality atom — is assigned, so a pair we split on is decided
    the next [Final]; either polarity removes the disagreement (equal ⇒ both models agree
    on it; unequal ⇒ the [<]/[>] disjunct constrains the arithmetic child to match), and a
    decided pair never re-disagrees. Finitely many shared pairs ⇒ finitely many splits.

    {b Why a functor with a [ROUTER], not a fixed EUF+LIA pair.} The frozen THEORY
    signature deliberately carries no ownership predicate (per-assertion traffic is a
    packed {!Oxsmt_core.Atom.t}, ADR-0005 D2). Routing — which child owns an atom, and how
    to build the equality split for a given sort — is structural knowledge the combinator
    needs but the engine must not. A generic functor takes it as the {!ROUTER} parameter,
    so [Combine] depends only on [core] (I3) and is tested against hand-rolled child
    theories, independent of the real EUF/LIA engines. (The interface set is derived by
    the combinator's own structural walk of each registered atom — see above — not from a
    router hint, so the [ROUTER] carries only ownership + the sort-appropriate equality
    split.)

    {b Soundness backbone (INVARIANTS.md I4/I8; DESIGN §6, Session SOUNDNESS RULE).} The
    combinator NEVER fabricates an assertion: it only ever forwards literals the engine
    placed on the trail. A shared equality becomes asserted to both theories exactly when
    the SAT core decides one of the [Split] disjuncts, so every conflict a child later
    derives from that equality lists the split literal in its premises — provenance is by
    construction, never "from thin air". Explanations pass through unchanged: premises are
    already the global {!Oxsmt_core.Lit.t} currency (one engine allocator, ADR-0005 D2),
    so a child's premise set is valid combined currency with no translation. When the
    combinator cannot certify a state (a child returns [Sat] with a model that violates an
    already-decided shared equality — a child bug), it raises {!Combination_unsound},
    which the engine's CONTRACT-POISON handling turns into verdict [unknown] (never a
    verdict from unsound state).

    {b Determinism (I6, ADR-0005 C1–C8).} Every observable output is a deterministic
    function of the register/assert/check/push/pop sequence: children are consulted A then
    B; shared terms are iterated in {!Oxsmt_core.Term} tag order; the split pair chosen is
    the tag-least disagreeing pair.

    N children compose by nesting ([Combine (R2) (Combine (R1) (A) (B)) (C)]); binary
    matches the frozen [Combine (A) (B)] shape in {!Oxsmt_core.Theory}. *)

open Oxsmt_core

(** Which child(ren) an atom belongs to, and the material for the shared-equality split.
    Instantiated per theory pair (QF_UFLIA: [A] = EUF, [B] = LIA). *)
module type ROUTER = sig
  (** [A]/[B] = owned by exactly one child. [Both] = a shared interface equality,
      registered into A {e and} B alike, so an equality the SAT core settles via a
      {!Oxsmt_core.Theory.Split} reaches both theories.

      {b NB — there is no purification pass in this pipeline} (smt/preprocess does
      ite/divmod/simplify only; purification was deferred, ADR-0005), so atoms are NOT
      guaranteed pure. The combinator's robustness on mixed atoms comes not from purity
      but from (a) [owner]'s structural dispatch classifying by head/sort, (b) the
      combinator's own structural INTERFACE WALK, which records every boundary-crossing
      node of each registered atom — a mixed atom is routed to one theory yet its foreign
      Int subterms still surface as interface members, never silently dropped — and (c)
      the combinator EVALUATING (folding) a compound term like [x + 1] through a child's
      leaf-only model rather than requiring the child to key it (so a pinned equality over
      a compound is verified, not degraded — see [check_pins]/[model]). A future
      maintainer must not reintroduce a purity assumption. *)
  type owner =
    | A
    | B
    | Both

  (** [owner term] is the register-time classification: the UNION of children that may
      receive this atom at {e any} polarity. Drives [register_atom] fan-out. A pure,
      deterministic function of the term (idempotent across [register_atom]; I6). *)
  val owner : Term.t -> owner

  (** [assert_to term ~positive] is the assert-time routing of the SIGNED literal — a
      subset of [owner term]'s children. It lets a router withhold a polarity a child
      cannot accept: e.g. LIA raises [Unsupported] on a disequality, so a NEGATIVE shared
      equality routes to the congruence child ([A]) ONLY. Soundness is preserved (the
      congruence child handles diseq natively) and completeness via the split mechanism —
      if the arithmetic child's candidate model later equates the pair while the
      congruence child holds them apart, that is a shared-pair disagreement whose
      ℤ-trichotomy split carries the ordering to the arithmetic child. MUST agree with
      [owner] on the positive polarity for a shared equality (so registration and
      assertion match). *)
  val assert_to : Term.t -> positive:bool -> owner

  (** The single arithmetic sort owned by child B. *)
  val arithmetic_sort : Sort.t -> bool

  (** [equality_split ctx x y] is the disjunction the combinator emits as a
      {!Oxsmt_core.Theory.Split} when the two candidate models disagree on [x = y]: a
      clausified, genuinely-constraining set of {b ≥2 DISTINCT} atoms whose disjunction is
      valid (ADR-0005 CONTRACT-SPLIT). For Int this MUST be the ℤ-trichotomy
      [x = y ∨ x < y ∨ x > y] — NOT [x = y ∨ x ≠ y], which is [A ∨ ¬A] and is silently
      dropped by the SAT core's level-0 tautology removal (ADR-0005 §1a; freeze-package
      finding B1), leaving the split with no constraining power. Terms are built through
      [ctx] so they share the session tag stream / hash-consing (D6). *)
  val equality_split : Context.t -> Term.t -> Term.t -> Term.t list
end

(** Raised (→ engine CONTRACT-POISON → verdict [unknown], soundness bias I8) whenever the
    combinator cannot soundly certify a state — a SOUNDNESS degrade; never raised by
    correct children. The cases:
    - a child certifies [Final]→[Sat] over a model that violates an asserted shared
      equality it was routed (codex C2 pin check);
    - a child returns a consistent-but-non-[Sat] result at a full model (empty
      propagations is not a [Sat] certificate, codex C4);
    - the merged model cannot give an Int-sorted asserted term an integer value (codex
      C3);
    - [assert_lit]/[explain] names an atom [register_atom] never saw (engine contract). *)
exception Combination_unsound of string

(** Raised (→ verdict [unknown]) when the combinator meets a shape it handles SOUNDLY but
    INCOMPLETELY — a deliberate completeness degrade, {b distinct from}
    {!Combination_unsound} so a caller/log can tell "we chose not to decide this" from "a
    child violated its contract". A Boolean argument of an uninterpreted function
    (internalization ADR §3.6) raises in exactly two situations:
    - {b at assert time} — a STRUCTURED Boolean compound (an [And]/[Or]/[Not]/[Ite] or a
      Bool-[Eq]/iff, and a LIA order atom [Le]) as a UF argument, e.g. [h (b ∧ c)] (case
      (ii)): the leaf bridge names a nullary [K_bool] leaf, so a compound would decouple
      from its operands and wrong-SAT.
    - {b at Sat certification} — a BURIED Bool leaf ([h p], [p] a bare Bool variable) or a
      buried Bool-returning application ([h (g z)], [g : … → Bool]) that is NOT bound to
      [true]/[false] in the congruence child, i.e. never surfaced as a SAT atom (codex
      H2 + sibling): such a term would stay a third opaque Boolean class and wrong-SAT. A
      Bool CONSTANT ([h true]/[h false]) and a SURFACED (bound) Bool leaf do NOT raise —
      they are native / decidable (§3.6 cases (i')/(i)). The precise handling of the
      degraded shapes is the deferred Tseitin coupling (ADR §9 [bool-compound-uf-args]). *)
exception Incomplete of string

(** The congruence child [A]. It is an ordinary {!Oxsmt_core.Theory.THEORY} plus
    {!CONGRUENCE_CHILD.internalize_term}: the combinator internalizes a boundary term that
    surfaces only inside the OTHER child's atom (a LIA order atom's [App] subterms) so the
    congruence engine's model values it and closes congruence over it — the W1/R1 fix. The
    asymmetry (only [A] needs it) is intrinsic: LIA reaches a shared Int term through its
    own leaf/fold machinery, while EUF has no channel to a term it never saw as an atom. *)
module type CONGRUENCE_CHILD = sig
  include Theory.THEORY

  (** [internalize_term t term] internalizes [term] and its subterm closure into the child
      with NO atom binding — never asserted, watched, propagated, or explained; only made
      visible to the child's model + congruence. Idempotent; undone by [pop] of the
      introducing frame. *)
  val internalize_term : t -> Oxsmt_core.Term.t -> unit
end

type edge_id = Fabric.edge_id

type justification = Fabric.justification =
  | Real of Lit.t
  | Fabric of edge_id

module Fabric_explanation = Fabric.Explanation

type fabric_check_result = Fabric.check_result =
  | Sat
  | Propagations of Lit.t list
  | Conflict of Fabric_explanation.t
  | Split of Term.t list
  | Lemma of (Term.t * bool) list

(** Rich, non-frozen child seam used only inside the combinator. *)
module type FABRIC_CHILD = sig
  include Theory.THEORY

  val check_fabric : t -> Theory.effort -> fabric_check_result
  val explain_fabric : t -> Lit.t -> Fabric_explanation.t
  val fixed_bounds : t -> Term.t -> Fabric.fixed_bounds option

  (** F1-SEM independent re-verifier: confirm [term] is fixed to [value] with the two
      given oriented-bound premises, by a path separate from {!fixed_bounds}. *)
  val fabric_verify : t -> Term.t -> string -> justification -> justification -> bool

  (** ADR-0014 Stage 2 (§A.3): react to a hub [new_eq] by asserting the (pre-built) Int
      equality atom into this theory, attributed to the fabric edge. Pure mutation on the
      theory's own trail (F3 co-location). *)
  val notify_eq : t -> edge_id:edge_id -> Term.t -> unit

  (** ADR-0014 Stage 4.2: sub-frame checkpoint/rewind for chrono earliest-removed
      incremental undo (a watermark on the child's own undo trail; [rewind_to_checkpoint]
      drains it back without touching the frame stack). *)
  type checkpoint

  val checkpoint : t -> checkpoint
  val rewind_to_checkpoint : t -> checkpoint -> unit
end

module type FABRIC_CONGRUENCE_CHILD = sig
  include FABRIC_CHILD

  val internalize_term : t -> Term.t -> unit
  val fabric_are_equal : t -> Term.t -> Term.t -> bool
  val assert_fabric_eq : t -> edge_id:edge_id -> Term.t -> Term.t -> unit

  (** ADR-0014 Stage 2/3: the hub's multi-consumer merge log, per-class tag slot, and the
      congruence-explanation accessor for the [new_eq] justification (§A.3/§A.4). *)
  type merge_cursor

  val set_record_merges : t -> bool -> unit
  val add_merge_consumer : t -> merge_cursor
  val drain_merges : t -> merge_cursor -> Fabric.merge_event list
  val set_class_tag : t -> Term.t -> Term.t -> unit
  val class_tag : t -> Term.t -> Term.t option
  val fabric_explain_eq : t -> Term.t -> Term.t -> justification list
end

(** [Combine (R) (A) (B)] is the combined theory ([A] = the congruence child, [B] =
    arithmetic). Its [model] (valid after [Final]→[Sat]) is the sort-directed merge of the
    children's models: each term takes the child value whose variant matches the term's
    sort (Int from the arithmetic child, [Uninterp] from the congruence child), so the
    single witness respects the arrangement both children agreed on (CONTRACT-MODEL). *)
module Combine (R : ROUTER) (A : FABRIC_CONGRUENCE_CHILD) (B : FABRIC_CHILD) : sig
  include Theory.THEORY

  (** ADR-0012 L2/O3 (tranche 2): a read-only accessor for the congruence child's state,
      so {!Oxsmt_interface} can build the lemma-tier e-graph query view over the concrete
      {!Oxsmt_euf.Euf_adapter}. The abstract [Theory.THEORY] seam the CDCL(T) engine
      drives is unchanged — this only widens the {e combinator's own} interface (not
      frozen), it does not add a THEORY method. The matcher reads through the child's
      non-registering query API; it never mutates the e-graph (R6). *)
  type congruence_state = A.t

  val congruence_state : t -> congruence_state

  (** Symmetric to {!congruence_state}: a read-only accessor for the ARITHMETIC child's
      state ([B]), so {!Oxsmt_interface} can read LIA-specific observational evidence —
      the theory unsat core / Farkas coefficients of the most recent conflict (task #106)
      — after an UNSAT. Widens only the combinator's own (non-frozen) interface, never the
      abstract [Theory.THEORY] seam the CDCL(T) engine drives; the accessor is
      non-mutating. *)
  type arith_state = B.t

  val arith_state : t -> arith_state

  type fabric_stats =
    { edges_injected : int
    ; pairs_skipped : int
    ; expansions : int
    }

  val fabric_stats : t -> fabric_stats
  val set_fabric_trace : t -> Fabric.trace option -> unit

  (** Whether a cross-theory justification edge is currently live. An EUF certificate
      leaf may be claimed as pure congruence only while this is false; once an edge is
      live, an [Euf_congruence] explanation can contain expanded arithmetic evidence. *)
  val has_live_fabric_edges : t -> bool

  (** ADR-0014 Stage 4.2 sub-frame checkpoint/rewind (chrono earliest-removed incremental
      undo): capture both children's checkpoints + the pin/fabric trail watermarks;
      [rewind_to_checkpoint] restores all four to an absolute watermark without touching
      the frame stack. *)
  type checkpoint

  val checkpoint : t -> checkpoint
  val rewind_to_checkpoint : t -> checkpoint -> unit

  (** Test-only access to the fabric edge registry + recursive handle expansion, so the
      shared-ancestor (DAG diamond) and genuine-cycle behaviours of
      [expand_justifications] can be driven directly. Not used by any shipping path. *)
  module For_testing : sig
    val register_edge
      :  t
      -> edge_id
      -> justification list
      -> s:Term.t
      -> tt:Term.t
      -> unit

    val expand : t -> justification list -> Lit.t list
  end
end
[@@warning "-67"]
(* -67: the result signature (abstract [t]) does not mention the functor parameters, so
   they read as "unused" in the declaration; they are used in the body. *)
