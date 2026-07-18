(** W1b unconditional equality-elimination presolve (logs/w1b-design.md; the gate-cleared
    QF_LIA lever). A Session-level preprocessing pass that runs BEFORE internalization: it
    rewrites the asserted set feeding the combinator, eliminating top-level unconditional
    aliases so the theory stack sees a smaller, equisatisfiable problem.

    {b What it eliminates.} A top-level conjunct that is [(= x t)] where [x] is a bare Int
    variable, [x] does not occur in [t], and [x] is not an ADR-0010 interface variable.
    The defining conjunct is dropped and [x ↦ t] is substituted (to a fixpoint) into every
    other conjunct. Equisatisfiable by construction: [x] is fully determined by [t], so
    each constraint on [x] becomes the identical constraint on [t] and the only assert
    removed is the redundant definition.

    {b Soundness constraints (logs/w1b-design.md §"Build-soundness constraints").}
    - {b Unconditional only.} Only aliases that are top-level conjuncts of the assertion
      set (a root [And] is flattened; the pass descends NO further — an [Eq] under
      [Or]/[Ite]/[Not] is never eliminated).
    - {b Interface no-op (ADR-0010 §3.1).} A variable that occurs under an uninterpreted
      function (or whose alias would move a UF application) is NOT eliminated —
      eliminating an interface variable would perturb the combinator's boundary/shared-set
      computation (a wrong-SAT surface on QF_UFLIA). For pure QF_LIA the interface set is
      empty, so this guard is vacuous — the full elimination applies.
    - {b Deterministic (I6).} Elimination order is a deterministic function of assertion
      order (first-wins on repeated definitions; cycles are broken by keeping the later
      alias as an ordinary constraint).

    {b Model reconstruction.} The pass returns the eliminated variables as an ordered
    {!def} list (elimination order); {!Session} re-derives each eliminated variable's
    value from its {!def.value} at model-build time so the R1 in-process checker (which
    evaluates the ORIGINAL assertions) and [get_model] both bind it. Each {!def.value} is
    resolved over surviving variables only (chained definitions are composed out), so
    re-derivation needs no ordering between defs.

    Pure over {!Oxsmt_core.Term.t}: no theory, no {!Cdclt} — the transform is a term
    rewrite through the session {!Context}'s smart constructors, so every produced term is
    well-sorted and hash-consed. *)

open Oxsmt_core

(** One eliminated variable: its [name] (the bare-variable symbol name), its [sort]
    (always Int in v1), and its defining [value] — a term over the SURVIVING variables
    only (all other eliminated variables have been substituted out), evaluated at
    model-build to recover the variable's value. *)
type def =
  { name : string
  ; sort : Sort.t
  ; value : Term.t
  }

type result =
  { reduced : Term.t list
    (* the conjuncts to internalize: the flattened top-level conjuncts with every
         eliminated definition dropped and [x ↦ t] substituted to a fixpoint. When nothing
         is eliminable this is the ORIGINAL assertion list unchanged (byte-for-byte
         neutral). *)
  ; defs :
      def list (* eliminated variables, in ELIMINATION order (first eliminated first) *)
  }

(** [entailed_equalities ctx assertions] is Pass A (task #7): the equalities entailed by
    top-level disjunctions, to ADD as extra top-level unit assertions. For each top-level
    conjunct [(or D_1 … D_k)] it returns the equalities holding in the induced
    equality-closure of {e every} disjunct (hence entailed by the [or]), as a spanning
    forest of fresh [Eq] terms built through [ctx]. Sound (equisatisfiable, both
    directions): adds only entailed facts; eliminates no variable and introduces no new
    term, so the model surface is unchanged (R1 checks the ORIGINAL assertions — the added
    units must NOT be recorded in the R1 set).

    Grammar (soundness-critical: the win direction is UNSAT, where R1 does not run):
    within a disjunct it descends through [And] only; a leaf is a positive [Eq(a,b)]
    between NON-Bool terms; OPAQUE at [Eq]-operands, [Not], [Or], [Ite], and Bool/iff
    equalities. Each disjunct's closure is an INDEPENDENT union-find; the intersection is
    same-class-in- every-branch (never a cross-branch merge); emits a spanning forest.
    Single pass (no fixpoint). Neutral-abort (returns a subset, never fails) on an
    equality-free disjunct or a hard term/leaf cap. Pure and deterministic (I6). The
    caller gates it (flag + cert-OFF). *)
val entailed_equalities : Context.t -> Term.t list -> Term.t list

(** [run ctx assertions] presolves [assertions] (raw asserted terms, built through [ctx]).
    Total and deterministic: on a zero-alias input it returns
    [{ reduced = assertions; defs = [] }] (exact neutrality). All produced terms are built
    through [ctx]'s smart constructors. *)
val run : Context.t -> Term.t list -> result

(** [simplify_contextual ctx assertions] contextually simplifies each assertion (task
    #13): within an [(ite c a b)] it assumes [c] true while rewriting [a] and false while
    rewriting [b], folding a literal reoccurrence of the (peeled) condition atom to its
    truth value and — for a [(= v k)] condition with [v] a bare non-reserved variable and
    [k] a literal — substituting [v ↦ k] in the then-branch only. The rewrite is
    model-PRESERVING (logically equivalent, eliminates no variable), so no model
    reconstruction is needed and the R1 self-check (which evaluates the ORIGINAL asserted
    terms) is unaffected. All produced terms are built through [ctx]'s smart constructors,
    which fold the collapsed conditions automatically.

    Soundness (the win direction is UNSAT, where R1 does not run — these are the margin):
    the then/else assumptions are scoped strictly to their own branch (never the sibling,
    the condition, or above the [ite]); the equality substitution fires only for the TRUE
    polarity of an [Eq]; and each branch scope memoizes independently, so a fold under one
    branch's assumptions never leaks to another scope. Neutral-abort on a hard node-visit
    budget (returns the ORIGINAL list unchanged) and a pass-through for any assertion with
    no [Ite] (exact neutrality). Pure and deterministic (I6). The caller gates it (flag +
    cert-OFF). *)
val simplify_contextual : Context.t -> Term.t list -> Term.t list

(** [simplify_projection ctx assertions] rewrites each assertion with three pure,
    model-PRESERVING ITE identities on the shared hash-consed DAG (task #34) — a DIFFERENT
    algorithm from {!simplify_contextual} (no assumption-context tracking):

    - EQUALITY-OVER-ITE PROJECTION: [(= (ite c x y) d)] -> [(ite c (= x d) (= y d))] when
      [d] is a leaf (constant or bare variable), symmetric in the operands. This turns a
      nec-smt condition [(= chain_ite literal)] into a boolean function of the ORIGINAL
      condition atoms (rather than a fresh opaque Tseitin aux var), reproducing the DAG
      collapse z3 does in preprocessing (mk-bool-var 1 on the exemplars).
    - BOOLEAN-ITE COLLAPSE: a projected Bool-sorted [(ite c true false)] etc. is collapsed
      to [c] / [(not c)] / [and]/[or] (the {!Oxsmt_core.Node} [ite] constructor does not).
    - LOCAL SELECTOR COLLAPSE (depth-1): [(ite c a b)] drops a nested same-condition (or
      complement) [ite]'s dead sub-branch.

    All three are logical EQUIVALENCES (total ITE semantics) and eliminate NO variable, so
    — like {!simplify_contextual} — no model reconstruction is needed and the R1
    self-check (over the ORIGINAL asserted terms) is unaffected; unlike it, there is no
    scope-leak surface (these are context-free local identities). Memoized over the DAG
    and on the projection's [(ite-tag, leaf-tag)] pair; neutral-abort on a hard step
    budget (returns the ORIGINAL list unchanged); pass-through for any ITE-free assertion
    (exact neutrality). All terms are built through [ctx]'s smart constructors. Pure and
    deterministic (I6). The caller gates it (flag + cert-OFF). *)
val simplify_projection : Context.t -> Term.t list -> Term.t list

(** [symmetry_break ctx assertions] detects interchangeable same-sort constants (constants
    whose pairwise transposition maps the asserted set to itself) and returns extra
    top-level Bool constraints — full-action generator-based LEX-LEADER symmetry breaking
    over the ORIGINAL vocabulary (theory equalities only) — to internalize ALONGSIDE the
    assertions (task #25, quf-symmetry-experiment.md §6). Returns [[]] when nothing is
    broken (byte-neutral to a no-symmetry input).

    {b General / structural (no family logic).} Same-sort constants are refined by a cheap
    occurrence signature; each candidate transposition is then CONFIRMED EXACTLY by
    rebuilding every conjunct under the swap through [ctx] (whose smart constructors
    AC-normalize [and]/[or]/[eq], so the hash-cons tag is the canonical form) and
    comparing the tag multiset to the original. Interchangeable classes are the connected
    components of the confirmed-transposition graph.

    {b Emission is a SOUND lex-leader, never value precedence.} Value precedence is proven
    UNSOUND for this index+value symmetry (constants appear as both function arguments and
    values → real SAT→UNSAT flips). For each class and each adjacent generator [g] the
    pass requires the assignment [A] to be lexicographically <= [g(A)] over a fixed atom
    sequence, encoded with an O(n) prefix-equal chain carried by fresh reserved
    [".oxsmt.sym.*"] nullary Bool aux vars (see the last paragraph). [A ⪯ g(A)] keeps >=1
    representative per orbit ⇒ SAT-preserving; adding constraints ⇒ UNSAT-preserving.

    {b Size cap (sat-safe).} Emission is skipped for classes of size >= 6 (offline A/B:
    the size-6/7 classes regress the satisfiable instances they touch for ~0 conversion).

    {b Equisatisfiable, not equivalent.} The returned constraints REMOVE symmetric models,
    so the caller must NOT record them in the R1 self-check set (a found model still
    satisfies the ORIGINAL assertions) and MUST gate the pass cert-OFF (a lex-leader
    clause is not resolution-derivable into a certificate). Pure and deterministic (I6,
    tag order); neutral-abort (returns [[]]) on any hard budget. All terms are built
    through [ctx].

    The lex prefix-equal chain uses fresh reserved [".oxsmt.sym.*"] nullary Bool aux vars
    (an O(n) encoding; minted through the cap-gated [Env.declare_reserved] with
    [cap]/[env], kind ["sym"], disjoint from preprocessing's fresh symbols). [~counter] is
    a caller-owned monotone name counter: it MUST persist across calls on one [env]
    (idempotent [declare_reserved] would otherwise rebind [.oxsmt.sym.0] to a second,
    conflicting definition on a repeat call — wrong-unsat). *)
val symmetry_break
  :  counter:int ref
  -> Env.reserved_cap
  -> Env.t
  -> Context.t
  -> Term.t list
  -> Term.t list
