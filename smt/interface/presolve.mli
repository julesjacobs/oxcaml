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
