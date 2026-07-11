(** Session API (DESIGN.md §3): declare symbols, assert terms, check-sat, push/pop.

    A session bundles one {!Oxsmt_core.Env.t} + {!Oxsmt_core.Context.t}, a
    {!Oxsmt_preprocess.Preprocess} handle, the {!Oxsmt_preprocess.Cnf} clausifier, and the
    {!Oxsmt_solver.Sat} core. All term construction threads the one context (ADR-0003
    Decision 6), so the same atom is the same SAT variable throughout the session.
    Shipped, stdlib-only (INVARIANTS.md I3): it never links the test-only SMT-LIB parser.

    {b THE SOUNDNESS RULE (v1 is a propositional core, not yet a theory solver).}

    {i Why it holds — the boolean skeleton over-approximates.} Clausification replaces
    each theory atom by a fresh Boolean variable, dropping every theory constraint that
    ties those variables to arithmetic/congruence. So the propositional problem is a
    {b relaxation} of the real one: every real model induces a propositional model (read
    off the truth value each atom takes under the real interpretation), but not conversely
    — a propositional model may assign the atoms in a way no theory interpretation allows
    (it can set both [x < 0] and [x > 0] true, which are independent Booleans to the SAT
    core). Two consequences, asymmetric:

    - {b Propositional [Unsat] ⇒ real [Unsat] (sound, even with theory atoms).} If not
      even the relaxation has a model, the (more constrained) real problem has none
      either. The SAT core cannot see theory constraints, but adding them only removes
      models, never adds them.
    - {b Propositional [Sat] proves nothing} until a theory vets the atom assignment: the
      witness may be theory-inconsistent. So with any theory atom present, [Sat] must
      become [Unknown] — reporting [Sat] would be unsound.

    Concretely:

    - If the asserted formulas contain {b any theory atom} — an order atom [Le], a
      non-Bool equality [Eq], or an applied predicate [App] of arity ≥ 1 — then a
      propositional [Sat] verdict is {b downgraded to [Unknown]} (per the argument above;
      e.g. the skeleton of [x < 0] ∧ [x > 0] is satisfiable though the theory is not).
    - Propositional [Unsat] is reported as [Unsat] {b even with theory atoms present}
      (sound by the first consequence above).
    - A {b pure-Boolean} formula (every atom is a propositional variable — a nullary
      Bool-sorted symbol — or a Bool constant) gets a real [Sat]/[Unsat].
    - {!Term.Overflow}/{!Term.Unsupported} anywhere in preprocessing or clausification
      degrade the whole session to [Unknown] (INVARIANTS.md I8, session boundary): never a
      crash, never a partial verdict.

    When the EUF and LIA theories land (M2/M3) this rule is what they relax; until then it
    is the wall that keeps v1 sound. *)

type t

type verdict =
  | Sat
  | Unsat
  | Unknown

(** A fresh session: empty env (with the reserved [div]/[mod] built-ins), fresh context
    and SAT core, one active (base) assertion frame. *)
val create : unit -> t

(** The session's {!Oxsmt_core.Env.t}. Exposed so a front end (e.g. the test-only SMT-LIB
    parser) can declare symbols and build assertion terms in the {e same} context the
    session solves over. *)
val env : t -> Oxsmt_core.Env.t

(** The session's {!Oxsmt_core.Context.t} (same rationale as {!env}). *)
val context : t -> Oxsmt_core.Context.t

(** [declare_sort]/[declare_fun]/[declare_const] declare into {!env}. They reject the
    reserved fresh-symbol namespace ([".oxsmt.*"], board #48) with [Invalid_argument] so a
    user symbol cannot collide with one preprocessing invents. *)
val declare_sort : t -> string -> Oxsmt_core.Symbol.t

val declare_fun : t -> string -> Oxsmt_core.Rank.t -> Oxsmt_core.Symbol.t
val declare_const : t -> string -> Oxsmt_core.Sort.t -> Oxsmt_core.Symbol.t

(** [assert_term t phi] preprocesses [phi] (ADR-0003 §5 passes), clausifies the boolean
    skeleton, and adds the clauses to the current frame. [phi] must be Bool-sorted and
    built through {!context}. A theory atom in [phi] is recorded (see THE SOUNDNESS RULE);
    an [Overflow]/[Unsupported] degrades the session to [Unknown]. Legal before or after
    {!check_sat} (assert-after-check). *)
val assert_term : t -> Oxsmt_core.Term.t -> unit

(** Open a new assertion frame. Assertions added until the matching {!pop} are retracted
    by it. Implemented with a fresh selector variable: frame clauses are guarded by the
    selector, which {!check_sat} assumes true while the frame is active (standard
    MiniSat-style retraction — nothing is physically removed). *)
val push : t -> unit

(** Close the innermost frame, deactivating its assertions. [Invalid_argument] if there is
    no matching {!push}. *)
val pop : t -> unit

(** Decide satisfiability of the active assertions under THE SOUNDNESS RULE. Repeatable;
    more assertions or push/pop may follow. *)
val check_sat : t -> verdict

(** The model of the most recent {!check_sat}, iff that call returned a {e real} [Sat] (a
    pure-Boolean formula — see THE SOUNDNESS RULE). Bindings are [(symbol-name, value)]
    for every propositional variable, sorted by name. [None] after [Unsat]/[Unknown],
    after a theory-downgraded [Sat], or before any [check_sat]. *)
val get_model : t -> (string * bool) list option

(** The SAT core's counter trio, monotonic across the session (DESIGN.md §8). *)
val stats : t -> Oxsmt_solver.Sat.Stats.t
