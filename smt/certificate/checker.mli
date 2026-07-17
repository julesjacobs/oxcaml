(** Certificate replay CHECKER (ADR-0013 step 2) — turns the {e search} of the untrusted
    solver into a {e check}.

    It consumes a recorded event stream (the frozen {!Oxsmt_solver.Sat.trace} seam
    surfaced by {!Recorder}) and VALIDATES the propositional resolution skeleton of an
    [Unsat]:

    - {b Input well-formedness / id-resolvability (kind-aware).} Every clause [id] a hint
      or the terminal conclusion cites resolves to EXACTLY ONE content event — an
      [on_input], an [on_learned], or an [on_theory_clause] —
      {e of the kind the citation site requires}. A dangling id, an ambiguous id (two
      clauses under one id — the cross-solver misuse), or a wrong-KIND citation (e.g. a
      [Root_empty] citing a learned event's id) is [Invalid]. This is the kind-keyed
      resolution the step-1 recorder deferred (board #153a): the recorder's
      [unresolved_citations] counts occurrences only; the checker keys (kind, id).
    - {b Level-0 unit closure (§1.3).} The checker re-derives the level-0 unit closure by
      confluent BCP-to-fixpoint over the {e axiom} clauses (query/lemma inputs + theory
      leaves), and every declared [on_unit] must fall inside it (re-derived, not trusted;
      cannot spuriously reject — the closure is a superset of any declared unit).
    - {b Learned-clause ordered RUP (§1.4).} Each learned clause replays by
      {e ordered, hint-restricted} RUP over its recorded antecedents [rₙ..r₁; conflict] —
      each cited clause must be unit, falsified, or already satisfied (skipped) at its
      turn — see the appendix below; the checker never SEARCHES for a propagation. If the
      hinted ordered chain does not close, the checker FALLS BACK to full-closure RUP
      (task #56, appendix): the clause is accepted iff [base + ¬clause] derives ⊥ by
      unrestricted BCP fixpoint over the whole verified closure ([refutes_under]). The
      hint chain is thus the fast path; the ground truth is closure-entailment.

    {b ADR-0013 appendix (satisfied-hint skip, task #42).} A cited clause that is already
    SATISFIED at its turn is a no-op and is SKIPPED, not rejected — the checker accepts
    NON-MINIMAL (but still ordered) hint chains. Soundness: a satisfied clause forces no
    literal, so skipping it removes no inference and is exactly equivalent to the emitter
    having omitted that antecedent; the derivation the remaining hints close is unchanged,
    and a chain that ends without a conflict still fails. This is the standard
    drat-trim-style treatment of satisfied antecedents. It is needed because the LIA-heavy
    analyze records antecedents whose unit literal an earlier antecedent already delivered
    (theory-propagated literals' lazy explain reasons overlap the Boolean resolution
    chain). The skip fires ONLY on an id the kind-keyed resolver already accepted (never a
    dangling / ambiguous / wrong-kind / unverified-learned id), and the "never SEARCHES"
    contract is untouched for the unit / ≥2-free cases.

    {b ADR-0013 appendix (learned-clause full-closure RUP fallback, task #56).} When the
    hint-restricted ordered chain does NOT close (consumed / not-unit / all-satisfied),
    the checker falls back to full RUP over the ENTIRE verified closure: accept iff
    [base + ¬clause] derives ⊥ by unrestricted BCP fixpoint over [bcp.db]
    ([Bcp.refutes_under bcp] seeded with the clause's literals negated). This is the
    direct sibling of the E1/E2 terminal fallback (task #47), and it UNIFIES all replay
    sites — E1 [Root_empty], E2 [Level0_conflict], E3 [Failed_assumption], and
    learned-clause ordered-RUP — on ONE acceptance criterion: the cited chain/witness is
    ADVISORY, and the ground truth is UP-derivability of ⊥ from the admitted axioms +
    earlier-verified learned clauses. Needed because the emitter records the antecedent
    chain valid in the SOLVER's incremental level-0 state, while the checker's batch
    closure over the full theory/cut leaf union can SATISFY a cited antecedent (a literal
    flips true vs solver state — task #52, rings id-6571/6572), stranding the hinted chain
    even though the clause is genuinely entailed. SOUND: [bcp.db] at a learned clause's
    turn holds ONLY admitted axioms + learned clauses verified EARLIER in the loop (each
    folded only after acceptance — the CRIT-1 emission-order invariant, load-bearing here:
    no self / forward citation can enter the fallback DB). Citation WELL-FORMEDNESS stays
    a HARD gate on the fallback (every cited id must resolve and, if learned, be
    already-verified), so a dangling / ambiguous / forward-or-self citation is rejected
    regardless of entailment (CRIT-1 defense in depth). An unentailed clause derives no ⊥
    and is still rejected (no accept-invalid). BCP fixpoint is not SEARCH. CONSEQUENCE:
    the cited chain being advisory means a well-formed but incomplete / mis-ordered chain
    on an ENTAILED clause is now VALID — this is the intended unification, not a soundness
    relaxation.

    {b The explicit trade — chain quality becomes a monitored METRIC, not a validity
      criterion.}
    Soundness = closure entailment (checked here); hint-chain quality no longer gates
    validity, but it stays OBSERVABLE via {!fallback_firing_count} (surfaced on the
    corpus-gate summary line). A sudden rise in fallback firings flags a degraded /
    drifting emitter — chains that no longer replay in hint-restricted order — WITHOUT
    failing soundness. So the checker keeps a signal on emitter health while no longer
    over-rejecting valid non-minimal chains.

    {b COMPLETION of the advisory-witness principle.} With #42 (satisfied-hint skip), #47
    (E1/E2 terminal fallback), and #56 (learned-clause fallback), ALL FOUR replay sites —
    E1 [Root_empty], E2 [Level0_conflict], E3 [Failed_assumption], and learned-clause
    ordered-RUP — now share ONE acceptance criterion: the recorded hint / cited witness is
    ADVISORY (a fast path and a monitored quality metric), and validity is exactly
    UP-derivability of ⊥ from the admitted axioms + earlier-verified learned clauses
    ([refutes_under] over the verified closure). The checker's trust basis is the verified
    closure, uniformly, everywhere.

    {b Shape (b) — emitter-minimal reverse-propagation-ordered chains — is PERMANENTLY
      DEFERRED (disproven).}
    Named as the faithful long-term option at #42/#47 and expected to be motivated by its
    "first genuine trigger" (cut lemmas, task #52), the trigger instead DISPROVED it: the
    solver-incremental vs checker-batch-closure divergence is inherent, so no
    statically-emitted antecedent chain can be simultaneously ordered-RUP under both the
    solver's state and the checker's recomputed batch closure. The checker-side
    full-closure fallback is the correct resolution.
    - {b Theory leaves (§1.5).} A LIA [Conflict] carrying a Farkas witness is checked
      independently: its premise literals must be exactly the emitted clause's negation,
      every multiplier must be nonnegative, and the weighted integer half-planes must
      cancel every variable and leave a strictly positive constant. A pure-EUF
      [Reason]/[Conflict] witness is checked by negating its exact clause and rebuilding
      congruence closure from the cited atom statements: reflexivity/symmetry/transitivity
      plus congruence over matching applications must collapse a cited disequality. A
      claimed but bad witness is [Invalid], never silently trusted. Unwitnessed theory
      leaves and [Theory_lemma] inputs remain trusted axioms and force
      [Valid_modulo_theory_leaves]. An empty [Conflict] clause
      (an unconditional
      [T_conflict []], ADR-0013 Rev 6) has NO v1 leaf witness for ⊥-from-∅ and is reported
      [Unsupported], not [Valid].
    - {b Terminal conclusion (§4.0 E1–E4).} [Root_empty] / [Level0_conflict] check the
      cited clause is falsified by the level-0 closure, OR —
      {b ADR-0013 appendix (E1/E2 cited-clause fallback, task #47)} — that the level-0
      closure is GLOBALLY INCONSISTENT (BCP over the whole closure derives ⊥, i.e.
      [refutes_under] with no assumptions). The fallback is needed because the emitter
      cites the clause falsified in the SOLVER's incremental level-0 state, while the
      checker's batch closure over the full theory-leaf union can reach ⊥ through a
      different clause and force a variable that SATISFIES the cited one (rings id-7866).
      The cited id is therefore ADVISORY for E1/E2 (the same relaxation philosophy as the
      non-minimal ordered chains above); the acceptance criterion stays "a genuine
      unit-propagation derivation of ⊥ from validated clauses", identical to the trust the
      checker already places in the closure for the forward falsification check and for
      E3. A consistent closure still fails both disjuncts and is rejected. This unifies
      E1/E2 with the E3 [refutes_under] idiom. [Failed_assumption] replays the
      assumption-forcing chain by ordered RUP seeded with the solve's assumption literals
      true (the OCaml-side equivalent of the §1.0 selector strip: an assumed-true
      selector's [¬sel] literal is false throughout, so ordered RUP over the guarded
      clauses derives the same conflict the stripped [||] step would — no separate strip
      pass is needed to CHECK; the explicit strip-to-[||] is a Lean-bridge concern, step
      3).

    {b FAIL-CLOSED.} Anything unrecognized, dangling, ambiguous, wrong-kind, or
    unreplayable is [Invalid] (or [Unsupported] for a well-formed feature this checker
    version cannot witness) — never skipped-as-valid.

    Stdlib-only; reads only the recorder's accessors + the frozen {!Oxsmt_solver.Sat} lit
    algebra (dependency firewall I3). Independent of the solver's search. *)

module Sat = Oxsmt_solver.Sat

type verdict =
  | Valid_modulo_theory_leaves
  (** the resolution skeleton closes, but at least one [Reason]/[Conflict] clause or
      [Theory_lemma] input has no checked witness and is still trusted as T-valid. *)
  | Valid
  (** the skeleton closes and every theory leaf is checked. This includes theory-free
      propositional certificates and certificates whose theory leaves are verified pure
      EUF clauses or LIA Farkas [Conflict] clauses. *)
  | Invalid of string (** an artifact-attributable rejection (ADR-0013 §3.3) *)
  | Unsupported of string
  (** a well-formed leaf/feature this checker version cannot witness (coverage gap) *)

(** The self-contained refutation the checker consumes: exactly the recorded event stream
    (ADR-0013 step 1) plus the assumption literals the traced solve ran under.

    [atoms] is the off-frozen-seam statement map from SAT theory variables to their
    immutable atom terms. It is separate from leaf witnesses so a corrupted EUF or
    Farkas proof cannot redefine the proposition it claims to prove; duplicate variable
    declarations are [Invalid].

    [assumptions] are the literals passed true to {!Sat.solve} (for a session solve, the
    active frame selectors [List.map Sat.pos frames]); they seed the [Failed_assumption]
    (E3) terminal RUP. Empty for an assumption-free solve (E1/E2/E4). *)
type events =
  { inputs : Recorder.input_event list
  ; atoms : Recorder.atom_event list
  ; units : Recorder.unit_event list
  ; learned : Recorder.learned_event list
  ; theory : Recorder.theory_event list
  ; conclusion : Sat.unsat_conclusion option
  ; assumptions : Sat.lit list
  }

(** Snapshot a recorder's accessors into an {!events}. [assumptions] as above. *)
val of_recorder : Recorder.t -> assumptions:Sat.lit list -> events

(** Validate the recorded refutation. See {!verdict} for full versus conditional
    validity. Total (never raises on a malformed stream). *)
val check : events -> verdict

val string_of_verdict : verdict -> string

(** Cumulative count of learned clauses accepted via the full-closure RUP FALLBACK (task
    #56 appendix) rather than their hinted ordered chain. Observability only — NOT a
    soundness signal (soundness = closure entailment, which the fallback establishes). A
    rising count is a degraded/drifting emitter producing chains that no longer replay in
    hint-restricted order. The corpus gate surfaces the run total; the self-test asserts a
    per-case firing. Increments across [check] calls until [reset_fallback_firings]. *)
val fallback_firing_count : unit -> int

val reset_fallback_firings : unit -> unit
