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
      each cited clause must be unit (or falsified) at its turn; the checker never
      SEARCHES for a propagation. A dropped, permuted, or wrong-set hint chain fails.
    - {b Theory leaves = accepted axioms AT THIS STAGE (§1.5, deferred witness).} A
      [Reason] / [Conflict] theory clause is a leaf shell taken as a valid axiom here —
      its EUF/LIA witness (proof tree / Farkas multipliers) is a later leaf-checking
      tranche. Its {e premises still resolve} (kind-keyed) and it participates soundly in
      BCP/RUP as a T-valid clause. An empty [Conflict] clause (an unconditional
      [T_conflict []], ADR-0013 Rev 6) has NO v1 leaf witness for ⊥-from-∅ and is reported
      [Unsupported], not [Valid].
    - {b Terminal conclusion (§4.0 E1–E4).} [Root_empty] / [Level0_conflict] check the
      cited clause is falsified by the level-0 closure; [Failed_assumption] replays the
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
  | Valid
  | Invalid of string (** an artifact-attributable rejection (ADR-0013 §3.3) *)
  | Unsupported of string
  (** a well-formed leaf/feature this checker version cannot witness (coverage gap) *)

(** The self-contained refutation the checker consumes: exactly the recorded event stream
    (ADR-0013 step 1) plus the assumption literals the traced solve ran under.

    [assumptions] are the literals passed true to {!Sat.solve} (for a session solve, the
    active frame selectors [List.map Sat.pos frames]); they seed the [Failed_assumption]
    (E3) terminal RUP. Empty for an assumption-free solve (E1/E2/E4). *)
type events =
  { inputs : Recorder.input_event list
  ; units : Recorder.unit_event list
  ; learned : Recorder.learned_event list
  ; theory : Recorder.theory_event list
  ; conclusion : Sat.unsat_conclusion option
  ; assumptions : Sat.lit list
  }

(** Snapshot a recorder's accessors into an {!events}. [assumptions] as above. *)
val of_recorder : Recorder.t -> assumptions:Sat.lit list -> events

(** Validate the recorded refutation. [Valid] iff the whole skeleton replays; see the
    module doc for the rejection taxonomy. Total (never raises on a malformed stream). *)
val check : events -> verdict

val string_of_verdict : verdict -> string
