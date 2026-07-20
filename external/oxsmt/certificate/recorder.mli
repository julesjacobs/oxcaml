(** Certificate emission recorder (ADR-0013 §4.0, M5 step 1).

    A minimal consumer of the frozen {!Oxsmt_solver.Sat.trace} seam: it records every
    emitted certificate event ([on_input] / [on_unit] / [on_learned] / [on_theory_clause]
    / [on_unsat]) into an in-memory log, in emission order, and exposes accessors. This is
    the step-1 foundation the step-2 serializer + OCaml resolution checker consume (the
    events carry exactly the structural data a certificate needs: the active input
    clauses, the level-0 units, the learned-clause resolution chains with ordered-RUP
    antecedents, the materialized theory reason/conflict leaves, and the terminal
    [||]-step conclusion for whichever of the four [Unsat] exits fired).

    Install {!trace} on a PRISTINE solver before the first [add_clause] (the seam
    lifecycle contract) via {!Oxsmt_solver.Sat.set_trace}. The optional theory-leaf
    channel additionally uses immutable core terms and exact LIA rationals; all
    dependencies remain stdlib-only. *)

module Sat = Oxsmt_solver.Sat

type lia_premise =
  { lit : Sat.lit
  ; multiplier : Oxsmt_lia.Rational.t
  }

(** A Farkas witness for a LIA [Conflict] leaf. Each entry binds one true theory-premise
    literal to its nonnegative multiplier; its atom meaning resolves independently
    through the certificate's unique {!atom_event} table. The checker requires the leaf
    clause to be exactly the negation of these literals and independently recomputes the
    weighted half-plane sum. *)
type lia_conflict_witness = { premises : lia_premise list }

(** An EUF proof claim for one materialized theory leaf. [clause] is repeated here so the
    recorder can bind off-frozen-seam evidence to the matching SAT trace event by content,
    rather than by timing: chronological backtracking may ask for an explanation before
    the SAT core materializes its reason clause. The checker requires an exact match and
    re-derives EUF validity from the separate {!atom_event} statement map. *)
type euf_leaf_witness = { clause : Sat.lit list }

(** A datatype constructor-distinctness claim for one materialized [Conflict] leaf.
    [left]/[right] are the two constructor applications claimed equal by the negated
    clause. The datatype declaration that gives those symbols their constructor meaning
    is stored separately in {!theory_event.dt_registry}; the checker re-derives equality
    from the authoritative {!atom_event} statement map. *)
type dt_distinctness_witness =
  { clause : Sat.lit list
  ; left : Oxsmt_core.Term.t
  ; right : Oxsmt_core.Term.t
  }

type rewrite_statement =
  { id : int
  ; context : Oxsmt_core.Context.t
  ; original : Oxsmt_core.Term.t list
  ; reduced : Oxsmt_core.Term.t list
  }

type equality_definition =
  { name : string
  ; sort : Oxsmt_core.Sort.t
  ; value : Oxsmt_core.Term.t
  }

type equality_elimination_witness =
  { statement_id : int
  ; definitions : equality_definition list
  }

type clausify_group =
  { id : int
  ; statement_id : int
  ; source : Oxsmt_core.Term.t
  ; preprocessed : Oxsmt_core.Term.t
  ; selector : Sat.var option
  ; bindings : (Oxsmt_core.Term.t * Sat.var) list option
  ; input_ids : int list
  }

(** The arithmetic meaning assigned to one SAT theory variable. This belongs to the
    certificate statement, not to an individual Farkas proof: the checker rejects
    duplicate/rebound variables and requires every witness premise to resolve here. *)
type atom_event =
  { var : Sat.var
  ; atom : Oxsmt_core.Term.t
  }

type input_event =
  { id : int
  ; clause : Sat.lit array (** the RAW clause, before level-0 filtering *)
  ; origin : Sat.origin
  ; clausify_group : int option
  }

type unit_event =
  { id : int
  ; lit : Sat.lit
  }

type learned_event =
  { id : int
  ; clause : Sat.lit array
  ; antecedents : int list (** ordered-RUP order [rₙ..r₁; conflict] (ADR-0013 §1.4) *)
  ; btlevel : int
  }

type theory_event =
  { id : int
  ; clause : Sat.lit array
  ; role : Sat.theory_clause_role
  ; lia_witness : lia_conflict_witness option
  ; euf_witness : euf_leaf_witness option
  ; dt_registry : Oxsmt_core.Datatype_defs.t option
  ; dt_witness : dt_distinctness_witness option
  }

type t

val create : unit -> t

(** A trace that records into [t]. Attach with {!Sat.set_trace} before any clause. *)
val trace : t -> Sat.trace

(** Record the off-SAT-seam evidence for the next materialized theory [Conflict] clause.
    The LIA adapter produces this immediately before returning [T_conflict]; the frozen
    SAT trace then supplies the clause id. The recorder pairs the two streams in order.
    A claimed witness is never silently discarded: the checker rechecks that its premise
    literals are exactly the emitted clause's negation. *)
val record_lia_conflict
  :  t
  -> premise_lits:Sat.lit list
  -> multipliers:Oxsmt_lia.Rational.t list
  -> unit

(** Record a claim that [clause] follows by pure EUF congruence. Claims are reusable and
    matched to frozen-seam theory events by exact clause content; the checker treats every
    attached claim as a hard proof obligation. *)
val record_euf_leaf : t -> clause:Sat.lit list -> unit

(** Record a conservative claim that [clause] is contradictory because [left] and
    [right] are distinct constructors of one datatype. [registry] is certificate
    statement data, kept outside the witness. Claims bind to frozen-seam events by exact
    clause content, as EUF claims do. *)
val record_dt_distinctness
  :  t
  -> registry:Oxsmt_core.Datatype_defs.t
  -> clause:Sat.lit list
  -> left:Oxsmt_core.Term.t
  -> right:Oxsmt_core.Term.t
  -> unit

(** Record the original/reduced W1b statement and its separate definition witness. The
    returned id is an opaque group key for the session-side clausification callbacks. *)
val record_equality_elimination
  :  t
  -> context:Oxsmt_core.Context.t
  -> original:Oxsmt_core.Term.t list
  -> reduced:Oxsmt_core.Term.t list
  -> definitions:equality_definition list
  -> int

val begin_clausify
  :  t
  -> statement_id:int
  -> source:Oxsmt_core.Term.t
  -> preprocessed:Oxsmt_core.Term.t
  -> unit

val record_clausify_bindings
  :  t
  -> selector:Sat.var
  -> bindings:(Oxsmt_core.Term.t * Sat.var) list
  -> unit

val end_clausify : t -> unit

(** Record a theory atom's authoritative SAT-variable binding at internalization time. *)
val record_theory_atom : t -> var:Sat.var -> atom:Oxsmt_core.Term.t -> unit

(** {2 Accessors — recorded events in chronological (emission) order} *)

val inputs : t -> input_event list
val rewrite_statements : t -> rewrite_statement list
val equality_witnesses : t -> equality_elimination_witness list
val clausify_groups : t -> clausify_group list
val atoms : t -> atom_event list
val units : t -> unit_event list
val learned : t -> learned_event list
val theory_clauses : t -> theory_event list

(** The terminal [||]-step conclusion, present once a traced [solve] returned [Unsat]
    through one of the four exits (E1–E4). *)
val conclusion : t -> Sat.unsat_conclusion option

(** The cited clause ids (the conclusion's + every learned clause's antecedents) that do
    NOT resolve to EXACTLY ONE content-bearing event ([on_input] / [on_learned] /
    [on_theory_clause] — [on_unit] excluded per the sat.mli id-resolvability list). A
    cited id is unresolved if it appears in zero content events (dangling) OR in more than
    one (ambiguous — two distinct clauses under one id, which only arises when one
    recorder is misused across two solvers whose ids both restart from 0; the recorder
    cannot bind to a solver identity, so it rejects the ambiguity fail-closed). The
    id-resolvability invariant holds iff this is []. Sorted, deduplicated. *)
val unresolved_citations : t -> int list
