(** Incremental propositional SAT, implemented entirely in standard OCaml.
    Literals are signed, nonzero DIMACS variable numbers. Variables must be
    allocated before use. Variable identities are stable; none are eliminated. A
    solver is mutable and must not be used concurrently or from its own
    cancellation callback. Theory extensions supply their own sound inferences.
*)
type t

type context

type check_result =
  | Done
  | Continue
  | Incomplete

type extension =
  { start : unit -> unit;
    push_level : unit -> unit;
    pop_levels : int -> unit;
    assigned : context -> int -> unit;
    propagate : context -> unit;
    final_check : context -> check_result;
    finish : unit -> unit
  }

(** An extension is attached for the solver's lifetime. Its theory and permanent
    declarations must remain valid across solves. [start] resets search state;
    current root assignments are then replayed through [assigned]. Every search
    level, including an already-true assumption, calls [push_level];
    backtracking calls [pop_levels] after SAT assignments have been undone.
    [finish] is called on every exit, including an exception from [start], and
    must reset transient state even after a partially completed callback.
    [push_level], [pop_levels] and [finish] should not raise. User scopes remain
    the caller's responsibility. All inferred consequences must include their
    scope guards as antecedents.

    [assigned] receives each true signed literal in trail order. [propagate] is
    called at the Boolean/theory propagation fixed point; [final_check] runs
    only after every ordinary variable and every variable referenced by a
    stored clause is assigned.
    Variables allocated with [completion:true] may remain unassigned. [Done]
    accepts it, [Continue] requests more work, and [Incomplete] returns
    [Unknown]. Any newly emitted work is processed before accepting [Done].
    Callbacks must return promptly for cancellation. *)

(** Context operations are valid only during [assigned], [propagate], and
    [final_check], using the context passed to that callback. *)
val current_level : context -> int

val assigned_value : context -> int -> bool option

(** Same allocation policy as [new_var]. *)
val new_theory_var : ?completion:bool -> context -> int

(** Read the current assignment, or false for an unassigned positive variable
    allocated with [completion:true] and never referenced by a stored clause.
    This default is a proposed model value, not an assignment or a valid
    explanation antecedent. A theory must validate it and add valid lemmas
    before accepting the model. Referencing the variable in a stored clause
    permanently enables ordinary search for it. *)
val completion_value : context -> int -> bool option

(** [reason ()] returns true antecedent literals entailing the consequence.
    Antecedents must precede the consequence in the assignment trail. The thunk
    is evaluated lazily and must remain valid until that assignment is undone;
    root reasons are evaluated eagerly. Materialized explanations may be
    retained as permanent theory lemmas. It cannot call context operations.
    Invalid explanations raise [Invalid_argument]; logical validity is the
    extension's responsibility. Reassert consequences after backtracking when
    necessary. A consequence already true needs no explanation. *)
val imply : context -> int -> reason:(unit -> int array) -> unit

(** The antecedents must currently be true and jointly inconsistent in the
    theory. The explanation is evaluated immediately, before any backtracking.
*)
val conflict : context -> reason:(unit -> int array) -> unit

(** Add a globally theory-valid clause. Antecedents and scope guards must be
    explicit literals in it. The clause is copied and processed after the
    callback returns. Only an asserting or conflicting clause may backtrack
    search; other lemmas preserve the current trail. Added lemmas survive
    [Unknown] and callback exceptions. *)
val add_lemma : context -> int array -> unit

type result =
  | Sat of bool array
  | Unsat of int array
  | Unknown

(** A SAT model is indexed by variable number; index zero is unused. An UNSAT
    core is a subset of the supplied assumptions, sufficient for UNSAT with the
    current clauses and scopes. It need not be minimal. Returned arrays are
    independent of subsequent calls. *)

type statistics =
  { solves : int;
    decisions : int;
    propagations : int;
    conflicts : int;
    restarts : int;
    learnt_clauses : int;
    deleted_clauses : int
  }

val create : ?variables:int -> ?extension:extension -> unit -> t

(** Attach an extension to a solver created without one. The extension remains
    attached for the solver's lifetime. Existing clauses must remain valid in
    the theory; current root assignments are replayed on the next [solve].
    Attaching a second extension raises [Invalid_argument]. *)
val attach_extension : t -> extension -> unit

(** [completion:true] leaves the variable outside search until a stored clause
    mentions it. An unassigned variable then has value false in the returned
    model. This is suitable for private encoding variables whose theory use
    consists only of validated model completion. The default is false.
    Assumptions and theory implications still assign completion variables. *)
val new_var : ?completion:bool -> t -> int

val variable_count : t -> int

(** Add a clause to the current scope, copying its contents. Empty clauses,
    duplicate literals and tautologies are supported. Invalid literals raise
    [Invalid_argument] without changing the formula. *)
val add_clause : t -> int array -> unit

(** Add a permanent clause even inside a scope, for example a theory lemma. The
    clause survives every [pop]. *)
val add_clause_global : t -> int array -> unit

(** Scopes use private activation variables allocated from the same variable
    namespace as [new_var]. Allocate application variables with [new_var], never
    by incrementing a previously returned number. [variable_count] and models
    include activation variables; applications must not mention them. [pop]
    retracts the most recent scope and everything added in it. Learned clauses
    remain valid and are retained across scopes and solves. Popping disables
    scoped clauses without compacting the clause database. Popping without a
    matching push raises [Invalid_argument]. *)
val push : t -> unit

val pop : t -> unit

(** Conflict budgets are per call; zero immediately returns [Unknown].
    [should_stop] is polled before search and during propagation and search.
    True returns [Unknown]; exceptions propagate after restoring a reusable
    state. Both [Unknown] and exceptions preserve the formula and valid learned
    clauses. Budgets are best effort: preprocessing a newly added clause and a
    single conflict analysis are not interruptible. *)
val solve :
  ?assumptions:int array ->
  ?max_conflicts:int ->
  ?should_stop:(unit -> bool) ->
  t ->
  result

(** Cumulative counters, including work in interrupted searches. *)
val statistics : t -> statistics

val reset_statistics : t -> unit

(** Stored clause slots, including disabled clauses and deleted slots. *)
val clause_count : t -> int

module Internal : sig
  val set_occurrence_activity : t -> bool -> unit
end
