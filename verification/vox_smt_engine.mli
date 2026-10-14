module Term = Vox_smt_term
module Z = Vox_smt_integer.Z
type t
type value = Boolean of bool | Integer of Z.t | Bits of int * Z.t | Abstract of int
type result = Sat | Unsat of Term.term list | Unknown of string
val create : Term.t -> t
val register : ?resource:Vox_smt_resource.t -> t -> Term.term -> unit
val assert_ : ?resource:Vox_smt_resource.t -> t -> Term.term -> unit
val push : ?resource:Vox_smt_resource.t -> ?count:int -> t -> unit
val pop : ?resource:Vox_smt_resource.t -> ?count:int -> t -> unit

(** Nonlinear integer arithmetic is incomplete. A check can return [Unknown]
    without exhausting a resource limit; callers must distinguish this from
    both [Sat] and [Unsat].

    Assertions and scopes commit synchronously. Assertions may preprocess
    synchronously; encoding is deferred to the next check. Preprocessing
    failure during synchronous preprocessing leaves the logical assertions
    unchanged and discards the core.
    Encoding, replay, assumptions and search share [budget] and [cancelled].
    An optional shared [resource] also covers synchronous preprocessing and
    model operations and is charged in addition to legacy check limits.
    Interrupted encoding, including encoding requested during search, discards
    its core and replays the logical assertions on the next check. Later checks
    also rebuild cores with substantial allocations made within popped scopes;
    replay shares the budget and preserves the arena. Search interruption
    outside encoding keeps the warm core. Models are invalidated at check entry
    and on assertion/scope changes.
    A cancelled check never commits its assumptions.

    Cancellation takes precedence over a zero budget; either returns without
    encoding. Limits are cooperative. Repeated small budgets may exhaust
    during replay without making progress. Generated terms remain in the
    session's append-only term arena after a discarded core. Reclaiming this
    memory requires replacing both the solver and its term arena. Callbacks
    must not mutate or reenter the solver. Shared resource exhaustion returns
    [Unknown] from [check] and raises [Vox_smt_resource.Exhausted] from other
    operations. Failed synchronous assertions leave logical frames unchanged.
    Interrupted model access may retain completed cache entries; the model
    remains valid and can be queried again with a fresh resource. *)
val check :
  ?resource:Vox_smt_resource.t ->
  ?assumptions:Term.term list -> ?budget:int ->
  ?cancelled:(unit -> bool) -> t -> result
val value : ?resource:Vox_smt_resource.t -> t -> Term.term -> value option
val statistics : t -> Vox_sat_solver.statistics
val resource_count : t -> int

(** Model reconstruction uses the assertion substitutions and the ground
    applications observed by the theories. [raw_value] does not extend the
    model to expressions introduced after the check. *)
val model_term : ?resource:Vox_smt_resource.t -> t -> Term.term -> Term.term
val model_terms : ?resource:Vox_smt_resource.t -> t -> Term.term list
val raw_value : ?resource:Vox_smt_resource.t -> t -> Term.term -> value option

(** The current model generation is absent until a satisfying check.
    Checks and assertion/scope changes discard it. A new registration replaces
    it while retaining the model; repeated registration leaves it unchanged. *)
type model_generation
val model_generation : t -> model_generation option
val model_is_current : t -> model_generation -> bool

(** Internal controls for forcing search phases in recovery tests. *)
module For_testing : sig
  val check :
    ?resource:Vox_smt_resource.t ->
    ?assumptions:Term.term list -> ?budget:int -> ?reorder_after:int ->
    ?comparison_after:int -> ?cancelled:(unit -> bool) -> t -> result
end
