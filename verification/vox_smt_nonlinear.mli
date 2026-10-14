module Term = Vox_smt_term
module Z = Vox_smt_integer.Z

type t
type result = Done | Continue | Incomplete of string

val create :
  terms:Term.t -> boolean:(Term.term -> int) ->
  clause:(int array -> unit) -> ?poll:(unit -> unit) -> unit -> t

(** Register an encoded integer term, integer equality, or integer bound.
    Registration itself does not call back into the engine. *)
val register_modular_term : t -> Term.term -> unit

(** Pure polynomial normalization. Call in arithmetic encoding before the
    nonlinear multiplication fallback; if the returned term differs, encode
    it instead. [None] means the expansion limit was reached. *)
val normalize : t -> Term.term -> Term.term option

(** Call when encoding a nonlinear multiplication as an arithmetic variable.
    Registration itself does not call back into the engine. *)
val register_product : t -> Term.term -> unit
val register_extension : t -> Term.term -> unit

(** Call after allocating a quotient/remainder pair. Registration itself does
    not call back into the engine. Zero divisors remain uninterpreted. *)
val register_division :
  t -> dividend:Term.term -> divisor:Term.term ->
  quotient:Term.term -> remainder:Term.term -> unit

(** Call after installing an integral arithmetic model, before declaring it
    consistent. Replace the old pointwise multiplication refinement loop;
    retain the engine's division-by-zero congruence check. All emitted clauses
    are valid in every scope. [Done] validates the registered products and
    nonzero divisions in the candidate model. *)
val check :
  ?affine_bounds:bool -> t -> integer:(Term.term -> Z.t option) ->
  representative:(Term.term -> Term.term) ->
  explain:(Term.term -> Term.term -> int array option) -> result

(** Emit valid product lemmas before an integer split. Candidate values only
    select guarded lemmas and may be rounded rational values. The result says
    whether any clauses were added. *)
val propagate_products :
  ?affine_bounds:bool -> t -> integer:(Term.term -> Z.t option) ->
  representative:(Term.term -> Term.term) ->
  explain:(Term.term -> Term.term -> int array option) -> bool

(** Attempt the requested integer equalities without a candidate model.
    Every emitted clause includes its equality explanations and interval
    guards. With [affine], also request equalities to constant translations,
    retaining interval guards on the translated term. An unproved request
    has no effect on the logical state. *)
val request_modular_equalities :
  ?affine:bool -> t -> representative:(Term.term -> Term.term) ->
  explain:(Term.term -> Term.term -> int array option) ->
  (Term.term * Term.term) list -> bool

val has_moduli : t -> bool

(** Rebuild before the next check after an extension could not be materialized,
    so terms retained from popped assertions do not keep a session incomplete. *)
val needs_reclamation : t -> bool

val registration_count : t -> int

val enable_ordering : t -> unit
