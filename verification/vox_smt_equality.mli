module Term = Vox_smt_term

type t

type result =
  | Conflict of int array
  | Consistent of (int * int array) list

type lazy_result =
  | Lazy_conflict of int array
  | Lazy_consistent of (int * (unit -> int array)) list

(** [congruence] defaults to accepting every symbol. Its result must remain
    fixed for the lifetime of the solver. A rejected symbol's applications
    remain registered, but do not participate in congruence signatures or
    argument use lists. The caller must enforce their interpreted semantics.
    [commutative] defaults to false. Accepted symbols ignore argument order;
    the caller must ensure this axiom is valid and remains fixed. *)
val create : terms:Term.t -> ?poll:(unit -> unit) ->
  ?congruence:(Term.symbol -> bool) ->
  ?commutative:(Term.symbol -> bool) -> unit -> t

(** Monotonic revision of the registered terms and equivalence classes. *)
val revision : t -> int

(** Representatives for all existing terms, with unregistered terms left as
    singleton classes. This does not register terms or change the solver. *)
val snapshot : t -> int array

val discard_changed : t -> unit
val is_pure_uf : t -> bool

(** Classes affected by registration, merging or rollback since the last
    call. A newly registered application accepted by [congruence] also marks
    its argument classes. *)
val take_changed : t -> Term.term list
val is_registered : t -> Term.term -> bool
val members : t -> Term.term -> Term.term list

(** Constructor applications in [members] order, maintained across merges,
    rollback and permanent registrations. *)
val constructor_members : t -> Term.term -> Term.term list

(** Registered applications accepted by [congruence] that use this term as
    an argument. Applications rejected by [congruence] are omitted. *)
val uses : t -> Term.term -> Term.term list

(** Registrations are permanent. Terms of every sort can be equated.
    Congruence is applied only to symbols accepted by [congruence].
    A literal can name several equivalent Boolean terms or equalities. A new
    binding to an already asserted literal takes effect at the next [check]. *)
val register : t -> Term.term -> unit

val register_equality : t -> literal:int -> Term.term -> Term.term -> unit

(** Assigning [literal] requires the arguments to be pairwise distinct.
    Assigning its negation imposes no constraint. Arguments must have the same
    uninterpreted sort. Registration is permanent; activation follows the
    literal's assignment and is undone by [pop] or [reset]. *)
val register_distinct : t -> literal:int -> Term.term array -> unit

val register_boolean : t -> literal:int -> Term.term -> unit

(** Assert a true signed SAT literal. The solver reports conflicts and
    propagations in [check]; explanations contain asserted antecedent literals.
    Process returned propagations before adding another search level. *)
val assert_literal : t -> int -> unit

val push : t -> unit

val pop : t -> unit

val reset : t -> unit

(** Saturate congruence and return newly implied literals. Constants of Bool,
    Int, and BitVec sorts are distinct when their values differ. Disequalities
    are checked when their classes merge. In pure UF, disequalities of
    uninterpreted sorts also propagate to registered equality literals.
    Unknown interpretations
    are never inferred from a candidate arithmetic or bitvector model. *)
val check : t -> result

(** Lazy explanations preserve the proof state at this check. A thunk may be
    called until the search level at which it was returned is popped or reset.
    Invoke thunks before resetting the solver or reusing its search state. *)
val check_lazy : t -> lazy_result

val representative : t -> Term.term -> Term.term

val explain : t -> Term.term -> Term.term -> int array option

(** Capture an explanation for registered terms in the same current class.
    The thunk has the same lifetime as those returned by [check_lazy]. *)
val explain_later : t -> Term.term -> Term.term -> unit -> int array

(** Queue an equality entailed by true SAT antecedents returned by [reason].
    The explanation must be globally valid with explicit scope guards. It may
    be evaluated until the current search level is popped or reset. Both terms
    must have the same sort; registrations are permanent. *)
val equate : t -> Term.term -> Term.term ->
  reason:(unit -> int array) -> unit

(** Whether queued equalities still require a consistency check. *)
val has_merges : t -> bool
