module Term = Vox_smt_term

type t
type result = Done | Continue | Incomplete of string

val create :
  terms:Term.t ->
  boolean:(Term.term -> int) ->
  clause:(int array -> unit) ->
  internalize:(Term.term -> unit) ->
  ?poll:(unit -> unit) ->
  unit -> t

(** Call for observed terms, including testers and selectors, before any
    engine memoization. Calls made by [internalize] during generated witness
    construction are recognized automatically. *)
val register : t -> Term.term -> unit

(** Check inhabitation of a declared datatype without introducing terms. *)
val register_sort : t -> Term.sort -> unit

(** Propagate constructor and selector constraints for changed classes in a
    partial equality arrangement. [constructor_members] lists every constructor
    application in [members] order. [uses] lists direct parent applications.
    [explain left right] captures the current equality proof as a thunk returning
    true antecedent literals. [imply] and [conflict] consume these explanations
    with the same lifetime as the current search level. [equate] queues field
    equalities for Boolean and named sorts; saturate them before accepting
    the equality arrangement. *)
val propagate :
  t -> classes:Term.term list ->
  members:(Term.term -> Term.term list) ->
  constructor_members:(Term.term -> Term.term list) ->
  uses:(Term.term -> Term.term list) ->
  assigned:(int -> bool option) ->
  explain:(Term.term -> Term.term -> unit -> int array) ->
  imply:(int -> reason:(unit -> int array) -> unit) ->
  conflict:(reason:(unit -> int array) -> unit) ->
  equate:(Term.term -> Term.term -> reason:(unit -> int array) -> unit) ->
  unit

(** The representative function describes the current equality arrangement.
    Equalities between named-sort terms must therefore already be reflected
    in that arrangement. Generated clauses are globally valid and can remain
    after backtracking. *)
val check :
  t ->
  representative:(Term.term -> int) ->
  assigned:(int -> bool option) ->
  result

(** Enable constructor choices during propagation for demanded values.
    Otherwise choices wait for a tester or final model completion. *)
val enable_eager : t -> unit

(** Record a literal equivalent to equality of these same-sort terms.
    Datatype definitions must be complete and remain fixed after use.
    Propagation may emit globally valid finite-cardinality clauses.
    The [clause] callback must not reenter this datatype solver. *)
val register_equality : t -> literal:int -> Term.term -> Term.term -> unit
