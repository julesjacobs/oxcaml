type fact_origin =
  { kind : string;
    name : string option;
    span : Location.t option;
  }

(** Two origins that name the same kind, name and span.  The fact
    environment folds several introductions of one proposition into a single
    entry, so a consumer asking whether a given site is among an entry's
    producers compares this way rather than by identity. *)
val same_origin : fact_origin -> fact_origin -> bool

type fact =
  { expression : Types.refinement_expression;
    location : Location.t option;
    scope : Location.t option;
    origin : fact_origin;
    (* Every site that introduced this proposition, not just the one whose
       name and span the pane displays.  Two sites can introduce the same
       proposition -- a second call stating a predicate an earlier one already
       stated, or the two arms of a branch each stating it before the merge --
       and the fact environment keeps one entry for it.  Recording only the
       surviving entry's origin would leave the other site looking as though
       it introduced nothing that any obligation reads.  [origin] is always a
       member. *)
    producers : fact_origin list;
  }

type t =
  { location : Location.t;
    facts : fact list;
    goal : Types.refinement_expression;
  }

(* Cross-phase provenance for recursive bindings.  The defeq expander runs
   before typecore computes the structural-termination verdict.  Physical
   location identity connects those phases without a user-spellable
   attribute. *)
module Recursive_binding : sig
  val request_defeq : Location.t -> unit
  val defeq_requested : Location.t -> bool
end

val create :
  loc:Location.t ->
  facts:fact list ->
  goal:Types.refinement_expression ->
  t

val instantiate :
  refinement:Types.refinement_desc ->
  with_:Types.refinement_expression ->
  Types.refinement_expression

type scope_error =
  { location : Location.t;
    escaped : Ident.t list;
  }

module Fact_env : sig
  type nonrec fact = fact
  type vc = t
  type t

  val empty : t
  val enter : Ident.t -> t -> t
  val enter_many : Ident.t list -> t -> t
  val leave : Ident.t -> t -> t
  val leave_many : Ident.t list -> t -> t
  val in_scope : Ident.t -> t -> bool

  (* [typing_env] lets [add] recognise a hypothesis that says a term equals
     itself, which holds at every instantiation and so constrains nothing.
     Without it no such hypothesis is dropped. *)
  (* [producers] defaults to [[origin]].  Pass it when re-inserting a fact
     that already carries a producer set, so the set survives the round trip.
     When the proposition is already present, the incoming producers are
     merged into the existing entry rather than discarded. *)
  val add :
    origin:fact_origin ->
    ?producers:fact_origin list ->
    ?loc:Location.t ->
    ?scope:Location.t ->
    ?typing_env:Env.t ->
    Types.refinement_expression ->
    t ->
    t

  val facts : t -> fact list
  val scope : t -> Ident.Set.t

  (** Whether some fact here records [origin] among its producers.  False
      after an [add] whose expression was out of scope or filtered away, and
      true after one that was merged into a proposition already present. *)
  val introduced_by : fact_origin -> t -> bool

  (** Keep only facts whose bound identifiers are contained in [scope], and
      make [scope] the resulting lexical scope. *)
  val restrict : Ident.Set.t -> t -> t

  val intersect : t -> t -> t
  (** Keep the facts, and only the scope, shared by both environments. *)

  val union : t -> t -> t
  (** Combine facts and scope from two environments.  Unlike [add] and
      [intersect], this does not merge the producer sets of two entries
      holding the same proposition: it concatenates them, so one proposition
      can survive as two entries with disjoint producer sets, and a consumer
      reading only one of them sees a site that did introduce the proposition
      as having introduced nothing.  Nothing in [typing/] calls it; wiring a
      caller in means deduplicating here first. *)

  val same_facts : t -> t -> bool
  (** Whether nothing was added to, merged into or dropped from these facts
      between the one environment and the other.  For a caller asking "did
      evaluating this expression contribute anything to what is known?", to
      which the safe answer wherever it is unclear is yes: two entries count
      as the same only when they hold the same proposition and name the same
      introducers, so a site whose proposition was already present -- which
      changes nothing but that entry's producer list -- is a contribution. *)

  val snapshot :
    loc:Location.t ->
    goal:Types.refinement_expression ->
    t ->
    (vc, scope_error) result
end
