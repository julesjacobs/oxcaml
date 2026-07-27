type fact_origin =
  { kind : string;
    name : string option;
    span : Location.t option;
  }

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
  (** Combine facts and scope from two environments. *)

  val same_facts : t -> t -> bool
  (** Whether these two environments hold the same facts: not equal ones, the
      same list, unchanged since it was taken.  Identity is deliberate.  It is
      false whenever anything rebuilt the list, a rebuild that changed nothing
      included, so a caller asking "did evaluating this expression contribute
      a fact?" is told yes wherever the answer is not certainly no. *)

  val snapshot :
    loc:Location.t ->
    goal:Types.refinement_expression ->
    t ->
    (vc, scope_error) result
end
