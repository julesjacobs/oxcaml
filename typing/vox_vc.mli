type fact_origin =
  { kind : string;
    name : string option;
    span : Location.t option;
  }

type fact =
  { expression : Types.refinement_expression;
    location : Location.t option;
    origin : fact_origin;
  }

type t =
  { location : Location.t;
    facts : fact list;
    goal : Types.refinement_expression;
  }

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

  val add :
    origin:fact_origin ->
    ?loc:Location.t ->
    Types.refinement_expression ->
    t ->
    t

  val facts : t -> fact list
  val scope : t -> Ident.Set.t

  val intersect : t -> t -> t
  (** Keep the facts, and only the scope, shared by both environments. *)

  val union : t -> t -> t
  (** Combine facts and scope from two environments. *)

  val snapshot :
    loc:Location.t ->
    goal:Types.refinement_expression ->
    t ->
    (vc, scope_error) result
end
