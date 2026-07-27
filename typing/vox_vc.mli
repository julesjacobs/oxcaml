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

(* A [vox.decreases] termination measure, as typecore checked it: the
   parameters of the measured function in source order, one integer component
   per lexicographic position, the identifiers of the whole recursive group,
   and the span of the measure itself.  A parameter position carries several
   identifiers when its pattern aliases, and all of them denote that whole
   argument.  Components are written over the parameters as bound
   occurrences, so a call site obtains the measure at its actual arguments by
   substituting them. *)
module Decreases : sig
  type measure =
    { parameters : Ident.t list list;
      components : Types.refinement_expression list;
      group : Ident.t list;
      loc : Location.t;
    }

  val record : Ident.t -> measure -> unit
  val find : Ident.t -> measure option
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
  val add :
    origin:fact_origin ->
    ?loc:Location.t ->
    ?scope:Location.t ->
    ?typing_env:Env.t ->
    Types.refinement_expression ->
    t ->
    t

  val facts : t -> fact list
  val scope : t -> Ident.Set.t

  (** Keep only facts whose bound identifiers are contained in [scope], and
      make [scope] the resulting lexical scope. *)
  val restrict : Ident.Set.t -> t -> t

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
