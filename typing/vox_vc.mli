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

(* The obligations a unit admitted rather than proved, each recognised by the
   physical identity of the location object typecore minted for its [assume].
   The refinement travels with the site rather than being read back off the
   expression, because a use in statement position weakens the refined type
   away before the verifier sees it. *)
module Assumption : sig
  type t =
    { key : Location.t;
      (** The location object the verification mark carries, which is what
          identifies the site.  Never compared by span. *)
      site : Location.t;
      (** Where the [assume] is written, for reporting. *)
      guarded : bool;
      (** Whether a run-time check of the predicate was emitted, which is
          what makes an admitted statement falsifiable by running the
          program. *)
      refinement : Types.refinement_desc;
      refined_type : Types.type_expr;
    }

  val record :
    key:Location.t ->
    site:Location.t ->
    guarded:bool ->
    Types.refinement_desc ->
    Types.type_expr ->
    unit

  val refinement : Location.t -> Types.refinement_desc option

  (** A check the compiler generated for an admission, recognised by the
      physical identity of the location object its node carries.  The
      obligations its calls raise are not the program's, since whether a
      predicate runs must not decide whether the program compiles. *)
  val record_check : Location.t -> unit

  val is_check : Location.t -> bool

  (** Every obligation admitted in the unit, in source order. *)
  val admitted : unit -> t list

  (** Forget them, once reported. *)
  val forget : unit -> unit
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
