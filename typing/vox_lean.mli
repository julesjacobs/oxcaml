type emission_error =
  { location : Location.t;
    message : string;
  }

type verdict =
  | Proved
  | Not_proved
  | Disproved
  | Solver_error

type result =
  { verdict : verdict;
    location : Location.t;
    detail : string option;
    (* Display-only: fact indices the discharged proof did not reference (from
       Lean's [unusedVariables] linter).  Empty on any non-proved verdict and
       whenever the linter is silent, so a fact defaults to "used". *)
    unused_facts : int list;
  }

val string_of_verdict : verdict -> string

(* Internal logical predicate used by the verifier for the negative fact
   contributed by an earlier constructor arm.  The returned name cannot be
   written as an OCaml value identifier; the Lean emitter recognizes it and
   lowers the application to a constructor-head test. *)
val constructor_mismatch_name : string -> string

(* Whether values of this type have the structural Lean model needed by
   match-arm equalities and projections. *)
val supports_match_facts : env:Env.t -> Types.type_expr -> bool
(* Whether equality at this type can be represented by the Lean backend.
   Selfification uses this to avoid adding facts that would make an otherwise
   unrelated verification condition fail during emission. *)
val supports_equality : env:Env.t -> Types.type_expr -> bool

(* Renders a refinement predicate to source-like syntax (infix operators,
   [true]/[false], [()], constructor and field application) for user-facing
   display: the [int{ ... }] predicate in signatures, type-at-cursor and
   error messages, and the [display] field of the VC dump.  [env] resolves
   which primitive a reference denotes; unresolved references degrade to
   prefix application, never the raw [Types.Refinement.print] AST syntax. *)
val render_predicate :
  ?names:Out_type.Refinement_names.t ->
  env:Env.t ->
  Types.refinement_expression ->
  string

type witness_variable =
  { source_name : string;
    model_name : string;
  }

val witness_variables :
  env:Env.t ->
  Vox_vc.t ->
  (witness_variable list, emission_error) Stdlib.result

val emit :
  env:Env.t -> Vox_vc.t -> (string, emission_error) Stdlib.result

val resolve_lean : ?lean:string -> unit -> string option
val lean_available : ?lean:string -> unit -> bool

val discharge :
  ?lean:string ->
  ?timeout_seconds:int ->
  env:Env.t ->
  Vox_vc.t ->
  result
