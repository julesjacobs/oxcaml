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
  }

val string_of_verdict : verdict -> string

(* Recognizes the OCaml primitives that the Lean backend interprets (integer
   comparisons and arithmetic, boolean connectives).  Every recognized
   primitive is pure and deterministic (side-effect-free); the Q-003
   branch-condition purity gate in [Vox_verify] relies on that invariant, so a
   new entry here must be pure and deterministic too. *)
val primitive_builtin :
  string ->
  [ `Add
  | `And
  | `Equal
  | `Greater
  | `Greater_equal
  | `Less
  | `Less_equal
  | `Multiply
  | `Not
  | `Not_equal
  | `Or
  | `Subtract ]
    option

(* Renders a refinement predicate to source-like syntax (infix operators,
   [true]/[false], [()], constructor and field application) for user-facing
   display: the [int{ ... }] predicate in signatures, type-at-cursor and
   error messages, and the [display] field of the VC dump.  [env] resolves
   which primitive a reference denotes; unresolved references degrade to
   prefix application, never the raw [Types.Refinement.print] AST syntax. *)
val render_predicate : env:Env.t -> Types.refinement_expression -> string

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
