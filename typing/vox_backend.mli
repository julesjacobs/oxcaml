type backend =
  | Lean
  | Z3
  | Oxsmt

type selection =
  | Single of backend
  | Cross

type verdict =
  | Proved
  | Not_proved
  | Disproved
  | Unknown
  | Solver_error
  | Unavailable

(** Fact usage is conservative.  An SMT unsat core need not be minimal, so a
    core may keep an unnecessary fact visible.  Only facts outside the
    solver-returned core are unused: a fade may be missed, but never
    invented. *)
type fact_usage_capability =
  | Fact_usage
  | No_fact_usage

type capabilities =
  { fact_usage : fact_usage_capability;
  }

type obligation =
  { env : Env.t;
    condition : Vox_vc.t;
    prove_contents : string option;
  }

type backend_result =
  { backend : backend;
    capabilities : capabilities;
    verdict : verdict;
    location : Location.t;
    detail : string option;
    unused_facts : int list option;
  }

type result =
  { verdict : verdict;
    location : Location.t;
    detail : string option;
    unused_facts : int list option;
    backend_results : backend_result list;
  }

module type S = sig
  val backend : backend
  val capabilities : capabilities

  (** Stable, backend-specific material for persistent discharge caching.
      [None] conservatively bypasses the cache. *)
  val cache_key : command:string option -> obligation -> string option

  val discharge :
    command:string option -> obligation -> backend_result
end

(** A fail-closed persistent cache decorator.  Only proved and disproved
    results are stored; malformed or stale entries are treated as misses. *)
module Cached (Backend : S) : S

(** The log that would hold persistent results for [backend] under the
    current source file, compiler and cache directory.  [None] when the
    cache is unavailable. *)
val cache_bucket_path : backend -> string option

val backend_of_string : string -> (backend, string) Stdlib.result
val selection_of_string : string -> (selection, string) Stdlib.result
val string_of_backend : backend -> string
val string_of_selection : selection -> string
val string_of_verdict : verdict -> string
val capabilities : backend -> capabilities

module Lean_backend : S
module Z3_backend : S
module Oxsmt_backend : S

val discharge :
  selection:selection ->
  smt_solver:string option ->
  oxsmt_solver:string option ->
  ?prove_contents:string ->
  env:Env.t ->
  Vox_vc.t ->
  result
