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
