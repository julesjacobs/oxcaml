type emission_error =
  { location : Location.t;
    message : string;
  }

type backend =
  [ `Z3
  | `Oxsmt
  ]

type query =
  | Prove
  | Disprove

type input_mode =
  | Stdin
  | File_argument

type solver_status =
  | Sat
  | Unsat
  | Unknown

type verdict =
  | Proved
  | Not_proved
  | Disproved
  | Solver_error
  | Unavailable

type result =
  { verdict : verdict;
    location : Location.t;
    detail : string option;
  }

val string_of_verdict : verdict -> string

val parse_status : string -> solver_status option

val emit :
  query:query ->
  env:Env.t ->
  Vox_vc.t ->
  (string, emission_error) Stdlib.result

val discharge_oxsmt :
  ?timeout_seconds:int -> env:Env.t -> Vox_vc.t -> result

val discharge :
  backend:backend ->
  command:string option ->
  ?input_mode:input_mode ->
  ?timeout_seconds:int ->
  env:Env.t ->
  Vox_vc.t ->
  result
