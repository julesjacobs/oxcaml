(** [timeout_ms] must be positive. It configures Z3's timeout and the runner's
    best-effort deadline for startup and protocol handling. *)
type config =
  { executable : string;
    timeout_ms : int
  }

(** Z3 on PATH, with a five-second process deadline and solver timeout. The
    executable is invoked directly with [-in -smt2], never via a shell. *)
val default_config : config

type result =
  { validity : Vox_smt.validity;
    stderr : string
  }

exception Cancelled

(** Each call starts and reaps its own solver. Sort errors and unsupported
    target widths are raised before starting it. [dump] receives the exact bytes
    written, including follow-up model/reason requests and [exit].

    Cancellation is polled throughout I/O and waiting. A true [cancelled] raises
    [Cancelled]; any exception (including [Sys.Break] from the caller's signal
    handler) kills and reaps the child before propagating. Callbacks must not
    block. Calls must be serialized: SIGPIPE is temporarily ignored and restored
    while writing to the solver. Solver stderr is retained for every returned
    outcome, including timeout and protocol failure. *)
val check :
  ?config:config ->
  ?dump:(string -> unit) ->
  ?cancelled:(unit -> bool) ->
  int_width:int ->
  Vox_smt.query ->
  result
