(** [timeout_ms] must be positive. It configures Z3's timeout and the runner's
    best-effort deadline for serialization, startup and protocol handling. *)
type config =
  { executable : string;
    timeout_ms : int
  }

(** Z3 on PATH, with a five-second process deadline and solver timeout. The
    executable is invoked directly with [-in -smt2], never via a shell. *)
val default_config : config

(** [resources] is the Z3 resource count ([rlimit] units) used by the query,
    when the solver reports it. Unlike the timings, it does not depend on
    machine load. [encoding_seconds] covers serialization; [solving_seconds]
    covers the rest of the check. *)
type result =
  { validity : Vox_smt.validity;
    stderr : string;
    resources : int option;
    encoding_seconds : float;
    solving_seconds : float
  }

exception Cancelled

(** Each call starts and reaps its own solver. Sort errors and unsupported
    target widths are raised before starting it. [dump] receives the exact bytes
    written, including query scopes and follow-up model/reason requests.

    Cancellation is polled throughout I/O and waiting. A true [cancelled] raises
    [Cancelled]; any exception (including [Sys.Break] from the caller's signal
    handler) kills and reaps the child before propagating. Callbacks must not
    block. Calls must be serialized: SIGPIPE is temporarily ignored and restored
    while writing to the solver. Solver stderr is retained for every returned
    outcome, including timeout and protocol failure. [resource_limit] bounds the
    query's Z3 resources; exhausting it yields [Unknown]. *)
val check :
  ?config:config ->
  ?dump:(string -> unit) ->
  ?cancelled:(unit -> bool) ->
  ?resource_limit:int ->
  int_width:int ->
  Vox_smt.query ->
  result

(** Reuse a solver process for the checks performed by the callback. Each query
    starts from a reset solver, so its result and resource count do not depend
    on earlier queries, and has its own deadline. Timeout, cancellation, and
    protocol errors discard the process; a later check starts a new one. The
    process is always reaped when the callback returns or raises. The check
    function is valid only within the callback, and calls must be serialized.
    Dumps include the actual reset and echo commands used by the session. *)
val with_session :
  ?config:config ->
  ?dump:(string -> unit) ->
  ?cancelled:(unit -> bool) ->
  int_width:int ->
  ((?resource_limit:int -> Vox_smt.query -> result) -> 'a) ->
  'a

(** Monotonic clock used for verification budgets. *)
val monotonic_time : unit -> float
