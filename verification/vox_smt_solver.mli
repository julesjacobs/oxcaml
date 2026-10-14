(** [timeout_ms] must be positive. It configures the solver's timeout and the
    runner's best-effort deadline for complete operations.
    [executable = "ocaml"] selects the bundled in-process solver, which accepts
    typed queries directly and uses SMT-LIB only when a dump is requested.
    Other values name an external solver executable. *)
type config =
  { executable : string;
    timeout_ms : int
  }

(** Z3 on PATH, with a five-second process deadline and solver timeout. The
    executable is invoked directly with [-in -smt2], never via a shell. *)
val default_config : config

(** The solver version proofs are checked with, as [z3 -version] reports it
    after ["Z3 version "]. *)
val expected_version : string

(** Whether [-version] names the supported Z3 or bundled Vox SMT version.
    Select the pure OCaml executable with [executable = "vox-smt"]. *)
val is_expected_version : string -> bool

(** What the solver [executable] reports for [-version], trimmed, or [None]
    when it cannot be run, fails, or does not answer within five seconds. The
    browser build has one solver and no processes: it ignores [executable] and
    reports the Z3 it loaded, as ["Z3 version 4.16.0 - WebAssembly"]. *)
val version : executable:string -> string option

(** [resources] is the solver's resource count ([rlimit] units) used by the
    query, when reported. Units differ between solvers and do not measure
    elapsed time. [core] is, for a valid query checked with assumptions, the
    assumptions its proof used (an unsat core, not necessarily minimal).
    [encoding_seconds] covers serialization; [solving_seconds] covers the rest
    of the check. *)
type result =
  { validity : Vox_smt.validity;
    stderr : string;
    resources : int option;
    core : Vox_smt.Symbol.t list option;
    encoding_seconds : float;
    solving_seconds : float
  }

exception Cancelled

(** Each call uses a fresh solver. External solvers are started and reaped.
    Sort errors and unsupported
    target widths are raised before starting it. [dump] receives the exact bytes
    written, including query scopes and follow-up model/reason requests.

    Cancellation is polled throughout I/O and waiting. A true [cancelled] raises
    [Cancelled]; any exception (including [Sys.Break] from the caller's signal
    handler) kills and reaps the child before propagating. Callbacks must not
    block. Calls must be serialized: SIGPIPE is temporarily ignored and restored
    while writing to the solver. Solver stderr is retained for every returned
    outcome, including timeout and protocol failure. [resource_limit] bounds the
    query's solver resources; exhausting it yields [Unknown]. [assumptions] are
    passed to {!Vox_smt.to_smtlib}; a proof then also reports its core.
    [request_model] defaults to [true]. With [false], a satisfiable query returns
    [Invalid None] without retrieving scalar model values. *)
val check :
  ?request_model:bool ->
  ?config:config ->
  ?dump:(string -> unit) ->
  ?cancelled:(unit -> bool) ->
  ?resource_limit:int ->
  ?assumptions:Vox_smt.Symbol.t list ->
  int_width:int ->
  Vox_smt.query ->
  result

(** Reuse a solver process for the checks performed by the callback. The
    in-process backend currently uses a fresh core per check. Each query
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
  ((?request_model:bool ->
   ?resource_limit:int ->
   ?assumptions:Vox_smt.Symbol.t list ->
   Vox_smt.query ->
   result) ->
  'a) ->
  'a

(** Opt-in reuse of compatible declarations and common assertion prefixes,
    retained in blocks of up to 32 assertions.
    Every goal has its own scope; models and cores refer to the current query.
    Conflicting declarations reset the solver. Compatible declarations are
    global to the process until a reset.
    Resource counts are differences of the solver's cumulative count since
    reset; unavailable or decreasing counters yield [None]. Search history may
    affect performance and resource use. Each check has its own resource limit
    and deadline. Resource exhaustion during setup returns [Unknown] and
    discards the process. The process restarts after 256 checks or 16 MiB of
    input to bound retained history; a single query may exceed those bounds.
    Cancellation and errors have the same recovery contract as [with_session].
    The browser implementation currently resets between checks. *)
val with_incremental_session :
  ?config:config ->
  ?dump:(string -> unit) ->
  ?cancelled:(unit -> bool) ->
  int_width:int ->
  ((?request_model:bool ->
   ?resource_limit:int ->
   ?assumptions:Vox_smt.Symbol.t list ->
   Vox_smt.query ->
   result) ->
  'a) ->
  'a

(** Monotonic clock used for verification budgets. *)
val monotonic_time : unit -> float

(** Reuse identical assertion contexts and goals directly. Reuse an assertion
    prefix after three compatible queries share at least 16 KiB covering three
    quarters of their assertion text. Other queries reset the solver.
    Resource limits, deadlines, recovery and recycling follow
    [with_incremental_session]. *)
val with_automatic_session :
  ?config:config ->
  ?dump:(string -> unit) ->
  ?cancelled:(unit -> bool) ->
  int_width:int ->
  ((?request_model:bool ->
   ?resource_limit:int ->
    ?assumptions:Vox_smt.Symbol.t list -> Vox_smt.query -> result) -> 'a) -> 'a
