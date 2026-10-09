(** A deterministic, monotone effort budget for one [check_sat] (board #60).

    Generalizes the {!Cdclt.Split_budget_exceeded} precedent to a composite "effort"
    counter: a single shared mutable cell {!t} is ticked at each discrete progress event
    of the deterministic search, and {!tick} raises {!Exceeded} once the count passes the
    configured cap. The driver ({!Session}) catches {!Exceeded} at the [check_sat]
    boundary and returns [Unknown] with a distinct BUDGET reason — never a verdict from an
    unfinished search (DESIGN.md §6; INVARIANTS.md I6/I8).

    {b Which events tick.} SAT conflicts, SAT decisions, and seam [Final]-rounds — the
    reachable, unbounded-in-principle work of the wired CDCL(T) loop. (LIA simplex pivots
    / B&B nodes are NOT ticked separately: in the wired path LIA sits behind the frozen
    {!Oxsmt_core.Theory}-sealed combinator with no channel to hand it a budget, and its
    per-check work is already bounded — B&B branching is delegated to the SAT core as seam
    [Split]s, counted as [Final]-rounds. See the branch report / design note.)

    {b Determinism (I6).} [effort] is a sum of I6-deterministic counters — it counts
    logical events on a fixed search path, never wall-clock, allocation, GC, or hash order
    — so two runs of the same input report byte-identical [used]. This is the
    load-independence the counted cutoff buys over the wall clock.

    {b Per-check, poison-free.} {!reset} zeroes [used] at the start of each [check_sat];
    an {!Exceeded} does not brick anything (unlike the CONTRACT-POISON firewall), so the
    query is re-runnable at a larger cap. *)

type t

(** Raised by {!tick} when [used] exceeds the cap. Caught only at the {!Session}
    [check_sat] boundary (→ [Unknown] + BUDGET); it is an ordinary control-flow exception,
    not a soundness fault. *)
exception Exceeded

(** [create ?max ()] is a fresh budget with [used = 0]. [max = None] (the default) is
    UNBOUNDED: {!tick} counts but never raises — the instrumentation/calibration mode
    (record per-file effort with no cutoff). [max = Some n] caps [used] at [n]. *)
val create : ?max:int -> unit -> t

(** Increment [used] by one; raise {!Exceeded} if it then exceeds the cap. Called at each
    counted work event. *)
val tick : t -> unit

(** Effort consumed since the last {!reset} (or {!create}). Monotone within a check. *)
val used : t -> int

(** Zero [used] (the cap is unchanged). Called once per [check_sat] so the budget is
    per-check. *)
val reset : t -> unit
