type t

exception Exhausted of string

(** [timeout] is a positive finite duration in the units of [clock]. A timeout
    requires an explicit monotonic clock. At first exhaustion, cancellation
    takes precedence over timeout, then the work budget. Exhaustion is sticky.
    A child reserves work from every ancestor. Checks visit ancestors first.
    A failed reservation consumes the smallest remaining budget from all
    contexts on that path; no context exceeds its budget. Callbacks must not
    reenter any context sharing the same root resource. *)
val create :
  ?parent:t ->
  ?budget:int ->
  ?cancelled:(unit -> bool) ->
  ?clock:(unit -> float) ->
  ?timeout:float ->
  unit -> t

(** Reserve one work unit, checking cancellation and time periodically. *)
val poll : t -> unit

(** Reserve nonnegative work units from this context and its ancestors.
    A failed reservation consumes the smallest remaining budget on the path.
    [used] never exceeds the budget. *)
val charge : t -> int -> unit

(** Check cancellation and time immediately without consuming work. *)
val check : t -> unit
val used : t -> int

(** Remaining local budget; an ancestor may exhaust sooner. *)
val remaining : t -> int
