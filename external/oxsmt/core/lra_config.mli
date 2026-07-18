(** Process-wide rollout gate for real arithmetic.  The environment is read at most
    once, on the first call. *)

val enabled : unit -> bool
