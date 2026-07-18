(** Checked linear-integer interpolation from public {!Session} evidence.

    The module treats [Session.last_farkas] as untrusted input: it reconstructs and checks
    the complete Farkas sum before deriving the A-side inequality. Positive Int equality
    premises use the signed equation coefficients documented by {!Session.last_farkas}.
    A result is returned only after its vocabulary and both interpolation obligations have
    been checked on fresh sessions supplied by the caller. *)

open Oxsmt_core

type side =
  | A
  | B

(** [sum coefficients + constant <= 0] with integer coefficients. Candidates derived by
    {!interpolate} have their common integer factor removed. *)
type 'a t =
  { coefficients : ('a * Bigint.t) list
  ; constant : Bigint.t
  }

(** One side reified in a fresh Session. [resolve] maps the caller's stable
    shared-variable descriptors into this session's context. *)
type 'a replay =
  { assertions : Term.t list
  ; resolve : 'a -> Term.t
  }

(** [verify] creates two sessions and calls [build A] and [build B] respectively. It
    accepts only when [A and not I] and [I and B] both return [Session.Unsat], every
    coefficient is nonzero, and [is_shared] accepts every variable. Both solver checks
    run even if the first rejects. The complement [not I] is constructed directly as the
    positive integer half-plane [linear(I) >= 1], avoiding an extra [Not] term. Any other
    verdict or ordinary exception from building or solving rejects the interpolant;
    exceptions from [create] are caller control flow and propagate. *)
val verify
  :  create:(unit -> Session.t)
  -> build:(side -> Session.t -> 'a replay)
  -> is_shared:('a -> bool)
  -> 'a t
  -> bool

(** [interpolate evidence ~side_of ~project_shared ...] reads the most recent Farkas
    conflict from [evidence]. [side_of] must classify every premise and both partitions
    must contribute. [project_shared] maps precisely the shared arithmetic variables to
    the caller's representation; any residual A-local variable rejects the candidate.
    The candidate then passes through {!verify}; unchecked interpolants are never
    returned.

    [side_of], [project_shared], [build], and [is_shared] are the trusted reification
    boundary: they must describe the same A/B partition, [project_shared] must be
    injective on shared variables, and every call to [create] must return a fresh empty
    Session. The module independently checks the public certificate arithmetic and both
    solver obligations, but cannot compare terms from different Contexts. *)
val interpolate
  :  Session.t
  -> side_of:(Term.t * bool -> side option)
  -> project_shared:(Term.t -> 'a option)
  -> create:(unit -> Session.t)
  -> build:(side -> Session.t -> 'a replay)
  -> is_shared:('a -> bool)
  -> 'a t option
