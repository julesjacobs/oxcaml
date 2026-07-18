(** The shared backtracking substrate (ADR-0014 Stage 0).

    Owns, in one place, the scope/frame/pop-ordering/truncation logic that the EUF,
    simplex, and combinator engines each re-implemented by hand — and where this wave's
    scope/pop-ordering bugs lived (the EUF prop-mark watermark trap, the simplex bound
    scopes, the combinator pin frames). Consolidating it here makes the fabric-wide
    [cancel_until 0] invariant (ADR-0014 F3) enforceable once instead of re-proven four
    times.

    A [('e, 'f) t] is a trail of undo entries ['e] under a stack of frames, each frame
    carrying a caller payload ['f]. The substrate never inspects an entry or a payload: it
    owns only the frame stack, the newest-first drain-to-watermark pop loop, and the
    truncation discipline. The caller supplies:

    - the entry type ['e] and an [~apply] that reverses one entry. A hot-path site keeps
      its own int-packed / typed undo sum (EUF's [U_parent], simplex's [Undo_lower], …) as
      ['e], so migrating changes no per-entry allocation (ADR-0014 Stage 0 item 1a — the
      typed-undo realization). A cold site may instead use [unit -> unit] closures as ['e]
      (realization 1b) and [~apply:(fun f -> f ())].
    - a per-frame payload ['f] recording whatever else the frame must restore (EUF's
      auxiliary array watermarks; the combinator's pin-vector watermark), reversed by the
      [~restore] callback. A site whose only backtrackable state is the entry trail uses
      ['f = unit] and the default [~restore].

    Because one frame carries BOTH the entry watermark and the payload, a site's several
    per-frame watermarks are pushed and popped atomically through a single stack — the
    property that removes the multi-watermark desync class of bug, rather than merely
    relocating it. *)

type ('e, 'f) t

val create : unit -> ('e, 'f) t

(** Number of undo entries currently on the trail. This is the {b watermark} primitive
    consumed by {!mark}/{!rewind_to}. *)
val length : ('e, 'f) t -> int

(** Number of open frames (net [push] minus [pop]). *)
val depth : ('e, 'f) t -> int

(** Append an undo entry to the trail, in the current frame. O(1) amortized; allocates
    nothing beyond the backing [Dynarray]'s own growth (the entry itself is built by the
    caller — for a typed site, the same value it allocated before migration). *)
val record : ('e, 'f) t -> 'e -> unit

(** [push t f] opens a new frame at the current trail length, saving payload [f] (the
    state to restore to should this frame later be a [pop] target). *)
val push : ('e, 'f) t -> 'f -> unit

(** [pop t ~apply ?restore n] discards the [n] newest frames, restoring state to the
    checkpoint at which the oldest of them was opened. It drains the trail newest-first
    back to that checkpoint's watermark, calling [apply] on every drained entry in
    reverse-record order, then calls [restore] once with that checkpoint's payload.
    [n = 0] is a no-op. [restore] defaults to [ignore].

    Draining to the target checkpoint in one pass (rather than frame-by-frame) and
    restoring the payload once matches the hand-rolled sites' semantics exactly: an undo
    is applied exactly once, newest-first, and each auxiliary array is truncated to the
    target watermark once.

    @raise Invalid_argument if [n < 0] or [n > depth t]. *)
val pop : ('e, 'f) t -> apply:('e -> unit) -> ?restore:('f -> unit) -> int -> unit

(** {2 Intra-frame transaction primitives (ADR-0014 Rev4-4, §C Stage 0 item 5)}

    A checkpoint {b within} a frame, independent of the frame stack: the fabric's
    per-class merge-undo captures a theory's own trail watermark before a merge-combine
    and replays the theory's trail back to it on unwind; a theory-local transaction (e.g.
    a speculative round) uses the same shape. *)

(** The current trail length, to be paired with a later {!rewind_to}. Equivalent to
    {!length}; named for intent at the call site. *)
val mark : ('e, 'f) t -> int

(** [rewind_to t ~apply m] drains the trail newest-first back to length [m], calling
    [apply] on every drained entry. Does not touch the frame stack.

    @raise Invalid_argument if [m < 0] or [m > length t]. *)
val rewind_to : ('e, 'f) t -> apply:('e -> unit) -> int -> unit

(** {2 Scope-aware undo addressing (ADR-0014 Stage 4)}

    The frame stack already records, for each open frame, the trail length at which it was
    opened — a [depth -> watermark] index. Stage 4's scope-aware-undo driver rewinds a
    migrated trail to the frame of an earliest-removed decision level (an absolute target),
    not by a [pop] COUNT. These expose that index. On a monotone trail (the removed set is a
    contiguous suffix) rewinding to a frame is byte-identical to the equivalent {!pop}; the
    driver's win is realized only once a NON-monotone caller (chronological backtracking)
    replays the survivors above the rewind point (Stage 4.1), which is not part of this
    substrate. *)

(** [watermark_at t i] is the trail length at which the frame at 0-based depth [i] was
    opened (the [i]-th still-open [push]'s checkpoint). @raise Invalid_argument if [i < 0]
    or [i >= depth t]. *)
val watermark_at : ('e, 'f) t -> int -> int

(** [rewind_to_depth t ~apply ?restore d] discards every frame above absolute depth [d]
    (keeping frames [0 .. d-1]), draining the trail newest-first to frame [d]'s checkpoint
    and restoring that checkpoint's payload. It is exactly
    [pop t ~apply ?restore (depth t - d)] — an absolute-depth addressing of {!pop} for the
    Stage-4 driver, which computes a target level rather than a pop count — and is therefore
    byte-identical to that [pop] on every trail. @raise Invalid_argument if [d < 0] or
    [d > depth t]. *)
val rewind_to_depth
  :  ('e, 'f) t
  -> apply:('e -> unit)
  -> ?restore:('f -> unit)
  -> int
  -> unit

(** {2 Test-only introspection}

    Not part of the substrate's operational contract; exposed so the Stage-0 disjointness
    oracle and mutant tests can assert on trail extent without a site reaching into the
    representation. *)
module For_test : sig
  (** All live undo entries, oldest-first. *)
  val entries : ('e, 'f) t -> 'e list
end
