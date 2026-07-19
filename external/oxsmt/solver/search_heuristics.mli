(** Pure, deterministic building blocks for the modern CDCL search layer: LBD ("glue")
    scoring and LBD-based reduceDB selection (S3), and the CaDiCaL-style rephasing cycle
    (board #155). Stdlib-only (I3) and STATELESS. This is where the hand-checkable
    heuristic logic lives and is unit-tested directly — the frozen {!Sat} interface hides
    the solver internals, so [Sat] threads its state (clause levels, activity, the
    rephase-event counter) through these pure functions. Novelty-free (DESIGN.md §5): LBD
    is Audemard–Simon 2009; the reduceDB order and rephase schedule mirror Glucose /
    CaDiCaL. *)

(** LBD ("glue") of a clause = the number of DISTINCT decision levels among its literals
    (Audemard–Simon 2009). [of_levels levels] takes the decision level of each literal and
    returns the distinct-level count; [0] for the empty array. *)
val lbd_of_levels : int array -> int

(** Clauses with LBD at or below this are "glue": kept permanently by {!reduce_deletions}. *)
val glue_threshold : int

type clause_stat =
  { lbd : int
  ; activity : float
  ; protected_ : bool
    (** locked (current reason) or binary: never deleted, structurally. *)
  }

(** [reduce_deletions stats] returns a bool array parallel to [stats] marking which
    learned clauses to delete in an LBD-based reduceDB. Protected clauses and glue (LBD <=
    {!glue_threshold}) are never marked; among the rest, the worst half of the WHOLE set
    is marked, ordered worst-first by LBD descending, ties by activity ascending.
    Deterministic (stable sort). *)
val reduce_deletions : clause_stat array -> bool array

type rephase_mode =
  | Flipped_true (** decide every var TRUE-first (the flip of the FALSE-first default) *)
  | Best_trail (** the phases of the longest trail prefix seen so far *)
  | Original_default (** the solver's initial default (FALSE-first) *)
  | Saved (** keep the phase-saving array untouched *)

(** The rephasing cycle (CaDiCaL-style), indexed by the rephase-event count. Front-loads
    [Flipped_true] (event 0) so the very first rephase impulse searches for a TRUE-heavy
    model — the firehose lever — then cycles [Best_trail], [Original_default], [Saved]. *)
val rephase_mode : int -> rephase_mode

(** [grow_interval n] — the next rephase interval, grown ~1.5x, so rephasing backs off on
    long instances instead of thrashing. *)
val grow_interval : int -> int
