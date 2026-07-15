(** Exact-integer Hermite Normal Form (HNF) over {!Oxsmt_core.Bigint} — a STANDALONE
    integer-lattice kernel for the Stage B LIA integer cut (charter
    logs/lia-cuts-charter.md, spec logs/lia-cuts-hnf-spec.md). No solver state, no
    {!Rational}, no {!Term}: it consumes an integer matrix and returns its HNF plus the
    unimodular transform.

    {b Why.} z3's rings integer-cut mechanism lives in the integer LATTICE spanned by the
    constraint rows, which the rational simplex tableau (a ℚ basis) cannot expose.
    Reducing the (lcm-normalized, integer) constraint matrix to HNF surfaces that lattice
    — its diagonal is the lattice determinant, and the reduction is what a modular/ring
    cut is derived from. This module is the reusable, provable-in-isolation kernel; the
    cut selection and emission are Stage B integration (gated separately).

    {b Arithmetic.} All entries are {!Oxsmt_core.Bigint}, so no operation overflows or
    raises — HNF entries can grow large (coefficient blow-up) but never wrap. The
    degrade-to-[None]-on-overflow discipline of the charter lives at the Rational→Bigint
    INGESTION boundary and the {!max_rows}/{!max_cols} caps the integration layer
    enforces; this kernel is total over Bigint. *)

(** A dense integer matrix, [m] rows × [n] columns, row-major. Transparent so callers and
    tests build matrices directly; compare with {!matrix_equal} (never polymorphic [=],
    per the {!Oxsmt_core.Bigint} discipline). *)
type matrix = Oxsmt_core.Bigint.t array array

type t =
  { h : matrix (** the HNF of the input: [U * A]. *)
  ; u : matrix (** the [m × m] unimodular transform, so [u * A = h]. *)
  ; det_sign : int
  (** [det(u)] ∈ [{-1, +1}], tracked through the elementary row ops (each swap/negate
      flips it, each add-multiple leaves it unchanged) — [u] is unimodular by
      construction. *)
  }

(** [compute a] row-reduces [a] to Hermite Normal Form by elementary unimodular row ops.
    The returned [h] is in HNF (row echelon: each nonzero row's leading column strictly
    right of the row above, zero rows last; positive pivots; every entry above a pivot
    reduced into [0, pivot)), and [u * a = h] with [u] unimodular. Total (Bigint; never
    raises). Deterministic. *)
val compute : matrix -> t

(** [mul x y] is the integer matrix product (cols of [x] = rows of [y]). *)
val mul : matrix -> matrix -> matrix

(** Value equality (same shape, {!Oxsmt_core.Bigint.equal} entrywise). *)
val matrix_equal : matrix -> matrix -> bool

(** [is_hnf h] iff [h] satisfies the structural HNF invariants (echelon order, positive
    pivots, above-pivot entries reduced into [0, pivot)). *)
val is_hnf : matrix -> bool

(** [verify t a] re-checks the result against its defining properties: [det_sign] is ±1,
    [u] is square [m × m], [u * a = h] exactly, and [h] is a valid HNF. The always-on
    self-check (the mutation-testing tripwire: a corrupted transform or a non-HNF result
    is rejected). *)
val verify : t -> matrix -> bool

(** z3-parity caps (util/lp/lp_settings.h): rows [limit_on_rows_for_hnf_cutter = 75],
    columns [limit_on_columns_for_hnf_cutter = 150], and the [m_hnf_cut_period = 4] cut
    throttle. Named for the integration layer to enforce; the kernel imposes no limit. *)
val max_rows : int

val max_cols : int
val cut_period : int
