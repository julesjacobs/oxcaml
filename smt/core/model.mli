(** A candidate assignment produced by a theory (or the combinator) — consumed by the
    sat-side self-certifying model evaluator (DESIGN.md §8) and, internally, by
    model-based Nelson–Oppen combination (§6). [Model.t] is abstract; a complete,
    integer-valued, N-O-agreed model is valid only after [check Final] returns [Sat]
    (ADR-0005 CONTRACT-MODEL).

    {b Freeze status (ADR-0005 Tranche B, NOT Tranche A).} [Model.t] is abstract and
    stable, but the {!value} variant's [Uninterp] witness encoding is pinned by the EUF
    adapter (open q3) — so this file is deliberately {e not} hash-frozen at M1; it freezes
    at M2 with the first real model consumer, to avoid a freeze-then-unfreeze.
    {!Oxsmt_core.Theory.THEORY.model} (frozen in Tranche A) names only the abstract
    [Model.t], so freezing [theory.mli] now cannot drift against this file: a shape change
    here fails [theory.mli] to compile, loudly. *)

type value =
  | Int of Bigint.t
  (** an integer term's value. Arbitrary-precision ({!Oxsmt_core.Bigint.t}, ADR-0018
      unfreeze) so a model value exceeding int63 — e.g. a uint256 Certora constant — is
      representable; the term layer is already Bigint (kills the 2^64 coefficient cap),
      and this closes the lagging int63 inconsistency at the model boundary. *)
  | Bool of bool
  | Uninterp of int
  (** an opaque, per-model class id for an uninterpreted-sort term (equal terms share it);
      the exact encoding is pinned at M2 (open q3). *)
  | Real of Term.rational
  (** an exact normalized value for a Real term. *)

type t

(** [value m term] is [term]'s value under [m], or [None] for a term [m] does not
    constrain. Total over asserted terms once [m] is produced after [Final]→[Sat]
    (CONTRACT-MODEL). *)
val value : t -> Term.t -> value option

(** [of_alist bindings] builds a model from term→value bindings — the first-consumer
    construction path shared by the M4 theory adapters (ADR-0005 Tranche B). Additive and
    encoding-agnostic: it does not pin the [Uninterp] witness encoding (open q3, the EUF
    adapter's M2-freeze decision).

    Raises [Invalid_argument] on a {b duplicate term}: a model binds each term exactly
    once, so a repeat is a caller construction bug. This is a deliberate choice over a
    silent last-wins, which would let two conflicting assignments coexist with one masking
    the other (an L1-class fault). *)
val of_alist : (Term.t * value) list -> t
