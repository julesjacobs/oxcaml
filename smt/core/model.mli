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
  | Int of int
  | Bool of bool
  | Uninterp of int
  (** an opaque, per-model class id for an uninterpreted-sort term (equal terms share it);
      the exact encoding is pinned at M2 (open q3). *)

type t

(** [value m term] is [term]'s value under [m], or [None] for a term [m] does not
    constrain. Total over asserted terms once [m] is produced after [Final]→[Sat]
    (CONTRACT-MODEL). *)
val value : t -> Term.t -> value option
