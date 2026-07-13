(** One-word tagged representation for {!Rational} (Zarith [Z]-style). A {!t} is a SINGLE
    machine word, discriminated at runtime by its tag bit ({!is_immediate}):

    - an IMMEDIATE tagged [int] — the value is an integer with denominator 1 that fits
      int63; the tagged int {b is} the numerator. Zero-allocation.
    - a POINTER to a boxed {!block} — everything else (a fraction, or an integer/fraction
      whose components exceed int63).

    This is the ONLY module in the tree that names [Obj] (user hard constraint 1). It is
    declared in [oxsmt_lia]'s dune [private_modules], so it is mechanically invisible to
    every consumer outside the library — the [Obj] cast can be reached through nothing but
    the safe, total functions below (the [iarr_unsafe]/[atom_unsafe] discipline,
    ADR-0003). {!Rational} is the sole client; it holds the canonical invariant (below)
    and never itself touches [Obj].

    {b Representation contract (load-bearing).} A {!block} NEVER holds a value that an
    immediate could represent — i.e. never [(den = 1 ∧ num fits int63)]. Equivalently: an
    int63 integer is ALWAYS the immediate form. This canonical-uniqueness (one value ⇒ one
    physical form) is what makes {!Rational}'s value equality decidable and its
    [to_string] well-defined, and it is the caller's obligation on {!of_block}: the caller
    must have already reduced the fraction (den > 0, gcd = 1) AND confirmed it is not
    immediate-representable.

    {b Do NOT use polymorphic [(=)] / [Stdlib.compare] / [Hashtbl.hash] / [Marshal] on
      {!t}.}
    The representation is MIXED (immediate ints interleaved with pointers), so the
    runtime's structural [compare] orders every immediate BEFORE every block regardless of
    value (e.g. [compare 0 (−2⁶²−1)] would come out negative) — a genuine mis-order, not
    the merely-disciplined hazard {!Bigint} carries. There is no safe polymorphic op on
    this type; {!Rational} exposes only value-based ones. *)

type t

(** The boxed arm: a reduced fraction with arbitrary-precision components. Invariant
    (caller-established, see {!of_block}): [den > 0], [gcd(|num|, den) = 1], and NOT
    [(den = one ∧ num fits int63)]. *)
type block =
  { num : Oxsmt_core.Bigint.t
  ; den : Oxsmt_core.Bigint.t
  }

(** [true] iff [t] is the immediate (integer, den = 1, fits int63) form; [false] iff it is
    a {!block} pointer. Total; the runtime tag-bit test ([Obj.is_int]). *)
val is_immediate : t -> bool

(** Wrap a native [int] as the immediate integer [int/1]. Any OCaml [int] fits int63, so
    this always yields a valid immediate. Zero-allocation. *)
val of_int_unchecked : int -> t

(** The numerator of an immediate. UNCHECKED: the caller guarantees {!is_immediate}; the
    result is meaningless (and unsound to use) on a {!block}. Zero-allocation. *)
val to_int_unchecked : t -> int

(** Wrap a {!block}. The caller guarantees the block invariant above (in particular that
    the value is not immediate-representable), so the canonical-uniqueness property holds. *)
val of_block : block -> t

(** Read the boxed {!block}. UNCHECKED: the caller guarantees [not (is_immediate t)]. *)
val to_block : t -> block
