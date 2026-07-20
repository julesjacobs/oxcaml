(** One-word tagged representation for {!Rational} (Zarith [Z]-style). A {!t} is a SINGLE
    machine word, discriminated at runtime by its tag bit ({!is_immediate}):

    - an IMMEDIATE tagged [int] — the value is an integer with denominator 1 that fits
      int63; the tagged int {b is} the numerator. Zero-allocation.
    - a POINTER to a boxed {!block} — everything else: a small fraction whose numerator
      and denominator both fit int63 ([Frac], native-int arithmetic), or an
      integer/fraction whose components exceed int63 ([Big], arbitrary precision).

    This is the ONLY module in the tree that names [Obj]. It is declared in [oxsmt_lia]'s
    dune [private_modules], so it is mechanically invisible to every consumer outside the
    library — the [Obj] cast can be reached through nothing but the safe, total functions
    below (the [iarr_unsafe]/[atom_unsafe] discipline, ADR-0003). {!Rational} is the sole
    client; it holds the canonical invariant (below) and never itself touches [Obj].

    {b Representation contract (load-bearing).} A value maps to EXACTLY ONE physical form
    (canonical-uniqueness):
    - a fits-int63 integer is ALWAYS the immediate;
    - a fraction (den <> 1) whose numerator and denominator both fit int63 is ALWAYS a
      [Frac];
    - everything else (a >int63 integer, or a fraction with a >int63 component) is a
      [Big]. Each block is reduced (den > 0, gcd = 1). This one-value-one-form property is
      what makes {!Rational}'s value equality decidable and its [to_string] well-defined;
      establishing it is the caller's obligation on {!of_block}.

    {b Do NOT use polymorphic [(=)] / [Stdlib.compare] / [Hashtbl.hash] / [Marshal] on
      {!t}.}
    The representation is MIXED (immediate ints interleaved with pointers), so the
    runtime's structural [compare] orders every immediate BEFORE every block regardless of
    value (e.g. [compare 0 (−2⁶²−1)] would come out negative) — a genuine mis-order, not
    the merely-disciplined hazard {!Bigint} carries. There is no safe polymorphic op on
    this type; {!Rational} exposes only value-based ones. *)

type t

(** The boxed arm. Invariants (caller-established, see {!of_block}):
    - [Frac { n; d }]: [d > 1], [gcd(|n|, d) = 1] (so a fraction, never an integer), and
      both [n] and [d] fit int63. Native-int fraction arithmetic lives on this arm.
    - [Big { num; den }]: [den > 0], [gcd = 1], and the value is NOT representable as an
      immediate or a [Frac] (i.e. some component exceeds int63). *)
type block =
  | Frac of
      { n : int
      ; d : int
      }
  | Big of
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
    the value is not immediate-representable), so canonical-uniqueness holds. *)
val of_block : block -> t

(** Read the boxed {!block}. UNCHECKED: the caller guarantees [not (is_immediate t)]. *)
val to_block : t -> block
