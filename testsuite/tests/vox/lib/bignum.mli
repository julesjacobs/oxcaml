(* Binary bignums behind the SAME sealed arithmetic interface as
   lib/peano.mli -- identical [lnat] ghost sort naming Lean's built-in
   [Nat], identical [zero]/[succ]/[add] specs.  Two representations under
   one arithmetic spec is the point: a client's [zero]/[succ]/[add]
   reasoning (see lean_bignum_seal.ml, the same body as lean_peano_seal.ml)
   verifies against EITHER machine.  Where peano is unary ([Z]/[S]), this
   is little-endian binary, and [succ]/[add] are ripple carry -- yet the
   client sees only [Nat] arithmetic and never the representation.

   NON-CANONICITY (stated precisely).  Binary is not canonical: [Bnil],
   [B0 Bnil], [B0 (B0 Bnil)] all denote [0].  [via] QUOTIENTS THE LOGIC by
   [toN] -- no client PROOF distinguishes two values with the same
   denotation (they satisfy the same refinements and flow to the same
   conclusions) -- while the PROGRAM can still tell them apart, because
   polymorphic [(=)]/[compare]/[Hashtbl.hash] operate on representations,
   ignoring the abstraction.

   EQUALITY.  Consequently polymorphic [(=)] on a value of [t] is
   REPRESENTATION equality: at the abstract sort it is uninterpreted and
   fails closed (a spec [_ = (a = b)] cannot be met by [a = b] -- see
   lean_bignum.ml).  For MODEL equality use [equal] below, whose inner
   [(=)] is at the [Nat] sort and which is implemented modulo trailing
   zeros and PROVED. *)
type lnat [@@vox.sort lean "Nat"]
type t : value refines (lnat)

val zero : t{ _ = 0 }
val succ : (n : t) -> t{ _ = n + 1 }
val add  : (a : t) -> (b : t) -> t{ _ = a + b }

(* Model equality: [_ = (a = b)] with the inner [(=)] at the [Nat] sort.
   Proved modulo trailing zeros, so it is true of the DENOTATIONS, not the
   representations -- [equal Bnil (B0 Bnil)] is [true]. *)
val equal : (a : t) -> (b : t) -> bool{ _ = (a = b) }
