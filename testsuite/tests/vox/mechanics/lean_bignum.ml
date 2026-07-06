(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Binary bignums modelled as Lean's built-in [Nat], transparent (the
   sealed version is lib/bignum.mli + lean_bignum_seal.ml).  Little-endian
   bits with TWO tail constructors -- [B0 r = 2 * toN r], [B1 r = 1 + 2 *
   toN r] -- rather than [Bcons of bool * bits]: vox emits an OCaml [bool]
   field as a Lean [Prop], and matching on a bool LITERAL does not refine
   it (the bit becomes a wildcard), so [B0]/[B1] as constructors are what
   make the low bit visible to the solver.

   This is the SAME interface and [Nat] model as lib/peano -- one
   arithmetic spec, two machines -- so this file pins only what is
   DISTINCTIVE to binary: ripple-carry proving, NON-CANONICITY, and that
   polymorphic [(=)] is representation equality. *)

type bits = Bnil | B0 of bits | B1 of bits
type lnat [@@vox.sort lean "Nat"]

[%%vox.lean {lean|
@[grind] def toN : Vox_bits -> Nat
  | .Bnil => 0
  | .B0 r => 2 * toN r
  | .B1 r => 1 + 2 * toN r
|lean}]
type t = bits [@vox.via (toN : lnat)]
[%%expect{|
type bits = Bnil | B0 of bits | B1 of bits
type lnat
type t = bits{ true via (toN : lnat) }
|}]

(* [succ] by ripple carry proves honestly (no [assume_unchecked_]): the
   recursion carries past low [1]s, and each step is linear over [Nat]. *)
let rec bsucc : (u : bits) -> bits{ toN _ = toN u + 1 } =
  fun u ->
    match u with
    | Bnil -> B1 Bnil
    | B0 r -> B1 r
    | B1 r ->
      let r' = bsucc r in
      B0 r'
let succ : (n : t) -> t{ _ = n + 1 } =
  fun n ->
    let refine_ pn = n in
    let w = bsucc pn in
    (w : t{ _ = n + 1 })
[%%expect{|
val bsucc : (u : bits) -> bits{ toN _ = toN u + 1 } = <fun>
val succ : (n : t) -> bits{ true && toN _ = n + 1 via (toN : lnat) } = <fun>
|}]

(* NON-CANONICITY, logic side.  [Bnil] and [B0 Bnil] are different
   representations that DENOTE the same [Nat] ([0]): both satisfy
   [t{ _ = 0 }].  [via] quotients the logic by [toN], so no proof can
   distinguish them -- they flow to identical conclusions.  (The PROGRAM
   still can: polymorphic [(=)]/[compare] read the representation.) *)
let zero_plain : unit -> t{ _ = 0 } =
  fun () -> (Bnil : t{ _ = 0 })
let zero_padded : unit -> t{ _ = 0 } =
  fun () -> (B0 Bnil : t{ _ = 0 })
[%%expect{|
val zero_plain : unit -> bits{ true && toN _ = 0 via (toN : lnat) } = <fun>
val zero_padded : unit -> bits{ true && toN _ = 0 via (toN : lnat) } = <fun>
|}]

(* POLYMORPHIC [(=)] IS REPRESENTATION EQUALITY.  At the abstract sort a
   value-level [a = b] is uninterpreted -- it produces a bool with NO
   spec, so a claim that it equals the [Nat]-model equality [(a = b)]
   fails CLOSED (no hypothesis connects them).  This is sound (poly [(=)]
   is never reflected as image equality); for model equality use [equal]
   (proved modulo trailing zeros in lib/bignum.ml). *)
let poly_eq_is_representation : (a : t) -> (b : t) -> bool{ _ = (a = b) } =
  fun a b -> a = b
[%%expect{|
Line 2, characters 13-18:
2 |   fun a b -> a = b
                 ^^^^^
Error: vox: verification failed (lean).
       Goal: *unknown6* = (a = b)
Hypotheses:
  true
(lean: error: `grind` failed)
|}]
