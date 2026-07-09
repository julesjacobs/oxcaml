type lnat [@@vox.sort lean "Nat"]
(* Little-endian binary: [B0 r] is [2 * toN r], [B1 r] is [1 + 2 * toN r].
   Two constructors rather than [Bcons of bool * bits]: a bool payload is
   emitted as a Lean [Prop] field, and matching on a bool LITERAL
   ([Bcons (false, r)]) does not refine the field (it becomes a wildcard),
   so the value of the low bit is invisible to the solver.  Separate [B0]
   /[B1] constructors thread through ordinary datatype matching. *)
type bits = Bnil | B0 of bits | B1 of bits
type t = bits [@vox.via (toN : lnat)]

[%%vox.lean {lean|
@[grind] def toN : Vox_Bignum_bits -> Nat
  | .Bnil => 0
  | .B0 r => 2 * toN r
  | .B1 r => 1 + 2 * toN r
|lean}]

(* [zero] denotes [toN Bnil = 0].  gap #31 fix: a top-level via value
   built by an inline constructor now binds at its SKELETON sort, so its
   self fact [zero = Bnil] is well sorted ([bits = bits], not
   [Nat = bits]) and it may be defined HERE, first, rather than forced
   last to keep an ill-sorted fact out of the other functions' scope. *)
let zero : t{ _ = 0 } = (Bnil : t{ _ = 0 })

(* [succ] by ripple carry on the skeleton: set a low [0] bit, or carry
   past a low [1].  Each step is linear over [Nat] ([2 *] stays linear). *)
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

(* [add] by a full adder with the carry-in encoded as TWO functions --
   [add0] (carry 0) and [add1] (carry 1) -- rather than a bool carry
   parameter (same Prop-field reason as the representation).  The
   mutual recursion IS the induction; every recursive result is
   let-bound so its contract fact threads, and each arm closes by grind's
   linear arithmetic. *)
let rec add0 : (a : bits) -> (b : bits) -> bits{ toN _ = toN a + toN b } =
  fun a b ->
    match a, b with
    | Bnil, _ -> b
    | _, Bnil -> a
    | B0 ar, B0 br -> let r = add0 ar br in B0 r
    | B0 ar, B1 br -> let r = add0 ar br in B1 r
    | B1 ar, B0 br -> let r = add0 ar br in B1 r
    | B1 ar, B1 br -> let r = add1 ar br in B0 r
and add1 : (a : bits) -> (b : bits) -> bits{ toN _ = toN a + toN b + 1 } =
  fun a b ->
    match a, b with
    | Bnil, _ -> bsucc b
    | _, Bnil -> bsucc a
    | B0 ar, B0 br -> let r = add0 ar br in B1 r
    | B0 ar, B1 br -> let r = add1 ar br in B0 r
    | B1 ar, B0 br -> let r = add1 ar br in B0 r
    | B1 ar, B1 br -> let r = add1 ar br in B1 r

let add : (a : t) -> (b : t) -> t{ _ = a + b } =
  fun a b ->
    let refine_ pa = a in
    let refine_ pb = b in
    let w = add0 pa pb in
    (w : t{ _ = a + b })

(* MODEL equality modulo trailing zeros: compare bit by bit, and treat a
   remaining [B0...]-spine against [Bnil] as equal iff it denotes [0].
   Proves [_ = (toN a = toN b)] -- the parity arms ([B0]/[B1] mismatch is
   [false]) close by grind's Nat arithmetic. *)
let rec beq : (a : bits) -> (b : bits) -> bool{ _ = (toN a = toN b) } =
  fun a b ->
    match a, b with
    | Bnil, Bnil -> true
    | Bnil, B0 br -> let z = Bnil in beq z br
    | Bnil, B1 _ -> false
    | B0 ar, Bnil -> let z = Bnil in beq ar z
    | B1 _, Bnil -> false
    | B0 ar, B0 br -> beq ar br
    | B0 _, B1 _ -> false
    | B1 _, B0 _ -> false
    | B1 ar, B1 br -> beq ar br

(* [equal]'s spec [_ = (a = b)] reads [(=)] at the [Nat] image; the unpack
   links ([toN pa = a], [toN pb = b]) rewrite [beq]'s [toN pa = toN pb]
   into it.  This is model equality; polymorphic [(=)] on [t] would be
   representation equality (uninterpreted here -- see lean_bignum.ml). *)
let equal : (a : t) -> (b : t) -> bool{ _ = (a = b) } =
  fun a b ->
    let refine_ pa = a in
    let refine_ pb = b in
    beq pa pb
