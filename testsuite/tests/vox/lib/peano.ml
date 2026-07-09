type lnat [@@vox.sort lean "Nat"]
type pnat = Z | S of pnat
type t = pnat [@vox.via (toN : lnat)]

(* [toN] is the abstraction function into Lean's built-in [Nat].  No
   theory is defined here -- [Nat]'s arithmetic is native; [toN] only
   says how a unary [pnat] denotes one.  The block references the
   skeleton datatype under its filename-derived solver name. *)
[%%vox.lean {lean|
@[grind] def toN : Vox_Peano_pnat -> Nat
  | .Z => 0
  | .S n => toN n + 1
|lean}]

(* [zero] denotes [toN Z = 0].  gap #31 fix: a top-level via value built
   by an inline constructor now binds at its SKELETON sort, so its self
   fact [zero = Z] is well sorted ([pnat = pnat], not [Nat = pnat]).  It
   may therefore be defined HERE, first; before the fix it had to be
   defined LAST so its ill-sorted fact stayed out of the other
   functions' scope. *)
let zero : t{ _ = 0 } = (Z : t{ _ = 0 })

(* [succ]: unpack the image binder to its skeleton [pn] (link
   [toN pn = n]), rebuild [S pn], whose [toN] is [toN pn + 1 = n + 1]. *)
let succ : (n : t) -> t{ _ = n + 1 } =
  fun n ->
    let refine_ pn = n in
    (S pn : t{ _ = n + 1 })

(* [add] by Peano recursion on the first argument's skeleton.  Both
   arguments are unpacked; the local [go] recurses over the raw [pnat]
   [u] and returns a raw [pnat] whose [toN] is [toN u + toN pb] (a
   predicate over the skeleton sort, mentioning the image via [toN]).
   The recursion IS the induction -- each step is closed by grind's
   linear arithmetic ([toN (S _) = toN _ + 1] stays linear) -- and the
   single via injection at the end carries the whole result to the
   image spec [a + b] through the two unpack links. *)
let add : (a : t) -> (b : t) -> t{ _ = a + b } =
  fun a b ->
    let refine_ pa = a in
    let refine_ pb = b in
    let rec go : (u : pnat) -> pnat{ toN _ = toN u + toN pb } =
      fun u ->
        match u with
        | Z -> pb
        | S n ->
          let r = go n in
          S r
    in
    let w = go pa in
    (w : t{ _ = a + b })
