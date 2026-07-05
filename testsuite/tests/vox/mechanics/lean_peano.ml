(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* Peano numbers modelled as Lean's BUILT-IN [Nat], transparent (the
   sealed version is lib/peano.mli + lean_peano_seal.ml).  The ghost sort
   [lnat] names [Nat] with NO [%%vox.lean] block defining it: [Nat] is
   resolved natively, so the arithmetic vocabulary ([0], [+]) and its
   whole theory (linear arithmetic, decidability) come for free and no
   block travels -- only [toN], the abstraction function.

   This is the third point on the via modeling spectrum -- see
   lean_via.ml (inductive, decidable [card], concrete counterexamples but
   NOT extensional) and lean_xset.ml ([Int -> Prop], extensional but
   witness-free failures).  The built-in [Nat] gives BOTH full arithmetic
   automation AND concrete witnesses (the failing counterexample lives at
   the abstract interface -- see lean_peano_fail.ml). *)

type pnat = Z | S of pnat
type lnat [@@vox.sort lean "Nat"]

(* The ONLY block: the abstraction function into [Nat].  It defines no
   arithmetic -- [Nat] is native -- only how a unary [pnat] denotes one. *)
[%%vox.lean {lean|
@[grind] def toN : Vox_pnat -> Nat
  | .Z => 0
  | .S n => toN n + 1
|lean}]
type t = pnat [@vox.via (toN : lnat)]
[%%expect{|
type pnat = Z | S of pnat
type lnat
type t = pnat{ true via (toN : lnat) }
|}]

(* [succ] proves honestly: unpack to skeleton [pn] (link [toN pn = n]),
   rebuild [S pn], whose [toN] is [toN pn + 1 = n + 1]. *)
let succ : (n : t) -> t{ _ = n + 1 } =
  fun n ->
    let refine_ pn = n in
    (S pn : t{ _ = n + 1 })
[%%expect{|
val succ : (n : t) -> pnat{ true && ((toN _) = (n + 1)) via (toN : lnat) } =
  <fun>
|}]

(* [add] by Peano recursion on the first skeleton.  The recursion IS the
   induction: [go] returns a raw [pnat] whose [toN] is [toN u + toN pb]
   (a predicate over the skeleton sort naming the image via [toN]), and
   each step closes by grind's LINEAR arithmetic ([toN (S _) = toN _ + 1]
   stays linear).  One via injection carries the result to [a + b]. *)
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
[%%expect{|
val add :
  (a : t) -> (b : t) -> pnat{ true && ((toN _) = (a + b)) via (toN : lnat) } =
  <fun>
|}]

(* A TRUE fact re-coerced transparently: [add a b] denotes [a + b], and
   re-stating that at the via type verifies (the spine is visible and the
   result's [toN] matches).  A FALSE overclaim in this same transparent
   position fails CLOSED but at elaboration (an ill-sorted grind term over
   the datatype), not with a counterexample -- the clean concrete Nat
   witness needs the abstract interface, where the result IS a [Nat]; see
   lean_peano_fail.ml. *)
let restate : (a : t) -> (b : t) -> t{ _ = a + b } =
  fun a b -> add a b
[%%expect{|
val restate :
  (a : t) -> (b : t) -> pnat{ true && ((toN _) = (a + b)) via (toN : lnat) } =
  <fun>
|}]
