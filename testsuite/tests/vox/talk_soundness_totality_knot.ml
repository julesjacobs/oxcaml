(* TEST
 has-z3;
 flags = "-extension refinement_types";
 { expect; }
*)

(* Talk, section 6 ("how we hunt soundness bugs"), row "hidden types in the
   totality check": "verified to be 1: 2" through a GADT existential or an
   abstract type.

   EXPECTS THE FIX. This test fails on trunk until the hidden-types
   totality fix (branch jujacobs/vox/totality-existentials-20260927) is
   merged: on trunk both knots below are accepted. Its expected output is
   the output with that fix merged.

   Total code may match a type only if the type cannot reach itself through
   its representation; otherwise a total function can tie Landin's knot and
   prove [false]. The check used to walk only the declared representation,
   so a type it could not see into hid the cycle. Verdicts only; each knot
   has an accepted control. From the investigation's knot.ml and
   knot_false.ml. *)

(* The knot through a GADT existential (knot.ml): [any] packs a value of an
   existential type with a witness that recovers it as [r]. *)
type r = { f : any -> {u : unit | false} @@ many forkable unyielding total immutable }
and any = Any : 'a ty * 'a -> any
and _ ty = R : r ty

let (d @ total) (g : r) : {u : unit | false} = g.f (Any (R, g))

let (knot @ total) () : {u : unit | false} =
  let g = { f = (fun (Any (w, x)) -> match w with R -> d x) } in
  d g

let one () : {x : int | x = 1} = ghost_ (knot ()); 2;;
[%%expect{|
type r = {
  f : any -> {u : unit | false} @@ forkable unyielding many total immutable;
}
and any = Any : 'a ty * 'a -> any
and _ ty = R : r ty
val d : r -> {u : unit | false} = <fun>
Line 8, characters 21-33:
8 |   let g = { f = (fun (Any (w, x)) -> match w with R -> d x) } in
                         ^^^^^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 7-9, characters 19-5
         which is expected to be "total".
|}]

(* Control: an existential whose kind rules out functions cannot hide the
   cycle, and matching it stays total. *)
type packed = Pack : ('a : immediate). 'a * ('a, int) eq -> packed
and (_, _) eq = Refl : ('a, 'a) eq

let (unpack @ total) (p : packed) : int =
  match p with Pack (v, Refl) -> v + 1;;
[%%expect{|
type packed = Pack : ('a : immediate). 'a * ('a, int) eq -> packed
and (_, _) eq = Refl : ('a, 'a) eq
val unpack : packed -> int = <fun>
|}]

(* The knot through an abstract type (knot_false.ml): the signature hides
   [type u = t], so [t] looks nonrecursive outside [M]. *)
module M : sig
  type u
  type t = Roll of (u -> {v : unit | false})
  val to_u : t -> u @@ total
  val of_u : u -> t @@ total
end = struct
  type u = t
  and t = Roll of (u -> {v : unit | false})
  let to_u x = x
  let of_u x = x
end

let (delta @ total) (x : M.t) : {v : unit | false} =
  match x with M.Roll f -> f (M.to_u x)
let (omega @ total) () : {v : unit | false} =
  delta (M.Roll (fun y -> delta (M.of_u y)))

(* [ghost_] erases the call, so the looping proof never runs. *)
let (lie @ total) (x : int) : {r : int | r = x + 1} =
  ghost_ (omega ());
  x;;
[%%expect{|
module M :
  sig
    type u
    type t = Roll of (u -> {v : unit | false})
    val to_u : t -> u @@ total
    val of_u : u -> t @@ total
  end
Line 14, characters 15-23:
14 |   match x with M.Roll f -> f (M.to_u x)
                    ^^^^^^^^
Error: The expression is "partial"
       but is expected to be "total"
         because it is used inside the function at lines 13-14, characters 20-39
         which is expected to be "total".
|}]

(* Control: an abstract type whose kind rules out functions. *)
module N : sig
  type u : immediate
  type t = Roll of (u -> int)
end = struct
  type u = int
  type t = Roll of (u -> int)
end

let (use @ total) (x : N.t) : int = match x with N.Roll _ -> 0;;
[%%expect{|
module N : sig type u : immediate type t = Roll of (u -> int) end
val use : N.t -> int = <fun>
|}]
