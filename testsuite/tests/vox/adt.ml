(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: simple algebraic data types.  Constructors of "simple" variants
   (monomorphic, non-GADT, closed, non-empty, tuple arguments) may
   appear in predicates; the solver models them with its datatype
   theory.  Constructors get the usual refinements: [refine_ (K 3)] at
   [t{ _ = K 3 }] names the constructor term itself, and each match
   branch refines a variable scrutinee with [s = C x1 ... xn]. *)

type t =
  | K of int
  | L

let k3 : t{ _ = K 3 } = refine_ (K 3)
[%%expect{|
type t = K of int | L
Line 5, characters 32-37: vox VC:
  goal: (K 3) = (K 3)
  hypotheses: <none>
val k3 : t{ _ = (K 3) } = K 3
|}]

(* The dependent constructor wrapper is PROVED (lambda opening
   substitutes the binder). *)
let mk : (x : int) -> t{ _ = K x } = fun x -> refine_ (K x)
[%%expect{|
Line 1, characters 54-59: vox VC:
  goal: (K x) = (K x)
  hypotheses:
  k3 = (K 3)
val mk : (x : int) -> t{ _ = (K x) } = <fun>
|}]

(* Match facts: injectivity of K proves y = 3. *)
let get (s : t{ _ = K 3 }) : {r:int | r = 3} =
  let refine_ s = s in
  match s with
  | K y -> refine_ y
  | L -> assume_ 0
[%%expect{|
Line 4, characters 19-20: vox VC:
  goal: y = 3
  hypotheses:
  s = (K y)
  s = (K 3)
  s = (K 3)
  k3 = (K 3)
Line 5, characters 17-18: vox VC (ASSUMED):
  goal: 0 = 3
  hypotheses:
  s = L
  s = (K 3)
  s = (K 3)
  k3 = (K 3)
val get : t{ _ = (K 3) } -> int{ _ = 3 } = <fun>
|}]

(* Wildcard sub-patterns name fresh unknowns; recursion is fine; the
   compact grammar accepts constructor applications and tuples. *)
type ilist =
  | Nil
  | Cons of int * ilist

let head (s : ilist{ _ = Cons (3, Nil) }) : {r:int | r = 3} =
  let refine_ s = s in
  match s with
  | Cons (h, _) -> refine_ h
  | Nil -> assume_ 0
[%%expect{|
type ilist = Nil | Cons of int * ilist
Line 8, characters 27-28: vox VC:
  goal: h = 3
  hypotheses:
  s = (Cons (h, *vox-wild*))
  s = (Cons (3, Nil))
  s = (Cons (3, Nil))
  k3 = (K 3)
Line 9, characters 19-20: vox VC (ASSUMED):
  goal: 0 = 3
  hypotheses:
  s = Nil
  s = (Cons (3, Nil))
  s = (Cons (3, Nil))
  k3 = (K 3)
val head : ilist{ _ = (Cons (3, Nil)) } -> int{ _ = 3 } = <fun>
|}]

(* Refined constructor arguments compose with match facts: matching
   [W y] contributes both [w = W y] and the field's refinement. *)
type w =
  | W of {v:int | v > 0}
  | Z

let getw (t : w) : {r:int | r > 0} =
  match t with
  | W y -> let refine_ z = y in refine_ z
  | Z -> assume_ 1
[%%expect{|
type w = W of int{ _ > 0 } | Z
Line 7, characters 40-41: vox VC:
  goal: z > 0
  hypotheses:
  z > 0
  t = (W y)
  y > 0
  k3 = (K 3)
Line 8, characters 17-18: vox VC (ASSUMED):
  goal: 1 > 0
  hypotheses:
  t = Z
  k3 = (K 3)
val getw : w -> int{ _ > 0 } = <fun>
|}]

(* Constructors of non-simple variants (here: parameterized option) may
   not appear in predicates. *)
let bad : t{ _ = K 3 && None = None } = refine_ (K 3)
[%%expect{|
Line 1, characters 31-35:
1 | let bad : t{ _ = K 3 && None = None } = refine_ (K 3)
                                   ^^^^
Error: vox: only constructors of simple variant types (monomorphic, non-GADT, tuple constructor arguments) may appear in refinement predicates
|}]

(* Inline-record constructors (which may have mutable fields) are not
   simple either. *)
type r = R of { n : int }

let bad : r{ _ = R } = refine_ (R { n = 1 })
[%%expect{|
type r = R of { n : int; }
Line 3, characters 17-18:
3 | let bad : r{ _ = R } = refine_ (R { n = 1 })
                     ^
Error: vox: only constructors of simple variant types (monomorphic, non-GADT, tuple constructor arguments) may appear in refinement predicates
|}]

(* Mutually recursive datatypes are not supported: an obligation
   mentioning their constructors is an error. *)
type m =
  | MA of u
  | MB

and u = U of m

let bad : m{ _ = MB } = refine_ MB
[%%expect{|
type m = MA of u | MB
and u = U of m
Line 7, characters 32-34:
7 | let bad : m{ _ = MB } = refine_ MB
                                    ^^
Error: vox: this obligation mentions constructors of a type that is not usable here (not a simple variant, or mutually recursive)
|}]
