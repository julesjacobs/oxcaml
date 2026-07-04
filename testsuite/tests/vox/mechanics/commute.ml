(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: labeled dependent parameters and label commutation.  Deferring
   a dependent parameter by supplying a later labeled argument first is
   legal OCaml partial application: the deferred binder is NOT opened
   -- the partial application's type rebinds it -- and is substituted
   only when the deferred argument finally arrives.  Run with -dump-vc
   so the substituted facts are visible. *)

let f : foo:(x : int) -> bar:(y : int) -> int{ _ = x + y } =
  fun ~foo:x ~bar:y -> assume_unchecked_ (x + y)
[%%expect{|
Line 2, characters 41-48: vox VC (ASSUMED):
  goal: (x + y) = (x + y)
  hypotheses: <none>
val f : foo:(x : int) -> bar:(y : int) -> int{ _ = (x + y) } = <fun>
|}]

(* Both arguments, commuted at one application: both stamps are
   substituted (the goal names m and n, not the binders). *)
let both () =
  let n = 2 in
  let m = 3 in
  let refine_ r = f ~bar:n ~foo:m in
  let refine_ ok = (refine_ r : int{ _ = m + n }) in
  ok
[%%expect{|
Line 5, characters 28-29: vox VC:
  goal: r = (m + n)
  hypotheses:
  *unknown2* = (m + n)
  r = *unknown2*
  r = (m + n)
  m = 3
  n = 2
val both : unit -> int = <fun>
|}]

(* Deferring ~foo: the deferred arrow keeps its binder while ~bar's
   argument is substituted; the binder opens at the later
   application. *)
let deferred () =
  let n = 2 in
  let part = f ~bar:n in
  let m = 3 in
  let refine_ r = part ~foo:m in
  let refine_ ok = (refine_ r : int{ _ = m + n }) in
  ok
[%%expect{|
Line 6, characters 28-29: vox VC:
  goal: r = (m + n)
  hypotheses:
  *unknown5* = (m + n)
  r = *unknown5*
  r = (m + n)
  m = 3
  n = 2
val deferred : unit -> int = <fun>
|}]

(* A later parameter whose CONTRACT mentions the deferred binder: the
   obligation would speak of the unopened binder, which no fact can
   reach -- it must be rejected (fails closed), never discharged. *)
let g : foo:(x : int) -> bar:(y : int{ y > x }) -> int =
  fun ~foo:x ~bar:y -> x + y
[%%expect{|
val g : foo:(x : int) -> bar:int{ _ > x } -> int = <fun>
|}]

let probe () =
  let n = 1 in
  g ~bar:n
[%%expect{|
Line 3, characters 9-10:
3 |   g ~bar:n
             ^
Error: vox: this obligation mentions a variable that has escaped its scope
|}]

(* A compound dependent argument is one honest [let] away: the binding
   selfifies with the full typed fragment, so even shapes the surface
   translation cannot name inline (comparisons, field reads) reach the
   dependent parameter through their let-bound name. *)
let use : (b : bool) -> bool{ _ = b } = fun b -> refine_ b
[%%expect{|
Line 1, characters 57-58: vox VC:
  goal: b = b
  hypotheses: <none>
val use : (b : bool) -> bool{ _ = b } = <fun>
|}]

let cmp_arg () =
  let p = 1 in
  let q = 2 in
  let c = p < q in
  let refine_ r = use c in
  let refine_ ok = (refine_ r : bool{ _ = (p < q) }) in
  ok
[%%expect{|
Line 6, characters 28-29: vox VC:
  goal: r = (p < q)
  hypotheses:
  *unknown9* = c
  r = *unknown9*
  r = c
  c = (p < q)
  q = 2
  p = 1
val cmp_arg : unit -> bool = <fun>
|}]
