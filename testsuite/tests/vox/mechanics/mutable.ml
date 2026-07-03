(* TEST
 flags = "-dump-vc -vox-dry-run";
 expect;
*)

(* vox: flow-sensitive mutable locals.  Reads name the variable's
   current SSA VERSION; every write mints a fresh one whose
   definitional equation ([m@1 = m + 1]) is a conservative extension
   (usable everywhere), while declared-refinement instances stay
   path-scoped.  [let mutable] cannot be captured or escape, so every
   mutation is a syntactic assignment the walker sees. *)

let f () : {r:int | r = 4} =
  let mutable m = 3 in
  m <- m + 1;
  refine_ m
[%%expect{|
Line 4, characters 10-11: vox VC:
  goal: m@1 = 4
  hypotheses:
  m = 3
  m@1 = (m + 1)
val f : unit -> int{ _ = 4 } = <fun>
|}]

(* Conditional join: the merged version is pinned to whichever branch
   ran, under the reflected condition. *)
let g (b : bool) : {r:int | r >= 1} =
  let mutable m = 1 in
  if b then m <- m + 1;
  refine_ m
[%%expect{|
Line 4, characters 10-11: vox VC:
  goal: m@2 >= 1
  hypotheses:
  (b && (m@2 = m@1)) || ((not b) && (m@2 = m))
  m@1 = (m + 1)
  m = 1
val g : bool -> int{ _ >= 1 } = <fun>
|}]

(* Declared refinements are automatic loop invariants: havoc keeps
   them, and for-loop bounds reflect. *)
let triangle (n : int) : {r:int | r >= 0} =
  let mutable total : {v:int | v >= 0} = refine_ 0 in
  for i = 1 to n do
    let refine_ t = total in
    total <- (refine_ (t + i))
  done;
  let refine_ r = total in
  refine_ r
[%%expect{|
Line 2, characters 49-50: vox VC:
  goal: 0 >= 0
  hypotheses: <none>
Line 5, characters 22-29: vox VC:
  goal: (t + i) >= 0
  hypotheses:
  t >= 0
  1 <= i
  i <= n
  total@1 >= 0
  total >= 0
  total = 0
Line 8, characters 10-11: vox VC:
  goal: r >= 0
  hypotheses:
  r >= 0
  total@1 >= 0
  total >= 0
  total = 0
val triangle : int -> int{ _ >= 0 } = <fun>
|}]

(* Path facts flow through mutable reads (the version reflects). *)
let h () : {r:int | r > 0} =
  let mutable m = 4 in
  m <- m + 1;
  if m > 0 then refine_ m else refine_ 1
[%%expect{|
Line 4, characters 24-25: vox VC:
  goal: m@1 > 0
  hypotheses:
  m@1 > 0
  m = 4
  m@1 = (m + 1)
Line 4, characters 39-40: vox VC:
  goal: 1 > 0
  hypotheses:
  not (m@1 > 0)
  m = 4
  m@1 = (m + 1)
val h : unit -> int{ _ > 0 } = <fun>
|}]

(* [let x = m] pins the current value to an immutable name -- the only
   way to use a mutable's value in dependent positions, since mutable
   stamps stay banned from refinements. *)
let dep : (x : int) -> {v:int | v = x} = fun x -> refine_ x

let bridge () : {r:int | r = 7} =
  let mutable m = 3 in
  m <- 7;
  let x = m in
  let refine_ y = dep x in
  refine_ y
[%%expect{|
Line 1, characters 58-59: vox VC:
  goal: x = x
  hypotheses: <none>
val dep : (x : int) -> int{ _ = x } = <fun>
Line 8, characters 10-11: vox VC:
  goal: y = 7
  hypotheses:
  y = x
  x = m@1
  m@1 = 7
val bridge : unit -> int{ _ = 7 } = <fun>
|}]

(* Reads straddling a write bind DIFFERENT versions: x = y is
   unprovable (and indeed false). *)
let straddle () : {r:int | r = 0} =
  let mutable m = 0 in
  let x = m in
  m <- m + 1;
  let y = m in
  let refine_ w = (assume_unchecked_ true : {v:bool | x = y}) in
  refine_ 0
[%%expect{|
Line 6, characters 37-41: vox VC (ASSUMED):
  goal: x = y
  hypotheses:
  y = m@1
  x = m
  m@1 = (m + 1)
  m = 0
Line 7, characters 10-11: vox VC:
  goal: 0 = 0
  hypotheses:
  x = y
  y = m@1
  x = m
  m@1 = (m + 1)
  m = 0
Line 3, characters 6-7:
3 |   let x = m in
          ^
Warning 26 [unused-var]: unused variable "x".

Line 5, characters 6-7:
5 |   let y = m in
          ^
Warning 26 [unused-var]: unused variable "y".

val straddle : unit -> int{ _ = 0 } = <fun>
|}]

(* A write inside an unmodeled construct (application arguments have
   unspecified evaluation order) havocs: the continuation keeps only
   the declared refinement, here none. *)
let opaque (u : unit -> unit -> unit) : {r:int | r >= 1} =
  let mutable m = 1 in
  (u (m <- 2)) ();
  assume_unchecked_ m
[%%expect{|
Line 4, characters 20-21: vox VC (ASSUMED):
  goal: m@3 >= 1
  hypotheses: <none>
val opaque : (unit -> unit -> unit) -> int{ _ >= 1 } = <fun>
|}]

(* A match on a mutable read pins the version: match facts and
   negations apply to the value the match saw. *)
type t =
  | K of int
  | L

let get () : {r:int | r = 9} =
  let mutable m = L in
  m <- K 9;
  match m with
  | K y -> refine_ y
  | L -> assume_unchecked_ 0
[%%expect{|
type t = K of int | L
Line 9, characters 19-20: vox VC:
  goal: y = 9
  hypotheses:
  m@1 = (K y)
  m@1 = (K 9)
Line 10, characters 27-28: vox VC (ASSUMED):
  goal: 0 = 9
  hypotheses:
  m@1 = L
  not (m@1 is K)
  m@1 = (K 9)
val get : unit -> int{ _ = 9 } = <fun>
|}]
