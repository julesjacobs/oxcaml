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
  t = total@1
  t >= 0
  1 <= i
  i <= n
  total@1 >= 0
  total >= 0
  total = 0
Line 8, characters 10-11: vox VC:
  goal: r >= 0
  hypotheses:
  r = total@1
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
let dep (x : int) : {v:int | v = x} = refine_ x

let bridge () : {r:int | r = 7} =
  let mutable m = 3 in
  m <- 7;
  let x = m in
  let refine_ y = dep x in
  refine_ y
[%%expect{|
Line 1, characters 46-47: vox VC:
  goal: x = x
  hypotheses: <none>
val dep : (x : int) -> int{ _ = x } = <fun>
Line 8, characters 10-11: vox VC:
  goal: y = 7
  hypotheses:
  *unknown6* = x
  y = *unknown6*
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
  w = true
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
  m@1 = (K 9)
val get : unit -> int{ _ = 9 } = <fun>
|}]

(* Loop invariants: a [@vox.invariant] formula over program variables is
   a fact in the LOGICAL ENVIRONMENT, not a refinement type.  The
   classical quadruple: ASSERTED over the entry versions, ASSUMED over
   the head versions, ASSERTED over the body-exit versions at the
   back-edge; after the loop the head assumption stands with the negated
   guard.  Mutable variables stay unrefined. *)
let count (n : int) : {r:int | r >= 0} =
  let mutable x = 0 in
  let mutable y = n in
  (while y > 0 do
     x <- x + 1;
     y <- y - 1
   done) [@vox.invariant x >= 0 && x + y = n];
  refine_ x
[%%expect{|
Line 7, characters 9-45: vox VC:
  goal: (x >= 0) && ((x + y) = n)
  hypotheses:
  y = n
  x = 0
Line 7, characters 9-45: vox VC:
  goal: (x@2 >= 0) && ((x@2 + y@2) = n)
  hypotheses:
  y@1 > 0
  (x@1 >= 0) && ((x@1 + y@1) = n)
  y@2 = (y@1 - 1)
  x@2 = (x@1 + 1)
Line 8, characters 10-11: vox VC:
  goal: x@1 >= 0
  hypotheses:
  not (y@1 > 0)
  (x@1 >= 0) && ((x@1 + y@1) = n)
val count : int -> int{ _ >= 0 } = <fun>
|}]

(* The invariant may only mention mutables tracked at the loop: a loop
   inside a closure cannot see the enclosing function's mutables. *)
let bad_scope () =
  let mutable m = 0 in
  let f () =
    (while false do () done) [@vox.invariant m >= 0]
  in
  m <- 1;
  f ()
[%%expect{|
Line 4, characters 45-46:
4 |     (while false do () done) [@vox.invariant m >= 0]
                                                 ^
Error: vox: unbound variable in refinement predicate
|}]

(* The FOR-loop invariant elaborates in the body's environment, so it
   may mention the index: the entry assertion instantiates it at the
   first value, the back-edge assertion at the NEXT value (the next
   iteration's head state), and the post-loop assumption splits on
   whether the loop ran. *)
let iota (n : int) : {r:int | (n < 1 && r = 0) || (n >= 1 && r = n)} =
  let mutable x = 0 in
  (for i = 1 to n do
     x <- x + 1
   done) [@vox.invariant x = i - 1];
  refine_ x
[%%expect{|
Line 5, characters 9-35: vox VC:
  goal: x = (1 - 1)
  hypotheses:
  x = 0
Line 5, characters 9-35: vox VC:
  goal: x@2 = ((i + 1) - 1)
  hypotheses:
  1 <= i
  i <= n
  x@1 = (i - 1)
  x@2 = (x@1 + 1)
Line 6, characters 10-11: vox VC:
  goal: ((n < 1) && (x@1 = 0)) || ((n >= 1) && (x@1 = n))
  hypotheses:
  ((1 > n) && (x@1 = (1 - 1))) || ((1 <= n) && (x@1 = ((n + 1) - 1)))
val iota : (n : int) -> int{ ((n < 1) && (_ = 0)) || ((n >= 1) && (_ = n)) } =
  <fun>
|}]

(* [downto] mirrors, stepping the index down. *)
let count_down (n : int)
  : {r:int | (n < 0 && r = 0) || (n >= 0 && r = n + 1)}
  =
  let mutable x = 0 in
  (for i = n downto 0 do
     x <- x + 1
   done) [@vox.invariant x = n - i];
  refine_ x
[%%expect{|
Line 7, characters 9-35: vox VC:
  goal: x = (n - n)
  hypotheses:
  x = 0
Line 7, characters 9-35: vox VC:
  goal: x@2 = (n - (i - 1))
  hypotheses:
  0 <= i
  i <= n
  x@1 = (n - i)
  x@2 = (x@1 + 1)
Line 8, characters 10-11: vox VC:
  goal: ((n < 0) && (x@1 = 0)) || ((n >= 0) && (x@1 = (n + 1)))
  hypotheses:
  ((n < 0) && (x@1 = (n - n))) || ((n >= 0) && (x@1 = (n - (0 - 1))))
val count_down :
  (n : int) -> int{ ((n < 0) && (_ = 0)) || ((n >= 0) && (_ = (n + 1))) } =
  <fun>
|}]

(* An opaque bound is NAMED (a fresh unknown): one name serves the
   head bounds and the entry/post-loop index instances alike, so an
   index-mentioning invariant works over arbitrary bounds. *)
let opaque_bound (f : unit -> int) : int =
  let mutable x = 0 in
  (for i = 0 to f () do
     x <- x + 1
   done) [@vox.invariant x >= i];
  x
[%%expect{|
Line 5, characters 9-32: vox VC:
  goal: x >= 0
  hypotheses:
  x = 0
Line 5, characters 9-32: vox VC:
  goal: x@2 >= (i + 1)
  hypotheses:
  0 <= i
  i <= *unknown14*
  x@1 >= i
  x@2 = (x@1 + 1)
val opaque_bound : (unit -> int) -> int = <fun>
|}]

(* An arm containing an exception pattern can be reached with the
   scrutinee interrupted between writes: it -- and the continuation of
   a match that has one -- receives the pre-scrutinee state with the
   scrutinee's writes havocked, never the threaded versions.  Here the
   value arm keeps the threaded fact [x@1 = 1]; the exception arm and
   the continuation below see an unconstrained version. *)
let interrupted (p : bool) : {r:int | r = 1} =
  let mutable x = 0 in
  match (if p then raise Not_found); x <- 1 with
  | () -> refine_ x
  | exception Not_found -> refine_ x
[%%expect{|
Line 4, characters 18-19: vox VC:
  goal: x@1 = 1
  hypotheses:
  *unknown16* = ()
  x@1 = 1
Line 5, characters 35-36: vox VC:
  goal: x@2 = 1
  hypotheses: <none>
val interrupted : bool -> int{ _ = 1 } = <fun>
|}]

let interrupted_single (p : bool) : {r:int | r = 1} =
  let mutable x = 0 in
  (match (if p then raise Not_found); x <- 1 with
   | () | exception Not_found -> ());
  refine_ x
[%%expect{|
Line 5, characters 10-11: vox VC:
  goal: x@3 = 1
  hypotheses: <none>
val interrupted_single : bool -> int{ _ = 1 } = <fun>
|}]

(* Children of an unmodeled construct (application arguments here)
   evaluate in unspecified order: each sees the subtree's writes
   havocked, not a sibling's threaded version -- the write below may
   run before the read. *)
let siblings () : {r:int | r = 0} =
  let use (a : {v:int | v = 0}) (_ : unit) : {v:int | v = 0} = a in
  let mutable x = 0 in
  let r = use (refine_ x) (x <- 1) in
  r
[%%expect{|
Line 2, characters 63-64: vox VC:
  goal: a = 0
  hypotheses:
  a = 0
Line 4, characters 23-24: vox VC:
  goal: x@1 = 0
  hypotheses: <none>
Line 5, characters 2-3: vox VC:
  goal: r = 0
  hypotheses:
  r = 0
val siblings : unit -> int{ _ = 0 } = <fun>
|}]
