(* Flow-sensitive mutable locals, really proved through grind: SSA
   versioning with reflected arithmetic, conditional joins, declared
   refinements as loop invariants (with reflected for-bounds), path
   facts through mutable reads, and the [let x = m] bridge.  The result
   of each function is written directly on its annotation -- no explicit
   [refine_] intro is needed (the direct spelling). *)

let f () : {r:int | r = 4} =
  let mutable m = 3 in
  m <- m + 1;
  m

let g (b : bool) : {r:int | r >= 1} =
  let mutable m = 1 in
  if b then m <- m + 1;
  m

let triangle (n : int) : {r:int | r >= 0} =
  let mutable total : {v:int | v >= 0} = 0 in
  for i = 1 to n do
    let t = total in
    total <- t + i
  done;
  total

let h () : {r:int | r > 0} =
  let mutable m = 4 in
  m <- m + 1;
  if m > 0 then m else 1

let dep (x : int) : {v:int | v = x} = x

let bridge () : {r:int | r = 7} =
  let mutable m = 3 in
  m <- 7;
  let x = m in
  dep x

(* Mutable scrutinee: match facts and injectivity through the version. *)
type t =
  | K of int
  | L

let get () : {r:int | r = 9} =
  let mutable m = L in
  m <- K 9;
  match m with
  | K y -> y
  | L -> assume_unchecked_ 0

(* An index-mentioning loop invariant, really proved: it elaborates in
   the body's environment; the entry assertion instantiates the index
   at the first value, the back-edge assertion at the next, and the
   post-loop assumption splits on whether the loop ran. *)
let iota (n : int) : {r:int | (n < 1 && r = 0) || (n >= 1 && r = n)} =
  let mutable x = 0 in
  (for i = 1 to n do
     x <- x + 1
   done) [@vox.invariant x = i - 1];
  x

let count_down (n : int) : {r:int | (n < 0 && r = 0) || (n >= 0 && r = n + 1)} =
  let mutable x = 0 in
  (for i = n downto 0 do
     x <- x + 1
   done) [@vox.invariant x = n - i];
  x
