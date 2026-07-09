(* Flow-sensitive mutable locals, really proved through grind: SSA
   versioning with reflected arithmetic, conditional joins, declared
   refinements as loop invariants (with reflected for-bounds), path
   facts through mutable reads, and the [let x = m] bridge.  The result
   of each function is written directly on its annotation (the direct
   spelling). *)

let f () : int{ _ = 4 } =
  let mutable m = 3 in
  m <- m + 1;
  m

let g (b : bool) : int{ _ >= 1 } =
  let mutable m = 1 in
  if b then m <- m + 1;
  m

let triangle (n : int) : int{ _ >= 0 } =
  let mutable total : int{ _ >= 0 } = 0 in
  for i = 1 to n do
    let t = total in
    total <- t + i
  done;
  total

let h () : int{ _ > 0 } =
  let mutable m = 4 in
  m <- m + 1;
  if m > 0 then m else 1

let dep (x : int) : int{ _ = x } = x

let bridge () : int{ _ = 7 } =
  let mutable m = 3 in
  m <- 7;
  let x = m in
  dep x

(* An index-mentioning loop invariant, really proved: it elaborates in
   the body's environment; the entry assertion instantiates the index
   at the first value, the back-edge assertion at the next, and the
   post-loop assumption splits on whether the loop ran. *)
let iota (n : int) : int{ (n < 1 && _ = 0) || (n >= 1 && _ = n) } =
  let mutable x = 0 in
  (for i = 1 to n do
     x <- x + 1
   done) [@vox.invariant x = i - 1];
  x

let count_down (n : int) : int{ (n < 0 && _ = 0) || (n >= 0 && _ = n + 1) } =
  let mutable x = 0 in
  (for i = n downto 0 do
     x <- x + 1
   done) [@vox.invariant x = n - i];
  x
