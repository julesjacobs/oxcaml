(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: obligations that must FAIL verification (lean backend).
   has-lean.sh locates the solver (VOX_LEAN, PATH, or a pinned copy)
   and skips the test when there is none. *)

(* A false obligation must fail verification. *)
let bad : {v:int | v > 0} = refine_ 0
[%%expect{|
Line 1, characters 36-37:
1 | let bad : {v:int | v > 0} = refine_ 0
                                        ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: 0 > 0
Hypotheses: <none>
The goal is false unconditionally.
|}]

(* Shadowing soundness: logical facts are keyed by stamp, never by
   name, so the rebound [a] must NOT inherit the outer [a]'s fact and
   this obligation must fail. *)
let shadow_unsound (a : {v:int | v > 0}) : {w:int | w > 0} =
  let a = 0 in
  refine_ a
[%%expect{|
Line 3, characters 10-11:
3 |   refine_ a
              ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: a > 0
Hypotheses:
  a = 0
  a#2 > 0
Counterexample (validated -- every hypothesis holds and the goal fails here):
  a = 0
  a#2 = 1
|}]

(* A phrase that fails verification is backtracked by the toplevel; its
   facts must NOT survive into later phrases.  The contradictory fact
   from [contra] would otherwise prove [oops]'s (false) obligation. *)
let contra : {v:int | v > 0 && not (v > 0)} = refine_ 0
[%%expect{|
Line 1, characters 54-55:
1 | let contra : {v:int | v > 0 && not (v > 0)} = refine_ 0
                                                          ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: 0 > 0 && not (0 > 0)
Hypotheses: <none>
The goal is false unconditionally.
|}]

let oops : {v:int | v = 42} = refine_ 0
[%%expect{|
Line 1, characters 38-39:
1 | let oops : {v:int | v = 42} = refine_ 0
                                          ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: 0 = 42
Hypotheses: <none>
The goal is false unconditionally.
|}]

(* Mutable locals, the exploits: the stale value after a write, and a
   pre-loop equation after havoc, must both FAIL. *)
let stale () : {r:int | r = 3} =
  let mutable m = 3 in
  m <- m + 1;
  refine_ m
[%%expect{|
Line 4, characters 10-11:
4 |   refine_ m
              ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: m@1 = 3
Hypotheses:
  m = 3
  m@1 = m + 1
Counterexample (validated -- every hypothesis holds and the goal fails here):
  m@1 = 4
  m = 3
|}]

let post_loop (n : int) : {r:int | r = 0} =
  let mutable m = 0 in
  let mutable i = 0 in
  while i < n do
    m <- m + 1;
    i <- i + 1
  done;
  refine_ m
[%%expect{|
Line 8, characters 10-11:
8 |   refine_ m
              ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: m@1 = 0
Hypotheses:
  not (i@1 < n)
Counterexample (validated -- every hypothesis holds and the goal fails here):
  m@1 = 1
  n = 1
  i@1 = 1
|}]

(* Loop invariants must be established at entry... *)
let inv_entry (n : int) : int =
  let mutable x = 5 in
  (while x > n do
     x <- x - 1
   done) [@vox.invariant x = 0];
  x
[%%expect{|
Line 5, characters 9-31:
5 |    done) [@vox.invariant x = 0];
             ^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: x = 0
Hypotheses:
  x = 5
Counterexample (validated -- every hypothesis holds and the goal fails here):
  x = 5
|}]

(* ... and preserved by the body. *)
let inv_preserved (n : int) : int =
  let mutable x = 0 in
  (while x < n do
     x <- x + 2
   done) [@vox.invariant x >= 0 && x <= n];
  x
[%%expect{|
Line 5, characters 9-42:
5 |    done) [@vox.invariant x >= 0 && x <= n];
             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: x >= 0 && x <= n
Hypotheses:
  x = 0
Counterexample (validated -- every hypothesis holds and the goal fails here):
  n = -1
  x = 0
|}]

(* The entry assertion instantiates an index-mentioning invariant at
   the FIRST index value... *)
let inv_index_entry (n : int) : int =
  let mutable x = 0 in
  (for i = 1 to n do
     x <- x + 1
   done) [@vox.invariant x = i];
  x
[%%expect{|
Line 5, characters 9-31:
5 |    done) [@vox.invariant x = i];
             ^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: x = 1
Hypotheses:
  x = 0
Counterexample (validated -- every hypothesis holds and the goal fails here):
  x = 0
|}]

(* ... and the back-edge asserts it at the NEXT one: holding at the
   current index is not preservation. *)
let inv_index_step (n : int) : int =
  let mutable x = 0 in
  (for i = 1 to n do
     ()
   done) [@vox.invariant x = i - 1];
  x
[%%expect{|
Line 5, characters 9-35:
5 |    done) [@vox.invariant x = i - 1];
             ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: x = i + 1 - 1
Hypotheses:
  1 <= i
  i <= n
  x = i - 1
  x = 0
Counterexample (validated -- every hypothesis holds and the goal fails here):
  i = 1
  x = 0
  n = 1
|}]

(* An exception arm can be reached with the scrutinee interrupted
   between writes (here, with [p] true, before [x <- 1] ran), so the
   continuation sees the write havocked and this does NOT verify. *)
let interrupted (p : bool) : {r:int | r = 1} =
  let mutable x = 0 in
  (match (if p then raise Not_found); x <- 1 with
   | () | exception Not_found -> ());
  refine_ x
[%%expect{|
Line 5, characters 10-11:
5 |   refine_ x
              ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: x@3 = 1
Hypotheses: <none>
Counterexample (validated -- every hypothesis holds and the goal fails here):
  x@3 = 2
|}]

(* Application arguments evaluate in unspecified order (right-to-left
   in practice, so the write races ahead of the read): siblings see
   each other's writes havocked and this does NOT verify. *)
let siblings () : {r:int | r = 0} =
  let use (a : {v:int | v = 0}) (_ : unit) : {v:int | v = 0} = a in
  let mutable x = 0 in
  let r = use (refine_ x) (x <- 1) in
  r
[%%expect{|
Line 4, characters 23-24:
4 |   let r = use (refine_ x) (x <- 1) in
                           ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: x@1 = 0
Hypotheses: <none>
Counterexample (validated -- every hypothesis holds and the goal fails here):
  x@1 = 1
|}]
