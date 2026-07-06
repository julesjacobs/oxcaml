(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox: branching on a refined bool threads its spec fact.  A decision
   procedure whose bool result IS its spec ([_ = P]) is useless at an
   [if] unless the branches learn [P] / [not P]: the condition is an
   unnamed call, so its refined result would otherwise be dropped
   (unnamed values' facts are unreachable by design).  [if c] is read
   as [let n = c in if n]: the condition is NAMED, its result
   refinement attached at the name, the then-branch gets [n] and
   [n = P], the else-branch [not n] and the same equation. *)

let sat (x : int) : bool{ _ = (x > 0) } = refine_ (x > 0)
[%%expect{|
val sat : (x : int) -> bool{ _ = (x > 0) } = <fun>
|}]

(* then-branch learns [x > 0]. *)
let a (x : int) : int{ _ > 0 } = if sat x then refine_ x else refine_ 1
[%%expect{|
val a : int -> int{ _ > 0 } = <fun>
|}]

(* else-branch learns [not (x > 0)], i.e. [x <= 0]. *)
let b (x : int) : int{ _ <= 0 } = if sat x then refine_ 0 else refine_ x
[%%expect{|
val b : int -> int{ _ <= 0 } = <fun>
|}]

(* Fast path: a translatable condition behaves exactly as before --
   the condition's own translation is the path fact, no named result. *)
let c (x : int) : int{ _ > 0 } = if x > 0 then refine_ x else refine_ 1
[%%expect{|
val c : int -> int{ _ > 0 } = <fun>
|}]

(* while-condition variant: the body runs under the named condition's
   spec. *)
let loop (x : int) : unit =
  while sat x do
    let _pos : int{ _ > 0 } = refine_ x in
    ()
  done
[%%expect{|
val loop : int -> unit = <fun>
|}]

(* SOUNDNESS: the equation is branch-conditional.  In the then-branch
   the named condition is TRUE, so [x <= 0] does not hold and the goal
   is DISPROVED (a program provable only if the then-branch had [not
   n] must still fail). *)
let unsound (x : int) : int{ _ <= 0 } = if sat x then refine_ x else refine_ 0
[%%expect{|
Line 1, characters 62-63:
1 | let unsound (x : int) : int{ _ <= 0 } = if sat x then refine_ x else refine_ 0
                                                                  ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: x <= 0
Hypotheses:
  *unknown9*
  *unknown9* = (x > 0)
Counterexample (validated -- every hypothesis holds and the goal fails here):
  x = 1
  *unknown9* = true
|}]

(* SOUNDNESS: a condition with NO refinement gains nothing new -- an
   untranslatable, unrefined test contributes no hypothesis, so [x > 0]
   is DISPROVED in the then-branch. *)
let opaque (x : int) : bool = x > 0 || x < 0 || true
let no_gain (x : int) : int{ _ > 0 } =
  if opaque x then refine_ x else refine_ 1
[%%expect{|
val opaque : int -> bool = <fun>
Line 3, characters 27-28:
3 |   if opaque x then refine_ x else refine_ 1
                               ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: x > 0
Hypotheses: <none>
Counterexample (validated -- every hypothesis holds and the goal fails here):
  x = 0
|}]
