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

(* &&/|| short-circuit: the condition need not translate wholesale.
   In the then-branch of [b && sat x] both operands hold, so the refined
   leaf's spec [x > 0] lands even though [b && sat x] has no translation. *)
let conj (b : bool) (x : int) : int{ _ > 0 } =
  if b && sat x then refine_ x else refine_ 1
[%%expect{|
val conj : bool -> int -> int{ _ > 0 } = <fun>
|}]

(* In the else-branch of [b || sat x] both operands are false, so the
   negated leaf spec [not (x > 0)] i.e. [x <= 0] lands. *)
let disj (b : bool) (x : int) : int{ _ <= 0 } =
  if b || sat x then refine_ 0 else refine_ x
[%%expect{|
val disj : bool -> int -> int{ _ <= 0 } = <fun>
|}]

(* Two refined leaves under &&: both specs land in the then-branch, the
   second guarded by the first (short-circuit). *)
let sat5 (x : int) : bool{ _ = (x > 5) } = refine_ (x > 5)
let both (x : int) : int{ _ > 5 } =
  if sat x && sat5 x then refine_ x else refine_ 6
[%%expect{|
val sat5 : (x : int) -> bool{ _ = (x > 5) } = <fun>
val both : int -> int{ _ > 5 } = <fun>
|}]

(* SOUNDNESS: the &&-then-branch gives [x > 0] (both true), NOT its
   negation -- proving [x <= 0] there is DISPROVED. *)
let n_conj (b : bool) (x : int) : int{ _ <= 0 } =
  if b && sat x then refine_ x else refine_ 0
[%%expect{|
Line 2, characters 29-30:
2 |   if b && sat x then refine_ x else refine_ 0
                                 ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: x <= 0
Hypotheses:
  b && *unknown22*
  b -> *unknown22* = (x > 0)
Counterexample (validated -- every hypothesis holds and the goal fails here):
  x = 1
  *unknown22* = true
  b = true
|}]

(* SOUNDNESS: the ||-then-branch only knows [b || sat x]; [b] alone can
   satisfy it, so [x > 0] is NOT implied and is DISPROVED. *)
let n_disj (b : bool) (x : int) : int{ _ > 0 } =
  if b || sat x then refine_ x else refine_ 1
[%%expect{|
Line 2, characters 29-30:
2 |   if b || sat x then refine_ x else refine_ 1
                                 ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: x > 0
Hypotheses:
  b || *unknown24*
  not b -> *unknown24* = (x > 0)
Counterexample (validated -- every hypothesis holds and the goal fails here):
  x = 0
  *unknown24* = false
  b = true
|}]
