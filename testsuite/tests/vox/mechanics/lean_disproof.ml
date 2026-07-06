(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox distinguishes a DISPROVED obligation (a counterexample was
   VALIDATED by evaluation: every hypothesis holds and the goal fails
   on a concrete assignment) from a NOT-PROVED one (grind's automation
   gave up, but no counterexample was found, so the property may still
   hold).  A nonsense grind atom value -- famously [x*x = -1] for the
   nonlinear [x*x >= 0] -- is never shown, because only BINDERS are
   assigned and every shown witness is re-checked by the solver. *)

(* A ground false goal: disproved with no assignment needed. *)
let ground : {v:int | v = 42} = refine_ 0
[%%expect{|
Line 1, characters 40-41:
1 | let ground : {v:int | v = 42} = refine_ 0
                                            ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: 0 = 42
Hypotheses: <none>
The goal is false unconditionally.
|}]

(* A false goal under a hypothesis: disproved, and the witness respects
   the hypothesis ([x >= 0] holds at [x = 0], yet [x - 1 >= 0] fails). *)
let under_hyp (x : {v:int | v >= 0}) : {w:int | w >= 0} = refine_ (x - 1)
[%%expect{|
Line 1, characters 66-73:
1 | let under_hyp (x : {v:int | v >= 0}) : {w:int | w >= 0} = refine_ (x - 1)
                                                                      ^^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: x - 1 >= 0
Hypotheses:
  x >= 0
Counterexample (validated -- every hypothesis holds and the goal fails here):
  x = 0
|}]

(* x*x >= 0 is TRUE but nonlinear; grind gives up with the bogus atom
   model [x*x = -1].  The verdict must be NOT PROVED -- no witness. *)
let nonlinear_true (x : int) : {v:int | v >= 0} = refine_ (x * x)
[%%expect{|
Line 1, characters 58-65:
1 | let nonlinear_true (x : int) : {v:int | v >= 0} = refine_ (x * x)
                                                              ^^^^^^^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: x * x >= 0
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

(* (a - b)^2 >= 0 spelled out: also true, also nonlinear, also UNKNOWN
   -- enumeration over the small integer spread finds no counterexample. *)
let amgm (a : int) (b : int) : {v:int | (a * a) + (b * b) >= 2 * (a * b)} =
  refine_ 0
[%%expect{|
Line 2, characters 10-11:
2 |   refine_ 0
              ^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: a * a + b * b >= 2 * (a * b)
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

(* A quantified goal is not evaluable (we cannot enumerate all [k]), so
   even though it is TRUE the honest verdict is NOT PROVED, never a
   claimed counterexample. *)
let quantified : {v:int | forall_ k. k * k >= 0} = refine_ 0
[%%expect{|
Line 1, characters 59-60:
1 | let quantified : {v:int | forall_ k. k * k >= 0} = refine_ 0
                                                               ^
Error: vox: verification failed -- NOT PROVED (automation gave up; no counterexample was found, so the property may still hold).
       Goal: forall_ k. k * k >= 0
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

type ilist =
  | Nil
  | Cons of int * ilist

let rec total_ sorted l =
  match l with
  | Nil -> true
  | Cons (x, t) ->
    (match t with
     | Nil -> true
     | Cons (y, _) -> x <= y && sorted t)
[%%expect{|
type ilist = Nil | Cons of int * ilist
val sorted : ilist -> bool = <fun>
|}]

(* A false property over a datatype: the concrete list [2; 1] is not
   sorted, so [sorted] (a reflected function, EVALUATED by the solver)
   disproves the goal. *)
let unsorted_ground : {v:ilist | sorted v} = refine_ (Cons (2, Cons (1, Nil)))
[%%expect{|
Line 1, characters 53-78:
1 | let unsorted_ground : {v:ilist | sorted v} = refine_ (Cons (2, Cons (1, Nil)))
                                                         ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: sorted (Cons (2, Cons (1, Nil)))
Hypotheses: <none>
The goal is false unconditionally.
|}]

(* A datatype counterexample found by ENUMERATION: for every [n] the
   list [n+1; n] is out of order, so [ord2] fails; the search reports
   the smallest witness. *)
let total_ ord2 l =
  match l with
  | Nil -> true
  | Cons (x, t) -> (match t with Nil -> true | Cons (y, _) -> x <= y)
[%%expect{|
val ord2 : ilist -> bool = <fun>
|}]

let ordered_pair (n : int) : {v:ilist | ord2 v} = refine_ (Cons (n + 1, Cons (n, Nil)))
[%%expect{|
Line 1, characters 58-87:
1 | let ordered_pair (n : int) : {v:ilist | ord2 v} = refine_ (Cons (n + 1, Cons (n, Nil)))
                                                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: ord2 (Cons (n + 1, Cons (n, Nil)))
Hypotheses: <none>
Counterexample (validated -- every hypothesis holds and the goal fails here):
  n = 0
|}]
