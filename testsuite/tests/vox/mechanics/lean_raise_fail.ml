(* TEST
 script = "sh ${test_source_directory}/../has-lean.sh";
 script;
 expect;
*)

(* vox Milestone 0 (soundness).  The bottom classification must NOT fire
   for a call that returns.  Each obligation is a ground [0 = 1] that
   only a [false] hypothesis could discharge; since no [false] is added,
   the goal is correctly DISPROVED. *)

(* (6) [Obj.magic : 'a -> 'b] is the external "%identity" -- a result
   variable in no argument, yet it RETURNS.  Externals bypass the scheme
   test, so it is NOT classified and [0 = 1] still fails. *)
let magic_returns () : int =
  let _ = Obj.magic () in
  let refine_ r = (0 : int{ _ = 1 }) in
  r
[%%expect{|
Line 3, characters 19-20:
3 |   let refine_ r = (0 : int{ _ = 1 }) in
                       ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: 0 = 1
Hypotheses: <none>
The goal is false unconditionally.
|}]

(* (7) The identity [('a -> 'a)] must not classify: its result variable
   occurs in its argument, so a terminating call can produce it. *)
let id (x : 'a) : 'a = x
[%%expect{|
val id : 'a -> 'a = <fun>
|}]

let id_returns () : int =
  let _ = id () in
  let refine_ r = (0 : int{ _ = 1 }) in
  r
[%%expect{|
Line 3, characters 19-20:
3 |   let refine_ r = (0 : int{ _ = 1 }) in
                       ^
Error: vox: verification failed -- goal DISPROVED (a counterexample was validated).
       Goal: 0 = 1
Hypotheses: <none>
The goal is false unconditionally.
|}]
