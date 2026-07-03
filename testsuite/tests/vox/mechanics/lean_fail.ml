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
Error: vox: verification failed (lean).
       Goal: 0 > 0
Hypotheses: <none>
(lean: error: `grind` failed)
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
Error: vox: verification failed (lean).
       Goal: a > 0
Hypotheses:
  a#2 > 0
Possible counterexample:
  a = 0
  a#2 = 1
(lean: error: `grind` failed)
|}]

(* A phrase that fails verification is backtracked by the toplevel; its
   facts must NOT survive into later phrases.  The contradictory fact
   from [contra] would otherwise prove [oops]'s (false) obligation. *)
let contra : {v:int | v > 0 && not (v > 0)} = refine_ 0
[%%expect{|
Line 1, characters 54-55:
1 | let contra : {v:int | v > 0 && not (v > 0)} = refine_ 0
                                                          ^
Error: vox: verification failed (lean).
       Goal: (0 > 0) && (not (0 > 0))
Hypotheses: <none>
(lean: error: `grind` failed)
|}]

let oops : {v:int | v = 42} = refine_ 0
[%%expect{|
Line 1, characters 38-39:
1 | let oops : {v:int | v = 42} = refine_ 0
                                          ^
Error: vox: verification failed (lean).
       Goal: 0 = 42
Hypotheses: <none>
(lean: error: `grind` failed)
|}]
