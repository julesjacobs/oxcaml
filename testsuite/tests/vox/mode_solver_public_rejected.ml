(* TEST
 has-z3;
 flags = "-extension refinement_types";
 prebuilt_modules = "mode_solver_semantics.mli mode_solver_semantics.ml mode_solver_three_qe_proof.ml mode_solver_graph_semantics.mli mode_solver_graph_semantics.ml mode_solver_graph_qe_proof.ml mode_solver_guarded_semantics.mli mode_solver_guarded_semantics.ml mode_solver_retained_symbolic.ml mode_solver_public.mli mode_solver_public.ml";
 {
   setup-ocamlc.opt-build-env;
   run-expect;
   check-program-output;
 }
*)

open Mode_solver_semantics;;
open Mode_solver_public;;

let (elimination_preserves_syntax @ total) (f : formula) :
    {u : unit | eliminate f === Le (Const Global, Const Local)} @ ghost =
  ghost_ ();;
[%%expect{|
Line 6, characters 9-11:
6 |   ghost_ ();;
             ^^
Error: Refinement could not be proved (counterexample)
Line 5, characters 16-62:
5 |     {u : unit | eliminate f === Le (Const Global, Const Local)} @ ghost =
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  The refinement is stated here.
|}]

let (wrong_decision @ total) () :
    {u : unit | decide_checked (Plain (Le (Const Local, Const Global)))
      === Some true} @ ghost =
  ghost_ (
    let atom = Le (Const Local, Const Global) in
    scoped_def 0 (Plain atom); scoped_qf_def 0 atom;
    scoped_term_def 0 (Const Local); scoped_term_def 0 (Const Global);
    eval_def [] (Plain atom); eval_qf_def [] atom;
    eval_term_def [] (Const Local); eval_term_def [] (Const Global);
    le_def Local Global;
    ());;
[%%expect{|
Line 11, characters 4-6:
11 |     ());;
         ^^
Error: Refinement could not be proved (counterexample)
Lines 2-3, characters 16-19:
2 | ................decide_checked (Plain (Le (Const Local, Const Global)))
3 |       === Some true...........
  The refinement is stated here.
|}]
