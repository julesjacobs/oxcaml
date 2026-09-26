(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_egraph_language_spec.ml vox_egraph_rule_spec.ml vox_egraph_match_spec.ml vox_egraph_closure_spec.ml vox_egraph_quantifier_measure.ml vox_egraph_quantifier.mli vox_egraph_quantifier.ml vox_egraph_saturation_spec.ml vox_egraph_assignment_spec.ml vox_egraph_assignment_proof.ml vox_egraph_assignments.ml egraph_assignments.ml";
 { bytecode; }
*)
module C = Vox_egraph_assignment_spec
module A = Vox_egraph_assignments
module L = Vox_egraph_language_spec

let () =
  assert (A.enumerate 2 [L.Integer; L.Boolean] 0 = A.Exhausted);
  assert (A.enumerate 2 [L.Integer; L.Boolean] 10 = A.Exhausted);
  match A.enumerate 2 [L.Integer; L.Boolean] 1000 with
  | A.Exhausted -> assert false
  | A.Done (cases, left) ->
    assert (0 <= left && left < 1000);
    assert (cases = C.Case ([1; 1], C.Case ([1; 0], C.Case ([1; -1],
      C.Case ([0; 1], C.Case ([0; 0], C.Case ([0; -1],
      C.Case ([-1; 1], C.Case ([-1; 0], C.Case ([-1; -1], C.End))))))))))

let () =
  match A.enumerate 0 [L.Integer; L.Boolean] 100 with
  | A.Exhausted -> assert false
  | A.Done (cases, _) -> assert (cases = C.Case ([-1; -1], C.End))
