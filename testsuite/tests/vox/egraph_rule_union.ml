(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml vox_egraph_language_spec.ml vox_egraph_language_proof.ml vox_egraph_rule_spec.ml vox_egraph_rules.ml vox_egraph_derivation_spec.ml vox_egraph_derivation.ml vox_egraph_union_spec.ml vox_egraph_union.ml vox_egraph_origin_frame.ml vox_egraph_rule_semantics.ml vox_egraph_ghost_arrays.ml vox_egraph_rule_union.ml egraph_rule_union.ml";
 { bytecode; }
*)

module G = Vox_egraph_rule_union
module L = Vox_egraph_language_spec
module LP = Vox_egraph_language_proof
module R = Vox_egraph_rule_spec
module RP = Vox_egraph_rules
module E = Vox_egraph_derivation_spec
module EP = Vox_egraph_derivation

let () =
  let rule = {R.vars = []; lhs = R.Int_lit 0; rhs = R.Int_lit 1} in
  let rules = R.Rule_cons (rule, R.No_rules) in
  let initial = G.create rules in
  (match Sys.backend_type with
   | Sys.Native -> assert (Obj.size (Obj.repr initial) = 1)
   | Sys.Bytecode -> assert (Obj.size (Obj.repr initial) = 4)
   | Sys.Other _ -> ());
  let #{G.value = _; state = first} = G.add initial (L.Int_lit 0) in
  let #{G.value = _; state} = G.add first (L.Int_lit 1) in
  let proof = ghost_ (
    R.lookup_rule_def rules 0;
    R.rule_valid_def rule;
    R.pat_sort_def [] rule.lhs;
    R.pat_sort_def [] rule.rhs;
    R.vars_in_def rule.rhs rule.lhs;
    R.subst_valid_def [] [];
    R.instantiate_def rule.lhs [];
    R.instantiate_def rule.rhs [];
    G.valid_def initial;
    G.valid_def first;
    Vox_iarray.updated_read first.origins 1 (L.Int_lit 1) 0;
    G.S.origin_def state.origins 0;
    G.S.origin_def first.origins 0;
    EP.rule_instance rules 0 rule [] ()) in
  let #{G.merged; state} = G.merge_nodes state 0 1 proof in
  assert merged;
  assert (G.same state 0 1)
