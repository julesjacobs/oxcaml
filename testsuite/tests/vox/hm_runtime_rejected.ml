(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml nested_pool_spec.ml copy_cleanup_spec.ml hm_execution_spec.ml hm_runtime_spec.ml fast_environment.ml fast_term.ml";
 readonly_files = "hm_runtime_rejected.ml";
 compile_only = "true";
 {
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   run-expect;
   check-program-output;
 }
 {
   setup-ocamlopt.byte-build-env;
   ocamlopt.byte;
   run-expectnat;
   check-program-output;
 }
*)
open Copy_spec;;
open Hm_declarative;;
open Hm_runtime_spec;;
[%%expect{|
|}]

module Unbound_closed_variable = struct
  let bad : {e : term | scoped_term Z e && term_let_free e} =
    let z = Z in let e = Bound z in ghost_ (scoped_term_def z e; present_def z z);
    refine_ e
end;;
[%%expect{|
Line 4, characters 4-13:
4 |     refine_ e
        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Let_in_let_free_driver = struct
  let bad : {e : term | term_let_free e} =
    let e = Let (Truth, Bound Z) in ghost_ (term_let_free_def e);
    refine_ e
end;;
[%%expect{|
Line 4, characters 4-13:
4 |     refine_ e
        ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Missing_mark_invariant = struct
  let bad : (h : Pref.heap) @ immutable -> (x : node Pref.t) @ immutable ->
      {u : unit | safe h x} @ ghost = fun h x -> ghost_ (
    safe_def h x; let u = () in refine_ u)
end;;
[%%expect{|
Line 4, characters 32-41:
4 |     safe_def h x; let u = () in refine_ u)
                                    ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Unchecked_level_increment = struct
  let bad : (depth : {n : int | n >= 0}) -> {n : int | n >= 0} = fun depth ->
    let refine_ depth = depth in let next = depth + 1 in refine_ next
end;;
[%%expect{|
Line 3, characters 57-69:
3 |     let refine_ depth = depth in let next = depth + 1 in refine_ next
                                                             ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Discard_parent_pool = struct
  let bad : (p : node Pref.t) @ immutable ->
      {u : unit | Generalize_spec.covered (H.put (H.empty ()) p (cell Var 0)) (-1) Generalize_spec.Empty p} @ ghost = fun p -> ghost_ (
    let h = H.empty () in let desc : desc = Var in let v = cell desc 0 in cell_def desc 0;
    let after = H.put h p v in let empty : Generalize_spec.pool = Generalize_spec.Empty in
    Generalize_spec.covered_def after (-1) empty p; Generalize_spec.listed_def empty p;
    Level_spec.at_level_def after p; let u = () in refine_ u)
end;;
[%%expect{|
Line 7, characters 51-60:
7 |     Level_spec.at_level_def after p; let u = () in refine_ u)
                                                       ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Invent_saved_origin = struct
  let (bad @ total) : (p : node Pref.t) @ immutable ->
      (o : Provenance_spec.origin) @ immutable ->
      {u : unit | let saved = H.empty () in
        let after = H.put saved p (cell Var 0) in
        Provenance_spec.originates saved after 0 p o} @ ghost = fun p o -> ghost_ (
    let saved = H.empty () in let desc : desc = Var in
    let after = H.put saved p (cell desc 0) in
    Provenance_spec.originates_def saved after 0 p o;
    match o with Provenance_spec.Origin (root, _) ->
      Level_spec.below_def saved root 0; Level_spec.at_level_def saved root;
      let u = () in refine_ u)
end;;
[%%expect{|
Line 12, characters 20-29:
12 |       let u = () in refine_ u)
                         ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Forget_boundary_agreement = struct
  let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun _p -> Boolean
  let[@def] eta : node Pref.t @ immutable total -> ty @ immutable total = fun _p -> Function (Boolean, Boolean)
  let (bad @ total) : (p : node Pref.t) @ immutable ->
      {u : unit | interpret rho eta (Boundary p) === eta p} @ ghost = fun p -> ghost_ (
    let schema = Boundary p in
    interpret_def rho eta schema; rho_def p; eta_def p;
    let u = () in refine_ u)
end;;
[%%expect{|
Line 8, characters 18-27:
8 |     let u = () in refine_ u)
                      ^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Forged_runtime_index = struct
  let bad : {t : Fast_term.term | Fast_term.valid t} @ immutable =
    let z = Hm_declarative.Z in
    let index : Fast_term.index = {number = 1; original = z} in
    let out = Fast_term.Bound index in
    ghost_ (Fast_term.valid_def out; Fast_environment.encoded_def z 1);
    refine_ out
end;;
[%%expect{|
Line 7, characters 4-15:
7 |     refine_ out
        ^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]

module Forged_tree_weight = struct
  let bad : (p : node Pref.t) @ immutable ->
      {t : Fast_environment.tree | Fast_environment.valid_tree t} @ immutable = fun p ->
    let leaf = Fast_environment.Leaf p in
    let tree = Fast_environment.Branch (4, p, leaf, leaf) in
    ghost_ (Fast_environment.valid_tree_def tree; Fast_environment.weight_def leaf);
    refine_ tree
end;;
[%%expect{|
Line 7, characters 4-16:
7 |     refine_ tree
        ^^^^^^^^^^^^
Error: Refinement could not be proved (counterexample)
|}]
