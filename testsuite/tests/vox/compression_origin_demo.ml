(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml compression_origin_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Generalize_spec
open Provenance_spec

let (lost_origin @ total) : (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (r : node Pref.t) @ immutable -> (o : origin) @ immutable ->
    {u : unit | not (p === q) && not (p === r) && not (q === r)} ->
    {u : unit | let saved = H.put (H.empty ()) p (cell Var 0) in
      let after = H.put (H.put (H.put (H.empty ()) p (cell (Link r) 0)) q (cell (Link r) 0)) r (cell Bool 0) in
      not (originates saved after 0 q o)} @ ghost = fun p q r o premise -> ghost_ (
    let refine_ premise = premise in let var : desc = Var in let link = Link r in let boolean : desc = Bool in
    cell_def var 0; cell_def link 0; cell_def boolean 0;
    let h = H.empty () in let saved = H.put h p (cell var 0) in
    let after = H.put (H.put (H.put h p (cell link 0)) q (cell link 0)) r (cell boolean 0) in
    originates_def saved after 0 q o;
    let u = () in match o with Origin (root, path) ->
      below_def saved root 0; reaches_def after root q path;
      match path with Stop -> refine_ u | Step (next, rest) ->
        edge_def after root next; reaches_def after next q rest;
        match rest with Stop -> refine_ u | Step (last, _) -> edge_def after next last; refine_ u)

let (prior_origin @ total) : (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (r : node Pref.t) @ immutable ->
    {u : unit | not (p === q) && not (p === r) && not (q === r)} ->
    {u : unit | let saved = H.put (H.empty ()) p (cell Var 0) in
      let before = H.put (H.put (H.put (H.empty ()) p (cell (Link q) 0)) q (cell (Link r) 0)) r (cell Bool 0) in
      originates saved before 0 q (Origin (p, Step (q, Stop))) && below before q 0} @ ghost = fun p q r premise -> ghost_ (
    let refine_ premise = premise in let var : desc = Var in let first = Link q in let second = Link r in let boolean : desc = Bool in
    cell_def var 0; cell_def first 0; cell_def second 0; cell_def boolean 0;
    let h = H.empty () in let saved = H.put h p (cell var 0) in
    let before = H.put (H.put (H.put h p (cell first 0)) q (cell second 0)) r (cell boolean 0) in
    let stop = Stop in let path = Step (q, stop) in let origin = Origin (p, path) in
    originates_def saved before 0 q origin; below_def saved p 0; at_level_def saved p;
    reaches_def before p q path; edge_def before p q; reaches_def before q q stop;
    below_def before q 0; at_level_def before q; let u = () in refine_ u)

let (finite_readback @ total) : (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (r : node Pref.t) @ immutable ->
    {u : unit | not (p === q) && not (p === r) && not (q === r)} -> {u : unit | true} @ ghost = fun p q r premise -> ghost_ (
    let refine_ premise = premise in let first = Link q in let second = Link r in let boolean : desc = Bool in
    cell_def first 0; cell_def second 0; cell_def boolean 0;
    let h = H.put (H.put (H.put (H.empty ()) p (cell first 0)) q (cell second 0)) r (cell boolean 0) in
    let leaf = Level_finite_spec.Constant_tree r in let child = Level_finite_spec.Alias_tree (q, leaf) in
    let tree = Level_finite_spec.Alias_tree (p, child) in
    Level_finite_spec.tree_root_def leaf; Level_finite_spec.tree_root_def child; Level_finite_spec.tree_root_def tree;
    Level_finite_spec.finite_def h leaf; Level_finite_spec.finite_def h child; Level_finite_spec.finite_def h tree;
    Level_unifier_spec.observe_def h p; Level_unifier_spec.observe_def h q; Level_unifier_spec.observe_def h r;
    let here = Level_unifier_spec.Here in let tail = Level_unifier_spec.Via (r, here) in
    let path = Level_unifier_spec.Via (q, tail) in
    Level_unifier_spec.resolves_def h p r path; Level_unifier_spec.resolves_def h q r tail;
    Level_unifier_spec.resolves_def h r r here; Level_unifier_spec.terminal_def h r;
    let u = () in Compression_finite_proofs.finite_compress h p q r path tree (refine_ u);
    Compression_finite_proofs.compress_readback p tree; refine_ u)
