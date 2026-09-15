(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Level_proofs
open Level_unifier_spec
open Level_unifier_proofs
open Level_unifier_metadata

let () =
  let refine_ state = Pref.empty () in
  let variable = cell Var 0 in let refine_ step = Pref.alloc variable state in
  let p = step.value in let state = step.state in
  let young = cell Var 4 in let refine_ step = Pref.alloc young state in
  let a = step.value in let state = step.state in
  let arrow = cell (Arrow (a, a)) 4 in let refine_ step = Pref.alloc arrow state in
  let q = step.value in let state = step.state in
  let saved = ghost_ (Pref.own (borrow_ state)) in
  let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || finite_scope saved x}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; cell_def desc 4; let desc = Arrow (a, a) in cell_def desc 4;
    active_def saved x; at_level_def saved x; active_def saved a; at_level_def saved a;
    finite_scope_def saved x; source_ok_def saved x; let u = () in refine_ u) in
  let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered saved x}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; cell_def desc 4; let desc = Arrow (a, a) in cell_def desc 4;
    below_def saved a 4; at_level_def saved a; children_below_def saved desc 4;
    let desc = Var in children_below_def saved desc 0; children_below_def saved desc 4;
    ordered_def saved x; let u = () in refine_ u) in
  ghost_ (let desc = Var in cell_def desc 0; let desc = Arrow (a, a) in cell_def desc 4;
    active_def saved p; at_level_def saved p; active_def saved q; at_level_def saved q);
  let state : {t : node Pref.token | Pref.own t === saved && H.mem saved p && H.mem saved q && active saved p && active saved q} = refine_ state in
  let unmarked : ((x : node Pref.t) @ immutable ->
    {u : unit | match H.at saved x with None -> true | Some v -> not v.visited}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; cell_def desc 4; let desc = Arrow (a, a) in cell_def desc 4;
    let u = () in refine_ u) in
  let refine_ out = Level_unifier.unify saved scope unmarked p q state in
  assert out.#ok;
  let h = ghost_ (Pref.own (borrow_ out.#state)) in let d = ghost_ out.#derivation in let ok = out.#ok in
  let _scope_after : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = unified_scope saved scope p q ok h d x (refine_ u) in refine_ u) in
  let _order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x}) @ total ghost = ghost_ (fun x -> order x; let u = () in let refine_ u = unified_ordered saved p q ok h d x (refine_ u) in refine_ u) in
  ghost_ (let u = () in unified_frame saved p q ok h d p (refine_ u);
    unified_frame saved p q ok h d q (refine_ u); unified_frame saved p q ok h d a (refine_ u));
  let state = out.#state in
  let old : node = let t : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in let refine_ v = Pref.read p t in v in
  let target : node = let t : {t : node Pref.token | H.mem (Pref.own t) q} = refine_ state in let refine_ v = Pref.read q t in v in
  let child : node = let t : {t : node Pref.token | H.mem (Pref.own t) a} = refine_ state in let refine_ v = Pref.read a t in v in
  assert (old.desc = Link q && target.level = Finite 0 && child.level = Finite 0);
  assert (old.memo = Empty_memo && target.memo = Empty_memo && child.memo = Empty_memo);
  ()
