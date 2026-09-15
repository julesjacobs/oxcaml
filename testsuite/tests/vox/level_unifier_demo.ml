(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml level_unifier_proofs.ml level_unifier_metadata.ml level_unifier.ml level_unifier_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_unifier_metadata

let run mode =
  let refine_ state = Pref.empty () in
  let v = cell Var 0 in let refine_ step = Pref.alloc v state in
  let a = step.value in let state = step.state in
  let v = cell Bool 4 in let refine_ step = Pref.alloc v state in
  let b = step.value in let state = step.state in
  let left_desc = Arrow (a, a) in let v = cell left_desc 4 in let refine_ step = Pref.alloc v state in
  let left = step.value in let state = step.state in
  let child = if mode = 2 then left else b in
  let right_desc = Arrow (b, child) in let v = cell right_desc 4 in let refine_ step = Pref.alloc v state in
  let right = step.value in let state = step.state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let active_all : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || active h x}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; let desc = Bool in cell_def desc 4;
    cell_def left_desc 4; cell_def right_desc 4;
    active_def h x; at_level_def h x; let u = () in refine_ u) in
  let scope : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || finite_scope h x}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; let desc = Bool in cell_def desc 4;
    cell_def left_desc 4; cell_def right_desc 4;
    active_all a; active_all b; active_all child;
    finite_scope_def h x; source_ok_def h x; let u = () in refine_ u) in
  let p, q = match mode with 1 -> a, left | 3 -> b, left | 4 -> a, right | 5 -> a, a | _ -> left, right in
  ghost_ (active_all p; active_all q);
  let state : {t : Pref.token | Pref.own t === h && H.mem h p && H.mem h q && active h p && active h q} = refine_ state in
  let refine_ result = Level_unifier.unify h scope p q state in
  let ok = result.#ok in let d = ghost_ result.#derivation in
  let after = ghost_ (Pref.own (borrow_ result.#state)) in
  ghost_ (let u = () in unified_frame h p q ok after d a (refine_ u));
  let state = result.#state in let state : {t : Pref.token | H.mem (Pref.own t) a} = refine_ state in
  let refine_ av = Pref.read a (borrow_ state) in
  match mode with
  | 0 -> assert ok; assert (av.desc = Link b)
  | 1 | 3 -> assert (not ok); assert (av.desc = Var)
  | 2 -> assert (not ok); assert (av.desc = Link b)
  | 4 -> assert ok; assert (av.desc = Link right)
  | _ -> assert ok; assert (av.desc = Var)

let () = List.iter run [0; 1; 2; 3; 4; 5]
