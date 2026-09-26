(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hmc_word64.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml effective_copy_datatype_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Generalize_spec
open Copy_cleanup_spec
open Effective_copy_spec
open Effective_copy_heap_proofs
module M = Effective_copy_metadata
module E = Effective_level
module R = Representative_level
module U = Level_unifier_spec
module C = Effective_copy_runtime

let[@def] (head @ total) (p : node Pref.t @ immutable) (q : node Pref.t @ immutable)
    (x : node Pref.t @ immutable total) : R.representative @ immutable total =
  let refine_ same = Pref.equal x p in
  if same then {R.root = q; path = U.Via (q, U.Here)} else {R.root = x; path = U.Here}

let run generic word =
  let refine_ state = Pref.empty () in
  let level = if generic then Generic else Finite 0 in
  let desc : desc = if word then Word else Var in
  let leaf = {desc; level; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc leaf state in let q = step.value in let state = step.state in
  let alias = {desc = Link q; level = Finite 7; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc alias state in let p = step.value in let state = step.state in
  let pair = {desc = List p; level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc pair state in let root = step.value in let state = step.state in
  let saved = ghost_ (Pref.own (borrow_ state)) in
  let base = Empty in let depth = 1 in
  let c = {Effective_copy_spec.saved = ghost_ saved; epoch = ghost_ root; depth = ghost_ depth; base = ghost_ base} in
  let heads : E.heads Ghost.t = {Ghost.ghost = ghost_ (head p q)} in
  let scope : (((x : node Pref.t) @ immutable -> {u : unit |
      not (H.mem c.Effective_copy_spec.saved x) || source_ok c.Effective_copy_spec.saved x})) Ghost.t = {Ghost.ghost = ghost_ (fun x ->
    source_ok_def c.Effective_copy_spec.saved x; let u = () in refine_ u)} in
  let clean : (((x : node Pref.t) @ immutable -> {u : unit |
      match H.at c.Effective_copy_spec.saved x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in refine_ u)} in
  let witness : (((x : node Pref.t) @ immutable -> {u : unit |
      E.valid_head c.Effective_copy_spec.saved heads.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (fun x ->
    E.valid_head_def c.Effective_copy_spec.saved heads.Ghost.ghost x;
    head_def p q x; let refine_ same = Pref.equal x p in
    let r = heads.Ghost.ghost x in U.resolves_def c.Effective_copy_spec.saved x r.root r.path;
    U.terminal_def c.Effective_copy_spec.saved x; U.observe_def c.Effective_copy_spec.saved x;
    let here = U.Here in U.resolves_def c.Effective_copy_spec.saved q q here;
    U.terminal_def c.Effective_copy_spec.saved q; U.observe_def c.Effective_copy_spec.saved q;
    let u = () in refine_ u)} in
  let state : {t : node Pref.token | Pref.own t === c.Effective_copy_spec.saved && H.mem c.Effective_copy_spec.saved root} = refine_ state in
  let refine_ state = state in

  let refine_ out = C.instantiate c heads scope clean witness (refine_ depth) base root (refine_ state) in
  let result = out.#value in let state = out.#state in let d = ghost_ out.#history in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ (let u = () in target_allocated c.Effective_copy_spec.saved heads.Ghost.ghost root depth d root result (refine_ u);
    M.result_at c.Effective_copy_spec.saved heads.Ghost.ghost root depth d result (refine_ u);
    let raw = heap c.Effective_copy_spec.saved root depth d in let trail = Pooled_spec.touched d in
    swept_at_def raw after trail result; ());
  let state : {t : node Pref.token | H.mem (Pref.own t) result} = refine_ state in
  let refine_ copied = Pref.read result (borrow_ state) in let refine_ state = state in
  assert (copied.level = Finite 1);
  (match copied.desc with
   | List a -> let refine_ same = Pref.equal a p in assert (same = not generic)
   | _ -> failwith "expected list constructor");
  (match out.#pool with
   | Entry (_, Empty) -> assert (not generic)
   | Entry (_, Entry (_, Empty)) -> assert generic
   | _ -> failwith "unexpected allocation count");
  ghost_ (let u = () in M.saved_observe c.Effective_copy_spec.saved heads.Ghost.ghost root depth d p (refine_ u);
    M.saved_observe c.Effective_copy_spec.saved heads.Ghost.ghost root depth d q (refine_ u));
  let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
  let refine_ original = Pref.read p (borrow_ state) in let refine_ state = state in
  assert (original.level = Finite 7); assert (original.desc = Link q);
  assert (original.memo = Empty_memo);
  let state : {t : node Pref.token | H.mem (Pref.own t) q} = refine_ state in
  let refine_ original = Pref.read q (borrow_ state) in let refine_ state = state in
  assert (original.memo = Empty_memo);
  ()

let () = run false false; run true false; run false true; run true true
