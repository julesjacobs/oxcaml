(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml hm_declarative.ml hm_type_proofs.ml hm_environment_spec.ml hm_execution_spec.ml hm_execution_proofs.ml hm_forest_proofs.ml hm_model_proofs.ml hm_runtime_spec.ml hm_runtime_proofs.ml hm_sound_proofs.ml hm_environment_proofs.ml hm_protected_proofs.ml hm_registration_proofs.ml hm_let_runtime_proofs.ml hm_infer.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml hm_origin_proofs.ml hm_let_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Hm_execution_spec
module D = Hm_declarative

type sample = Id_id | Nested | Shared | Recursive_let | Bad_body | Bad_rhs | Monomorphic
let[@def] (expression @ total) (s : sample) =
  let n0 = D.Z in let n1 = D.S n0 in let _n2 = D.S n1 in
  let e0 = D.Bound n0 in
  let e1 = D.Lambda e0 in
  let e2 = D.Apply (e0, e0) in
  let e3 = D.Let (e1, e2) in
  let e4 = D.Truth in
  let e5 = D.Apply (e0, e4) in
  let e6 = D.Bound n1 in
  let e7 = D.Apply (e6, e6) in
  let e8 = D.Let (e5, e7) in
  let e9 = D.Let (e1, e8) in
  let e10 = D.Lambda e6 in
  let e11 = D.Let (e10, e5) in
  let e12 = D.Lambda e11 in
  let e13 = D.Apply (e6, e0) in
  let e14 = D.Recursive e13 in
  let e15 = D.Let (e14, e5) in
  let e16 = D.Let (e4, e2) in
  let e17 = D.Apply (e4, e4) in
  let e18 = D.Let (e17, e4) in
  let e19 = D.Lambda e8 in
  match s with
  | Id_id -> e3
  | Nested -> e9
  | Shared -> e12
  | Recursive_let -> e15
  | Bad_body -> e16
  | Bad_rhs -> e18
  | Monomorphic -> e19

let (scoped_fixture @ total) : (s : sample) ->
    {u : unit | D.scoped_term D.Z (expression s)} @ ghost = fun s -> ghost_ (
  expression_def s;
  let n0 = D.Z in let n1 = D.S n0 in let n2 = D.S n1 in let _n3 = D.S n2 in
  let e0 = D.Bound n0 in
  let e1 = D.Lambda e0 in
  let e2 = D.Apply (e0, e0) in
  let e3 = D.Let (e1, e2) in
  let e4 = D.Truth in
  let e5 = D.Apply (e0, e4) in
  let e6 = D.Bound n1 in
  let e7 = D.Apply (e6, e6) in
  let e8 = D.Let (e5, e7) in
  let e9 = D.Let (e1, e8) in
  let e10 = D.Lambda e6 in
  let e11 = D.Let (e10, e5) in
  let e12 = D.Lambda e11 in
  let e13 = D.Apply (e6, e0) in
  let e14 = D.Recursive e13 in
  let e15 = D.Let (e14, e5) in
  let e16 = D.Let (e4, e2) in
  let e17 = D.Apply (e4, e4) in
  let e18 = D.Let (e17, e4) in
  let e19 = D.Lambda e8 in
  D.scoped_term_def n0 e3;
  D.scoped_term_def n0 e1;
  D.scoped_term_def n1 e0;
  D.present_def n1 n0;
  D.scoped_term_def n1 e2;
  D.scoped_term_def n0 e9;
  D.scoped_term_def n1 e8;
  D.scoped_term_def n1 e5;
  D.scoped_term_def n1 e4;
  D.scoped_term_def n2 e7;
  D.scoped_term_def n2 e6;
  D.present_def n2 n1;
  D.present_def n1 n0;
  D.scoped_term_def n0 e12;
  D.scoped_term_def n1 e11;
  D.scoped_term_def n1 e10;
  D.scoped_term_def n2 e5;
  D.scoped_term_def n2 e0;
  D.present_def n2 n0;
  D.scoped_term_def n2 e4;
  D.scoped_term_def n0 e15;
  D.scoped_term_def n0 e14;
  D.scoped_term_def n2 e13;
  D.scoped_term_def n0 e16;
  D.scoped_term_def n0 e4;
  D.scoped_term_def n0 e18;
  D.scoped_term_def n0 e17;
  D.scoped_term_def n0 e19;
  let u = () in refine_ u)

let run sample expected =
  let e = expression sample in ghost_ (scoped_fixture sample);
  let e : {e : D.term | D.scoped_term D.Z e} = refine_ e in
  let refine_ out = Hm_infer.closed_hm e in let refine_ e = e in
  assert (Option.is_some out.#value = expected);
  ghost_ (let h = H.empty () in let depth = 0 in
    let pool : Generalize_spec.pool = Generalize_spec.Empty in let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
    let after = Pref.own (borrow_ out.#state) in
    let facts : ((x : node Pref.t) @ immutable -> {u : unit | Hm_runtime_spec.runtime_at h depth pool x}) @ total = fun x ->
      Hm_runtime_spec.runtime_at_def h depth pool x; Hm_runtime_spec.safe_def h x;
      Hm_runtime_spec.depth_bound_def h depth x; let cut = depth - 1 in
      Generalize_spec.covered_def h cut pool x; Level_spec.ordered_def h x; let u = () in refine_ u in
    let cut = -1 in
    let prior : ((x : node Pref.t) @ immutable ->
      {o : Provenance_spec.origin | not (Level_spec.below h x cut) || Provenance_spec.originates h h cut x o} @ immutable) @ total = fun x ->
      let refine_ o = Provenance_proofs.initial_origin h cut x in refine_ o in
    let _origins : ((p : node Pref.t) @ immutable ->
      {o : Provenance_spec.origin | not (Level_spec.below after p cut) || Provenance_spec.originates h after cut p o} @ immutable) @ total = fun p ->
      let u = () in let refine_ o = Hm_origin_proofs.run_origin h cut h depth pool facts prior env out.#execution after out.#pool p (refine_ u) in refine_ o in ());
  match out.#value with None -> () | Some p ->
    let after = ghost_ (Pref.own (borrow_ out.#state)) in
    ghost_ (let u = () in
      let refine_ _tree = Hm_forest_proofs.closed_forest out.#execution after out.#pool p (refine_ u) in
      let h = H.empty () in let pool : Generalize_spec.pool = Generalize_spec.Empty in
      let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
      Hm_execution_proofs.run_result h 0 pool env out.#execution after out.#pool p (refine_ u));
    let state = out.#state in let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ v = Pref.read p (borrow_ state) in
    assert (v.level = Finite 0 && not v.visited)

let () =
  run Id_id true; run Nested true; run Shared true; run Recursive_let true;
  run Bad_body false; run Bad_rhs false; run Monomorphic false

let overflow : unit -> inference @ unique = fun () ->
  let refine_ state = Pref.empty () in let h = ghost_ (Pref.own (borrow_ state)) in
  let depth = 4611686018427387903 in
  let pool : Generalize_spec.pool = Generalize_spec.Empty in
  let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
  let rhs = D.Truth in let e = D.Let (rhs, rhs) in
  let facts : ((x : node Pref.t) @ immutable -> {u : unit | Hm_runtime_spec.runtime_at h depth pool x}) @ total ghost = ghost_ (fun x ->
    Hm_runtime_spec.runtime_at_def h depth pool x; Hm_runtime_spec.safe_def h x;
    Hm_runtime_spec.depth_bound_def h depth x; let cut = depth - 1 in
    Generalize_spec.covered_def h cut pool x; Level_spec.ordered_def h x; let u = () in refine_ u) in
  ghost_ (let z = D.Z in let one = D.S z in D.scoped_term_def z e;
    D.scoped_term_def z rhs; D.scoped_term_def one rhs;
    Generalize_spec.pool_scoped_def h pool; Hm_runtime_spec.env_owned_def h env;
    Hm_runtime_spec.env_depth_def env);
  let state : {t : node Pref.token | Pref.own t === h && depth >= 0 && Generalize_spec.pool_scoped h pool
    && Hm_runtime_spec.env_owned h env && D.scoped_term (Hm_runtime_spec.env_depth env) e} = refine_ state in
  let refine_ out = Hm_infer.infer h depth pool facts env e state in
  #{value = out.#value; state = out.#state; pool = out.#pool; execution = out.#execution}

let () = match overflow () with _ -> assert false | exception Assert_failure _ -> ()
