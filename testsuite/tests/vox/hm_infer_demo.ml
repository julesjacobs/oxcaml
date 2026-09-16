(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml hm_type_proofs.ml hm_execution_spec.ml hm_execution_proofs.ml hm_forest_proofs.ml hm_model_proofs.ml hm_runtime_spec.ml hm_runtime_proofs.ml hm_sound_proofs.ml hm_environment_proofs.ml hm_protected_proofs.ml hm_registration_proofs.ml hm_let_runtime_proofs.ml hm_infer.ml hm_infer_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Hm_declarative
open Hm_runtime_spec

type sample = Constant | Identity | Applied | Loop | Self_return | Omega | Bad
let[@def] (expression @ total) (s : sample) =
  let x = Bound Z in let self = Bound (S Z) in match s with
  | Constant -> Truth | Identity -> Lambda x | Applied -> Apply (Lambda x, Truth)
  | Loop -> Recursive (Apply (self, x)) | Self_return -> Recursive self
  | Omega -> Lambda (Apply (x, x)) | Bad -> Apply (Truth, Truth)

let (scoped_fixture @ total) : (s : sample) ->
    {u : unit | scoped_term Z (expression s) && term_let_free (expression s)} @ ghost = fun s -> ghost_ (
  expression_def s;
  let z = Z in let one = S z in let two = S one in
  let x = Bound z in let self = Bound one in let truth = Truth in
  let id = Lambda x in let call = Apply (self, x) in let loop = Recursive call in
  let self_return = Recursive self in let omega_body = Apply (x, x) in let omega = Lambda omega_body in
  let applied = Apply (id, truth) in let bad = Apply (truth, truth) in
  scoped_term_def z truth; scoped_term_def one x; present_def one z;
  scoped_term_def two x; present_def two z; scoped_term_def two self;
  present_def two one; scoped_term_def two call; scoped_term_def z loop;
  scoped_term_def z self_return; scoped_term_def z id; scoped_term_def z applied;
  scoped_term_def z bad; scoped_term_def one omega_body; scoped_term_def z omega;
  term_let_free_def truth; term_let_free_def x; term_let_free_def self;
  term_let_free_def id; term_let_free_def applied; term_let_free_def bad;
  term_let_free_def call; term_let_free_def loop; term_let_free_def self_return;
  term_let_free_def omega_body; term_let_free_def omega;
  let u = () in refine_ u)


let run sample expected =
  let e = expression sample in ghost_ (scoped_fixture sample);
  let e : {e : term | scoped_term Z e && term_let_free e} = refine_ e in
  let refine_ out = Hm_infer.closed e in
  let refine_ e = e in
  assert (Option.is_some out.#value = expected);
  match out.#value with
  | None -> ()
  | Some p ->
    let after = ghost_ (Pref.own (borrow_ out.#state)) in
    ghost_ (let h = H.empty () in let pool : Generalize_spec.pool = Generalize_spec.Empty in
      let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
      let u = () in Hm_execution_proofs.run_result h 0 pool env out.#execution after out.#pool p (refine_ u);
      let refine_ tree = Hm_forest_proofs.closed_forest out.#execution after out.#pool p (refine_ u) in
      let refine_ _typing = Hm_sound_proofs.closed_sound out.#execution after out.#pool p tree (refine_ u) in ());
    let state = out.#state in let state : {t : Pref.token | H.mem (Pref.own t) p} = refine_ state in
    let refine_ v = Pref.read p (borrow_ state) in
    assert (v.level = Finite 0 && not v.visited)

let () =
  run Constant true; run Identity true; run Applied true; run Loop true;
  run Self_return false; run Omega false; run Bad false
