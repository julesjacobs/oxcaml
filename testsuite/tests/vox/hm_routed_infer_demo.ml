(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml vox_sequence.mli vox_sequence.ml vox_int_sequence.mli vox_int_sequence.ml vox_iarray.mli vox_iarray.ml borrow_iarray.mli borrow_iarray.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.mli effective_copy_runtime.ml representative_certificate.ml copy_certificate_spec.ml copy_certificate_capture.ml copy_certificate_proofs.ml certified_copy.mli certified_copy.ml hm_type_proofs.ml hm_abstraction.ml hm_abstraction_proofs.ml hm_substitution.ml hm_substitution_proofs.ml hm_freshness_proofs.ml hm_template_instance_proofs.ml hm_execution_spec.ml hm_effective_environment.ml hm_effective_variable.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml graph_occurs.mli graph_occurs.ml effective_compression_spec.ml effective_compression_proofs.ml effective_unifier_spec.ml effective_unifier_model.ml effective_unifier_finite.ml effective_unifier_frame.ml effective_unifier_mgu.ml effective_scan_proofs.ml effective_bind_proofs.ml effective_bind.mli effective_bind.ml effective_compression_metadata.ml effective_unifier_heads.ml effective_unifier_metadata.ml effective_unifier_order.ml graph_representative.mli graph_representative.ml effective_compressed_representative.mli effective_compressed_representative.ml effective_link_proofs.ml effective_link.mli effective_link.ml effective_unifier_runtime.mli effective_unifier_runtime.ml forest_heads.ml hm_effective_execution_spec.ml hm_effective_forest.ml hm_effective_model.ml effective_unifier_pool.ml hm_effective_runtime.ml hm_effective_allocation.ml effective_allocator.mli effective_allocator.ml hm_effective_copy_runtime.ml hm_effective_registration.ml hm_effective_bound.ml hm_effective_closing.ml hm_runtime_spec.ml hm_effective_paths.ml hm_effective_result.ml hm_effective_invariant.ml hm_effective_membership.ml hm_effective_driver_proofs.ml effective_unifier_protected.ml hm_effective_generic.ml hm_effective_freshness.ml hm_effective_generalization.ml hm_effective_environment_proofs.ml hm_scheme_transport_proofs.ml hm_environment_models.ml hm_effective_complete_helpers.ml terminal_lower_origin.ml effective_compression_origin.ml effective_unifier_origin.ml hm_effective_agreement.ml hm_effective_origin.ml hm_effective_complete.mli hm_effective_complete.ml hm_effective_sound.mli hm_effective_sound.ml hm_polymorphic_fixtures.ml fast_environment.mli fast_environment.ml fast_term.mli fast_term.ml effective_hm_unify.mli effective_hm_unify.ml pool_closing_equivalence.ml level_pool_routing_spec.ml level_pool_routing.mli level_pool_routing.ml level_pool_store.ml level_pool_execution.ml hm_routed_context.ml hm_pool_capacity.mli hm_pool_capacity.ml hm_routed_infer.mli hm_routed_infer.ml verified_hm.mli verified_hm.ml hm_routed_infer_demo.ml";
 { native; }
*)
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
  let input : {e : D.term | D.scoped_term D.Z e} = refine_ e in
  let refine_ out = Hm_routed_infer.closed_hm input in
  (match out.#value with None -> () | Some p -> ghost_ (
    let h = Pref.own (borrow_ out.#state) in let e = out.#execution in
    let pool = out.#pool in let u = () in
    let refine_ tree = Hm_effective_forest.closed_forest e h pool p (refine_ u) in
    let empty = Copy_spec.H.empty () in
    let trees : ((x : Copy_spec.node Pref.t) @ immutable ->
      {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
        (if Copy_spec.H.mem empty x then Level_finite_spec.finite empty t else Level_unifier_spec.observe empty x === None)} @ immutable) @ total = fun x ->
      let t = Level_finite_spec.Free x in Level_finite_spec.tree_root_def t;
      Level_unifier_spec.observe_def empty x; refine_ t in
    let pool0 = Generalize_spec.Empty in let env0 = Hm_environment_spec.Empty in
    Hm_effective_driver_proofs.run_result empty trees 0 pool0 env0 e h pool p (refine_ u);
    let refine_ _typing = Hm_effective_sound.closed_sound e h pool p tree (refine_ u) in ()));
  assert (Option.is_some out.#value = expected)

let () =
  run Id_id true; run Nested true; run Shared true; run Recursive_let true;
  run Bad_body false; run Bad_rhs false; run Monomorphic false

let deep_infer : (e : {e : D.term | D.scoped_term D.Z e}) @ immutable -> unit =
  fun e -> let refine_ out = Hm_routed_infer.closed_hm e in
    match out.#value with None -> assert false | Some _ -> ()

let rec deep_lambdas : int -> (body : D.term) @ immutable ->
    (scope : (((n : D.index) @ immutable ->
      {u : unit | D.scoped_term n body})) Ghost.t) @ total -> unit =
  fun count body scope ->
    if count <= 0 then (
      ghost_ (let z = D.Z in scope.Ghost.ghost z; ());
      let input : {e : D.term | D.scoped_term D.Z e} = refine_ body in
      deep_infer input;
      let z = D.Z in let one = D.S z in let two = D.S one in
      let variable = D.Bound z in
      let copied = D.Let (body, variable) in
      ghost_ (D.scoped_term_def z copied; D.scoped_term_def one variable;
        D.present_def one z; ());
      let input : {e : D.term | D.scoped_term D.Z e} = refine_ copied in
      deep_infer input;
      let identity = D.Lambda variable in
      let applied = D.Apply (identity, body) in
      ghost_ (D.scoped_term_def z applied; D.scoped_term_def z identity; ());
      let input : {e : D.term | D.scoped_term D.Z e} = refine_ applied in
      deep_infer input;
      let call = D.Apply (variable, body) in
      let outer = D.Bound one in
      let inner_call = D.Apply (outer, body) in
      let drop = D.Lambda inner_call in
      let calls = D.Apply (drop, call) in
      let unified = D.Lambda calls in
      ghost_ (scope.Ghost.ghost one; scope.Ghost.ghost two;
        D.scoped_term_def one call; D.scoped_term_def two inner_call;
        D.scoped_term_def two outer; D.present_def two one;
        D.scoped_term_def two variable; D.present_def two z;
        D.scoped_term_def one drop; D.scoped_term_def one calls;
        D.scoped_term_def z unified; ());
      let input : {e : D.term | D.scoped_term D.Z e} = refine_ unified in
      deep_infer input)
    else (
      let next = D.Lambda body in
      let scope : (((n : D.index) @ immutable ->
        {u : unit | D.scoped_term n next})) Ghost.t =
        {Ghost.ghost = ghost_ (fun n ->
          let more = D.S n in scope.Ghost.ghost more;
          D.scoped_term_def n next; let u = () in refine_ u)} in
      deep_lambdas (count - 1) next scope)

let () =
  let body = D.Truth in
  let scope : (((n : D.index) @ immutable ->
    {u : unit | D.scoped_term n body})) Ghost.t =
    {Ghost.ghost = ghost_ (fun n ->
      D.scoped_term_def n body; let u = () in refine_ u)} in
  deep_lambdas 200000 body scope

open Copy_spec
open Hm_effective_execution_spec
module T = Hm_type_proofs
module P = Hm_effective_complete

let run : (e : D.term) @ immutable -> (target : ty) @ immutable ghost ->
    (d : D.typing) @ immutable ghost ->
    {u : unit | D.typed D.Z D.Empty_context e (D.embed target) d} @ ghost -> unit =
  fun e target d premise ->
  ghost_ (let refine_ premise = premise in let z = D.Z in let g = D.Empty_context in
    let t = D.embed target in let u = () in D.depth_def g;
    T.typing_scoped z g e t d (refine_ u); ());
  let input : {e : D.term | D.scoped_term D.Z e} = refine_ e in
  let refine_ out = Hm_routed_infer.closed_hm input in let refine_ input = input in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in
  ghost_ (let refine_ premise = premise in let u = () in
    P.closed_completes out.#execution after out.#pool target d (refine_ u));
  match out.#value with None ->
    ghost_ (let _impossible : {u : unit | false} = refine_ () in ()); assert false
  | Some p ->
    ghost_ (let refine_ premise = premise in let u = () in
      let refine_ tree = Hm_effective_forest.closed_forest out.#execution after out.#pool p (refine_ u) in
      let h = H.empty () in let pool : Generalize_spec.pool = Generalize_spec.Empty in
      let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
      let trees : ((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem h x then Level_finite_spec.finite h t else Level_unifier_spec.observe h x === None)} @ immutable) @ total = fun x ->
        let t = Level_finite_spec.Free x in Level_finite_spec.tree_root_def t;
        Level_unifier_spec.observe_def h x; refine_ t in
      Hm_effective_driver_proofs.run_result h trees 0 pool env out.#execution after out.#pool p (refine_ u);
      let use : ((inferred : D.typing) @ immutable ->
        (delta : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
        {u : unit | D.typed D.Z D.Empty_context (source out.#execution) (D.embed (Level_finite_spec.readback tree)) inferred
          && target === Level_mgu_spec.substitute delta (Level_finite_spec.readback tree)} ->
        {u : unit | true}) @ total = fun _inferred _delta _factor -> let u = () in refine_ u in
      Hm_effective_sound.closed_principal out.#execution after out.#pool p tree target d (refine_ u) true use; ()); ()

let nested (b : ty @ immutable ghost) =
  let z = D.Z in let v = D.Bound z in let rhs = D.Lambda v in
  let app = D.Apply (v, v) in let inner = D.Let (v, app) in let e = D.Let (rhs, inner) in
  let target = ghost_ (Function (b, b)) in
  let d : {d : D.typing | D.typed D.Z D.Empty_context e (D.embed target) d} @ immutable ghost = ghost_ (let a = D.embed b in T.embed_wf z b; D.embed_def target;
    let u = () in let refine_ d = Hm_polymorphic_fixtures.nested_alias_typing a (refine_ u) in refine_ d) in
  let refine_ d = d in
  run e target d (ghost_ (refine_ ()))

let mixed (b : ty @ immutable ghost) =
  let z = D.Z in let one = D.S z in let v = D.Bound z in
  let outer = D.Bound one in let rhs = D.Lambda outer in
  let truth = D.Truth in let app = D.Apply (v, truth) in
  let body = D.Let (rhs, app) in let e = D.Lambda body in
  let target = ghost_ (Function (b, b)) in
  let d : {d : D.typing | D.typed D.Z D.Empty_context e (D.embed target) d} @ immutable ghost = ghost_ (let refine_ d = Hm_polymorphic_fixtures.mixed_typing b in refine_ d) in
  let refine_ d = d in
  run e target d (ghost_ (refine_ ()))

let () =
  nested (ghost_ Boolean);
  nested (ghost_ (Function (Boolean, Boolean)));
  mixed (ghost_ Boolean);
  mixed (ghost_ (Function (Boolean, Boolean)))

let saved_garbage () =
  let z = D.Z in let one = D.S z in let two = D.S one in
  let v = D.Bound z in let identity = D.Lambda v in let truth = D.Truth in
  let discard = D.Lambda truth in let old = D.Apply (discard, identity) in
  let self_app = D.Apply (v, v) in let inner = D.Let (identity, self_app) in
  let e = D.Let (old, inner) in
  ghost_ (D.scoped_term_def z e; D.scoped_term_def z old;
    D.scoped_term_def z discard; D.scoped_term_def one truth;
    D.scoped_term_def z identity; D.scoped_term_def one v; D.present_def one z;
    D.scoped_term_def one inner; D.scoped_term_def one identity;
    D.scoped_term_def two v; D.present_def two z; D.scoped_term_def two self_app; ());
  let input : {e : D.term | D.scoped_term D.Z e} = refine_ e in
  let refine_ out = Hm_routed_infer.closed_hm input in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in
  match out.#value with None -> assert false | Some p ->
    ghost_ (let u = () in
      let refine_ tree = Hm_effective_forest.closed_forest out.#execution after out.#pool p (refine_ u) in
      let h = H.empty () in let pool : Generalize_spec.pool = Generalize_spec.Empty in
      let env : Hm_environment_spec.env = Hm_environment_spec.Empty in
      let trees : ((x : node Pref.t) @ immutable ->
        {t : Level_finite_spec.tree | Level_finite_spec.tree_root t === x &&
          (if H.mem h x then Level_finite_spec.finite h t else Level_unifier_spec.observe h x === None)} @ immutable) @ total = fun x ->
        let t = Level_finite_spec.Free x in Level_finite_spec.tree_root_def t;
        Level_unifier_spec.observe_def h x; refine_ t in
      Hm_effective_driver_proofs.run_result h trees 0 pool env out.#execution after out.#pool p (refine_ u);
      let _typing = Hm_effective_sound.closed_sound out.#execution after out.#pool p tree (refine_ u) in ()); ()

let () = saved_garbage ()

let rec retain_through_lets : (count : int) ->
    (body : {e : D.term | D.scoped_term (D.S D.Z) e}) @ immutable ->
    {e : D.term | D.scoped_term (D.S D.Z) e} @ immutable =
  fun count body ->
    let refine_ body = body in
    if count <= 0 then refine_ body else
      let z = D.Z in let one = D.S z in let two = D.S one in
      let variable = D.Bound z in let next = D.Let (body, variable) in
      ghost_ (D.scoped_term_def one next; D.scoped_term_def two variable;
        D.present_def two z; ());
      let refine_ out = retain_through_lets (count - 1) (refine_ next) in refine_ out

let rec physical_size count = function
  | Generalize_spec.Empty -> count
  | Generalize_spec.Entry (_, rest) -> physical_size (count + 1) rest

let routed_stress lambdas lets =
  let rec build : int -> (body : D.term) @ immutable ->
      (scope : (((n : D.index) @ immutable ->
        {u : unit | D.scoped_term n body})) Ghost.t) @ total -> unit =
    fun count body scope ->
    if count > 0 then (
      let next = D.Lambda body in
      let scope : (((n : D.index) @ immutable ->
        {u : unit | D.scoped_term n next})) Ghost.t =
        {Ghost.ghost = ghost_ (fun n -> let more = D.S n in
          scope.Ghost.ghost more; D.scoped_term_def n next;
          let u = () in refine_ u)} in
      build (count - 1) next scope)
    else (
      let z = D.Z in let one = D.S z in let variable = D.Bound z in
      let call = D.Apply (variable, body) in
      ghost_ (scope.Ghost.ghost one; D.scoped_term_def one variable;
        D.present_def one z; D.scoped_term_def one call);
      let refine_ nested = retain_through_lets lets (refine_ call) in
      let e = D.Lambda nested in ghost_ (D.scoped_term_def z e);
      let input : {e : D.term | D.scoped_term D.Z e} = refine_ e in
      let refine_ input = input in
      let refine_ compiled = Fast_term.compile input in
      let compiled : {e : Fast_term.term | Fast_term.valid e
        && D.scoped_term D.Z (Fast_term.source e)} = refine_ compiled in
      let refine_ out = Hm_routed_infer.closed_compiled compiled in
      assert (Option.is_some out.#value);
      assert (physical_size 0 out.#physical = 2 * lambdas + 5)) in
  let body = D.Truth in
  let scope : (((n : D.index) @ immutable ->
    {u : unit | D.scoped_term n body})) Ghost.t =
    {Ghost.ghost = ghost_ (fun n -> D.scoped_term_def n body; let u = () in refine_ u)} in
  build lambdas body scope

let () = routed_stress 2_000 2_000; routed_stress 64 200_000

module V = Verified_hm

let verified_fixture sample expected =
  let e = expression sample in
  ghost_ (scoped_fixture sample);
  let out = V.infer e in
  assert (Option.is_some out.#root = expected);
  ghost_ (
    match out.#root, out.#inferred_type with
    | Some p, Some ty ->
      let _correct : {u : unit |
        V.Spec.represents (Pref.own out.#ownership) p ty out.#evidence
        && V.Spec.has_type e ty out.#evidence} = () in ()
    | None, None -> ()
    | _ -> let _impossible : {u : unit | false} = () in ());
  ()

let verified_typable :
    (e : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
    (target : Copy_spec.ty Ghost.t) -> (typing : D.typing Ghost.t) ->
    {u : unit | D.typed D.Z D.Empty_context e
      (D.embed target.Ghost.ghost) typing.Ghost.ghost} Ghost.t ->
    {r : bool | r} = fun e target typing premise ->
  let out = V.infer e in
  ghost_ (
    let _premise = premise.Ghost.ghost in
    let claim = not (out.#inferred_type === None) in
    let use : ((delta : (Copy_spec.node Pref.t @ immutable total ->
        Copy_spec.ty @ immutable total)) @ total ->
      {u : unit | match out.#inferred_type with
        | None -> false
        | Some ty -> target.Ghost.ghost === Level_mgu_spec.substitute delta ty} ->
      {u : unit | claim}) @ total =
      fun _delta _factor -> () in
    V.principal e (borrow_ out) target.Ghost.ghost typing.Ghost.ghost ()
      claim use);
  match out.#root with Some _ -> true | None -> false

let () =
  verified_fixture Id_id true;
  verified_fixture Nested true;
  verified_fixture Shared true;
  verified_fixture Recursive_let true;
  verified_fixture Bad_body false;
  verified_fixture Bad_rhs false;
  verified_fixture Monomorphic false
