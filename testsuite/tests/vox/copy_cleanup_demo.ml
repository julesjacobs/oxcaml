(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml copy_cleanup_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Copy_heap_proofs
open Level_spec
open Generalize_spec
open Copy_order_proofs
open Copy_cleanup_spec
open Copy_cleanup_proofs

let run (depth : {n : int | n >= 0}) =
  let refine_ depth = depth in
  let refine_ state = Pref.empty () in
  let generic = {desc = Var; level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc generic state in let a = step.value in let state = step.state in
  let finite = cell Var 0 in let refine_ step = Pref.alloc finite state in
  let boundary = step.value in let state = step.state in
  let inner = {desc = Arrow (a, a); level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc inner state in let pair = step.value in let state = step.state in
  let outer = {desc = Arrow (pair, boundary); level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc outer state in let root = step.value in let state = step.state in
  let alias = {desc = Link root; level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc alias state in let link = step.value in let state = step.state in
  let saved = ghost_ (Pref.own (borrow_ state)) in
  let scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem saved x then source_ok saved x else H.at saved x === None}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; source_ok_def saved x;
    let u = () in refine_ u) in
  let bounds : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem saved x) || not (finite_node saved x) || below saved x depth}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; finite_node_def saved x;
    below_def saved x depth; at_level_def saved x; let u = () in refine_ u) in
  let order : ((x : node Pref.t) @ immutable -> {u : unit | ordered saved x}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0; ordered_def saved x; children_below_def saved desc 0;
    let u = () in refine_ u) in
  let base = Empty in ghost_ (pool_scoped_def saved base);
  let link : {p : node Pref.t | H.mem saved p} = refine_ link in
  let state : {t : node Pref.token | Pref.own t === saved && pool_scoped saved base && depth >= 0} = refine_ state in
  let refine_ out = Ordered_copy.instantiate saved scope base depth bounds order link state in
  let refine_ link = link in
  let result = out.#value in let state = out.#state in
  let h = ghost_ (Pref.own (borrow_ state)) in
  let history = ghost_ out.#history in let epoch = ghost_ out.#epoch in
  let next_order : ((x : node Pref.t) @ immutable -> {u : unit | ordered h x}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = copy_ordered saved depth bounds order epoch history x (refine_ u) in refine_ u) in
  let next_bounds : ((x : node Pref.t) @ immutable ->
      {u : unit | not (H.mem h x) || not (finite_node h x) || below h x depth}) @ total ghost = ghost_ (fun x ->
    let u = () in let refine_ u = copy_bounds saved depth bounds epoch history x (refine_ u) in refine_ u) in
  ghost_ (next_bounds result; next_bounds boundary; next_order result; next_order boundary; below_def h result depth);
  ghost_ (let u = () in touched_distinct saved epoch depth history (refine_ u);
    history_grows saved epoch depth history a (refine_ u);
    history_grows saved epoch depth history link (refine_ u);
    Clean_copy.memo_released saved epoch depth history link (refine_ u));
  let trail = out.#trail in
  let members : ((x : node Pref.t) @ immutable ->
    {u : unit | not (listed trail x) || H.mem h x}) @ total ghost = ghost_ (fun x ->
    let u = () in touched_saved saved epoch depth history x (refine_ u);
    history_grows saved epoch depth history x (refine_ u); refine_ u) in
  let state : {t : node Pref.token | Pref.own t === h} = refine_ state in
  let heap_witness : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (h)} in
  let members_witness : (((x : node Pref.t) @ immutable ->
    {u : unit | not (listed trail x) || H.mem heap_witness.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (refine_ members)} in
  let refine_ state = state in
  let refine_ state = Copy_cleanup.clear heap_witness trail members_witness (refine_ state) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  let framing : ((x : node Pref.t) @ immutable ->
    {u : unit | swept_at h after trail x}) @ total ghost = ghost_ (fun x ->
    let refine_ u = sweep_at h trail members x in refine_ u) in
  let same_model : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | equation h rho x === equation after rho x}) @ total ghost = ghost_ (fun rho x ->
    framing x; let u = () in
    let refine_ u = sweep_model h after trail rho x (refine_ u) in refine_ u) in
  ghost_ (let (rho @ total) (_ : node Pref.t @ immutable) = Boolean in
    same_model rho result; ());
  ghost_ (framing result; swept_at_def h after trail result;
    framing link; swept_at_def h after trail link;
    framing a; swept_at_def h after trail a);
  let state : {t : node Pref.token | H.mem (Pref.own t) link} = refine_ state in
  let refine_ source = Pref.read link (borrow_ state) in let refine_ state = state in
  assert (source.memo = Empty_memo);
  let state : {t : node Pref.token | H.mem (Pref.own t) a} = refine_ state in
  let refine_ source = Pref.read a (borrow_ state) in let refine_ state = state in
  assert (source.memo = Empty_memo);
  let state : {t : node Pref.token | H.mem (Pref.own t) result} = refine_ state in
  let refine_ value = Pref.read result (borrow_ state) in let refine_ state = state in
  assert (value.level = Finite depth);
  (match value.desc with
  | Arrow (_, b) -> let refine_ same = Pref.equal b boundary in assert same
  | _ -> failwith "expected arrow");
  let next_scope : ((x : node Pref.t) @ immutable ->
    {u : unit | if H.mem after x then source_ok after x else H.at after x === None}) @ total ghost = ghost_ (fun x ->
    let u = () in Copy_model_proofs.history_scope saved scope epoch depth history x (refine_ u);
    let refine_ u = sweep_scope h after trail framing x (refine_ u) in refine_ u) in
  let base = Empty in ghost_ (pool_scoped_def after base);
  let state : {t : node Pref.token | Pref.own t === after && pool_scoped after base} = refine_ state in
  let link : {p : node Pref.t | H.mem after p} = refine_ link in
  let depth : {n : int | n >= 0} = refine_ depth in
  let clean : ((x : node Pref.t) @ immutable ->
    {u : unit | match H.at after x with None -> true | Some v -> v.memo === Empty_memo}) @ total ghost = ghost_ (fun x ->
    let desc = Var in cell_def desc 0;
    let refine_ depth = depth in
    let u = () in Clean_copy.clean_result saved epoch depth history x (refine_ u);
    refine_ u) in
  let saved_witness1 : (node Pref.heap) Ghost.t = {Ghost.ghost = ghost_ (after)} in
  let clean_witness2 : (((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved_witness1.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t = {Ghost.ghost = ghost_ (refine_ clean)} in
  let scope_witness3 : (((p : node Pref.t) @ immutable ->
      {u : unit | if H.mem saved_witness1.Ghost.ghost p then source_ok saved_witness1.Ghost.ghost p
        else H.at saved_witness1.Ghost.ghost p === None})) Ghost.t = {Ghost.ghost = ghost_ (refine_ next_scope)} in
  let refine_ state = state in
  let copy_source1 : {p : node Pref.t | H.mem saved_witness1.Ghost.ghost p} =
    let refine_ p = link in refine_ p in
  let refine_ second = Clean_copy.instantiate saved_witness1 clean_witness2 scope_witness3 base depth copy_source1 (refine_ state) in
  let refine_ depth = depth in let refine_ link = link in
  (match second.#pool with Entry (_, Entry (_, Entry (_, Empty))) -> () | _ -> assert false);
  let refine_ same = Pref.equal result second.#value in assert (not same);
  let state = second.#state in
  let d2 = ghost_ second.#history in let e2 = ghost_ second.#epoch in
  let h2 = ghost_ (Pref.own (borrow_ state)) in
  ghost_ (let u = () in Clean_copy.memo_released after e2 depth d2 link (refine_ u);
    Clean_copy.result_at after e2 depth d2 link (refine_ u);
    let raw = heap after e2 depth d2 in let trail = Pooled_spec.touched d2 in
    swept_at_def raw h2 trail link;
    history_grows after e2 depth d2 link (refine_ u));
  let state : {t : node Pref.token | H.mem (Pref.own t) link} = refine_ state in
  let refine_ source = Pref.read link (borrow_ state) in
  assert (source.memo = Empty_memo);
  ()

let () =
  let zero = 0 in let zero : {n : int | n >= 0} = refine_ zero in run zero;
  let three = 3 in let three : {n : int | n >= 0} = refine_ three in run three
