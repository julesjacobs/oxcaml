(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.mli structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.mli copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.mli representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_lower_spec.ml effective_lower_proofs.ml effective_lower_paths.ml effective_lower_tree.ml effective_lower_write.mli effective_lower_write.ml terminal_lower_spec.ml terminal_lower_proofs.ml effective_lower_runtime.mli effective_lower_runtime.ml effective_lower_demo.ml";
 { bytecode; }
 { native; }
*)
open Copy_spec
open Level_spec
open Level_finite_spec
open Effective_lower_spec
module E = Effective_level
module R = Representative_level
module U = Level_unifier_spec
module P = Effective_lower_proofs
module C = Effective_lower_runtime

let[@def] (head @ total) (link : node Pref.t @ immutable) (leaf : node Pref.t @ immutable)
    (root : node Pref.t @ immutable) (pair : node Pref.t @ immutable)
    (x : node Pref.t @ immutable total) : R.representative @ immutable total =
  let refine_ first = Pref.equal x link in let refine_ second = Pref.equal x root in
  if first then {R.root = leaf; path = U.Via (leaf, U.Here)}
  else if second then {R.root = pair; path = U.Via (pair, U.Here)}
  else {R.root = x; path = U.Here}

let run stale =
  let refine_ state = Pref.empty () in
  let leaf_node = cell Var 7 in let refine_ step = Pref.alloc leaf_node state in
  let leaf = step.value in let state = step.state in
  let link_node = {desc = Link leaf; level = stale; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc link_node state in let link = step.value in let state = step.state in
  let pair_node = cell (Arrow (link, link)) 9 in
  let refine_ step = Pref.alloc pair_node state in let pair = step.value in let state = step.state in
  let root_node = cell (Link pair) 1 in
  let refine_ step = Pref.alloc root_node state in let root = step.value in let state = step.state in
  let h : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let heads : E.heads Ghost.t = {Ghost.ghost = ghost_ (head link leaf root pair)} in
  let values : ((x : node Pref.t) @ immutable -> {u : unit |
      H.at h.Ghost.ghost leaf === Some leaf_node && leaf_node.desc === Var && leaf_node.level === Finite 7
      && H.at h.Ghost.ghost link === Some link_node && link_node.desc === Link leaf
      && H.at h.Ghost.ghost pair === Some pair_node && pair_node.desc === Arrow (link, link) && pair_node.level === Finite 9
      && H.at h.Ghost.ghost root === Some root_node && root_node.desc === Link pair && root_node.level === Finite 1
      && (heads.Ghost.ghost x).R.root === (if x === link then leaf else if x === root then pair else x)
      && (heads.Ghost.ghost x).R.path === (if x === link then U.Via (leaf, U.Here) else if x === root then U.Via (pair, U.Here) else U.Here)}) @ total ghost = ghost_ (fun x ->
    let var = Var in cell_def var 7; let arrow = Arrow (link, link) in cell_def arrow 9;
    let indirect = Link pair in cell_def indirect 1;
    head_def link leaf root pair x; let refine_ first = Pref.equal x link in let refine_ second = Pref.equal x root in
    let u = () in refine_ u) in
  let witness : (((x : node Pref.t) @ immutable -> {u : unit |
      E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (fun x ->
    values x; E.valid_head_def h.Ghost.ghost heads.Ghost.ghost x;
    let r = heads.Ghost.ghost x in U.resolves_def h.Ghost.ghost x r.root r.path;
    U.terminal_def h.Ghost.ghost x; U.observe_def h.Ghost.ghost x;
    let here = U.Here in U.resolves_def h.Ghost.ghost leaf leaf here; U.resolves_def h.Ghost.ghost pair pair here;
    U.terminal_def h.Ghost.ghost leaf; U.observe_def h.Ghost.ghost leaf;
    U.terminal_def h.Ghost.ghost pair; U.observe_def h.Ghost.ghost pair;
    let u = () in refine_ u)} in
  let order : (((x : node Pref.t) @ immutable -> {u : unit |
      E.effective_ordered h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (fun x ->
    values x; values link;
    E.effective_ordered_def h.Ghost.ghost heads.Ghost.ghost x;
    E.effective_below_def h.Ghost.ghost heads.Ghost.ghost link 9;
    E.level_def h.Ghost.ghost heads.Ghost.ghost link;
    at_level_def h.Ghost.ghost leaf; let u = () in refine_ u)} in
  let scope : (((x : node Pref.t) @ immutable -> {u : unit |
      not (H.mem h.Ghost.ghost x) || E.effective_scope h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> values x; let var = Var in cell_def var 7; let arrow = Arrow (link, link) in cell_def arrow 9; let indirect = Link pair in cell_def indirect 1; source_ok_def h.Ghost.ghost x; order.Ghost.ghost x;
      let u = () in if H.mem h.Ghost.ghost x then (E.ordered_scope h.Ghost.ghost heads.Ghost.ghost witness.Ghost.ghost x (refine_ u); ()) else (); refine_ u)} in
  let trees : (((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x
      && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else U.observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> values x;
      let a = Free leaf in let b = Alias_tree (link, a) in let c = Branch (pair, b, b) in let d = Alias_tree (root, c) in
      let t = if x === leaf then a else if x === link then b else if x === pair then c else if x === root then d else Free x in
      tree_root_def a; tree_root_def b; tree_root_def c; tree_root_def d; tree_root_def t;
      finite_def h.Ghost.ghost a; finite_def h.Ghost.ghost b; finite_def h.Ghost.ghost c;
      finite_def h.Ghost.ghost d; finite_def h.Ghost.ghost t;
      U.observe_def h.Ghost.ghost leaf; U.observe_def h.Ghost.ghost link;
      U.observe_def h.Ghost.ghost pair; U.observe_def h.Ghost.ghost root; U.observe_def h.Ghost.ghost x;
      refine_ t)} in
  ghost_ (values root; values pair; E.effective_active_def h.Ghost.ghost heads.Ghost.ghost root;
    E.level_def h.Ghost.ghost heads.Ghost.ghost root; at_level_def h.Ghost.ghost pair);
  let bound = 2 in
  let refine_ out = C.lower h heads witness scope order trees bound root (refine_ state) in
  let after = ghost_ (Pref.own (borrow_ out.#state)) in let edits = ghost_ out.#edits in
  ghost_ (let u = () in P.lowering_at h.Ghost.ghost heads.Ghost.ghost bound edits leaf (refine_ u);
    P.lowering_at h.Ghost.ghost heads.Ghost.ghost bound edits link (refine_ u);
    P.lowering_at h.Ghost.ghost heads.Ghost.ghost bound edits pair (refine_ u);
    P.lowering_at h.Ghost.ghost heads.Ghost.ghost bound edits root (refine_ u);
    lower_frame_def h.Ghost.ghost after leaf; lower_frame_def h.Ghost.ghost after link;
    lower_frame_def h.Ghost.ghost after pair; lower_frame_def h.Ghost.ghost after root);
  let state = out.#state in
  let state : {t : Pref.token | H.mem (Pref.own t) leaf} = refine_ state in
  let refine_ a = Pref.read leaf (borrow_ state) in let refine_ state = state in
  let state : {t : Pref.token | H.mem (Pref.own t) link} = refine_ state in
  let refine_ b = Pref.read link (borrow_ state) in let refine_ state = state in
  let state : {t : Pref.token | H.mem (Pref.own t) pair} = refine_ state in
  let refine_ c = Pref.read pair (borrow_ state) in let refine_ state = state in
  let state : {t : Pref.token | H.mem (Pref.own t) root} = refine_ state in
  let refine_ d = Pref.read root (borrow_ state) in
  assert (a.level = Finite 2); assert (c.level = Finite 2);
  assert (b = link_node); assert (d = root_node)

let () = run (Finite 0); run Generic
