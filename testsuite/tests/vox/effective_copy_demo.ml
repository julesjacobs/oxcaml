(* TEST
 has-z3;
 flags = "-extension refinement_types";
 source_directories = "${test_source_directory}/../../../verification/library";
 all_modules = "pref.mli pref.ml copy_spec.ml copy_heap_proofs.ml copy_model_proofs.ml copy_complete_proofs.ml copy_sound_proofs.ml copy_template_proofs.ml copy_algorithm.ml level_spec.ml lower_locality_spec.ml level_proofs.ml lower_locality_proofs.ml level_lower.ml level_unifier_spec.ml marked_occurs_proofs.ml level_unifier_proofs.ml level_unifier_metadata.ml marked_occurs.ml level_unifier.ml level_copy_proofs.ml generalize_spec.ml generalize_proofs.ml generalize_scheme_proofs.ml generalize.ml level_finite_spec.ml level_finite_proofs.ml level_mgu_spec.ml level_mgu_proofs.ml forest_transport.ml pooled_spec.ml pooled_proofs.ml pooled_allocation_proofs.ml pooled_allocator.ml pooled_copy.ml provenance_spec.ml provenance_proofs.ml relative_generalization.ml compression_model_proofs.ml compression_finite_proofs.ml hm_declarative.ml hm_environment_spec.ml compression_spec.ml compression_path_proofs.ml compressed_representative.ml compression_proofs.ml leaf_provenance_spec.ml leaf_provenance_proofs.ml structure_finite_proofs.ml structure_model_proofs.ml structure_spec.ml structure_link.ml structure_origin_proofs.ml compression_origin_proofs.ml optimized_unifier_spec.ml optimized_metadata.ml optimized_finite_proofs.ml optimized_model_proofs.ml optimized_origin_proofs.ml optimized_link_proofs.ml optimized_link.ml pruned_lower_proofs.ml pruned_lower.ml pruned_bind.ml optimized_unifier.ml copy_order_proofs.ml ordered_copy.ml copy_cleanup_spec.ml copy_cleanup_proofs.ml copy_cleanup.ml clean_pooled_copy.ml clean_copy.ml nested_pool_spec.ml nested_pool_proofs.ml nested_pool.ml representative_level.ml representative_pool_spec.ml representative_pool_proofs.ml representative_pool.ml effective_level.ml effective_template.ml representative_mutation.ml effective_copy_spec.ml effective_copy_heap_proofs.ml effective_copy_metadata.ml effective_copy_complete.ml effective_copy_sound.ml effective_copy_template.ml effective_copy_finite.ml effective_copy_order.ml effective_copy_origin.ml effective_copy_pool.ml effective_copy_runtime.ml effective_copy_demo.ml";
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

let run generic =
  let refine_ state = Pref.empty () in
  let level = if generic then Generic else Finite 0 in
  let leaf = {desc = Var; level; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc leaf state in let q = step.value in let state = step.state in
  let alias = {desc = Link q; level = Finite 7; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc alias state in let p = step.value in let state = step.state in
  let pair = {desc = Arrow (p, p); level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc pair state in let root = step.value in let state = step.state in
  let saved = ghost_ (Pref.own (borrow_ state)) in
  let base = Empty in let depth = 1 in
  let c : C.context = {saved = ghost_ saved; epoch = ghost_ root; depth = ghost_ depth; base = ghost_ base} in
  let heads : E.heads Ghost.t = {Ghost.ghost = ghost_ (head p q)} in
  let scope : (((x : node Pref.t) @ immutable -> {u : unit |
      not (H.mem c.saved x) || source_ok c.saved x})) Ghost.t = {Ghost.ghost = ghost_ (fun x ->
    source_ok_def c.saved x; let u = () in refine_ u)} in
  let clean : (((x : node Pref.t) @ immutable -> {u : unit |
      match H.at c.saved x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in refine_ u)} in
  let witness : (((x : node Pref.t) @ immutable -> {u : unit |
      E.valid_head c.saved heads.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (fun x ->
    E.valid_head_def c.saved heads.Ghost.ghost x;
    head_def p q x; let refine_ same = Pref.equal x p in
    let r = heads.Ghost.ghost x in U.resolves_def c.saved x r.root r.path;
    U.terminal_def c.saved x; U.observe_def c.saved x;
    let here = U.Here in U.resolves_def c.saved q q here;
    U.terminal_def c.saved q; U.observe_def c.saved q;
    let u = () in refine_ u)} in
  let state : {t : node Pref.token | Pref.own t === c.saved && H.mem c.saved root} = refine_ state in
  let refine_ state = state in

  let refine_ out = C.instantiate c heads scope clean witness (refine_ depth) base root (refine_ state) in
  let result = out.#value in let state = out.#state in let d = ghost_ out.#history in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ (let u = () in target_allocated c.saved heads.Ghost.ghost root depth d root result (refine_ u);
    M.result_at c.saved heads.Ghost.ghost root depth d result (refine_ u);
    let raw = heap c.saved root depth d in let trail = Pooled_spec.touched d in
    swept_at_def raw after trail result; ());
  let state : {t : node Pref.token | H.mem (Pref.own t) result} = refine_ state in
  let refine_ copied = Pref.read result (borrow_ state) in let refine_ state = state in
  assert (copied.level = Finite 1);
  (match copied.desc with
   | Arrow (a, b) -> let refine_ same = Pref.equal a b in assert same; let refine_ same = Pref.equal a p in assert (same = not generic)
   | _ -> failwith "expected shared arrow");
  (match out.#pool with
   | Entry (_, Empty) -> assert (not generic)
   | Entry (_, Entry (_, Empty)) -> assert generic
   | _ -> failwith "unexpected allocation count");
  ghost_ (let u = () in M.saved_observe c.saved heads.Ghost.ghost root depth d p (refine_ u);
    M.saved_observe c.saved heads.Ghost.ghost root depth d q (refine_ u));
  let state : {t : node Pref.token | H.mem (Pref.own t) p} = refine_ state in
  let refine_ original = Pref.read p (borrow_ state) in let refine_ state = state in
  assert (original.level = Finite 7); assert (original.desc = Link q);
  assert (original.memo = Empty_memo);
  let state : {t : node Pref.token | H.mem (Pref.own t) q} = refine_ state in
  let refine_ original = Pref.read q (borrow_ state) in let refine_ state = state in
  assert (original.memo = Empty_memo);
  let c2 = {C.saved = ghost_ after; epoch = ghost_ root; depth = ghost_ depth; base = ghost_ base} in
  let next_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ (fun x ->
    if H.mem c.saved x then heads.Ghost.ghost x else {R.root = x; path = U.Here})} in
  let next_witness : (((x : node Pref.t) @ immutable -> {u : unit |
      E.valid_head c2.saved next_heads.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (fun x ->
    let u = () in M.result_head c.saved heads.Ghost.ghost next_heads.Ghost.ghost witness.Ghost.ghost root depth d x (refine_ u);
    refine_ u)} in
  ghost_ (
    let raw = heap c.saved root depth d in let trail = Pooled_spec.touched d in
    let full_scope : ((x : node Pref.t) @ immutable -> {u : unit |
        if H.mem c.saved x then source_ok c.saved x else H.at c.saved x === None}) @ total = fun x ->
      scope.Ghost.ghost x; let u = () in refine_ u in
    let raw_witness : ((x : node Pref.t) @ immutable -> {u : unit |
        E.valid_head raw next_heads.Ghost.ghost x}) @ total = fun x -> let u = () in
      M.history_head c.saved heads.Ghost.ghost next_heads.Ghost.ghost witness.Ghost.ghost root depth d x (refine_ u); refine_ u in
    let framing : ((x : node Pref.t) @ immutable -> {u : unit | swept_at raw c2.saved trail x}) @ total = fun x ->
      let u = () in M.result_at c.saved heads.Ghost.ghost root depth d x (refine_ u); refine_ u in
    let bounds : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem c.saved x)
        || E.level c.saved heads.Ghost.ghost x === Generic || E.effective_below c.saved heads.Ghost.ghost x depth}) @ total = fun x ->
      E.level_def c.saved heads.Ghost.ghost x; E.effective_below_def c.saved heads.Ghost.ghost x depth;
      head_def p q x; let refine_ same = Pref.equal x p in
      let r = heads.Ghost.ghost x in Level_spec.at_level_def c.saved r.root;
      let u = () in refine_ u in
    let order : ((x : node Pref.t) @ immutable -> {u : unit | E.effective_ordered c.saved heads.Ghost.ghost x}) @ total = fun x ->
      E.effective_ordered_def c.saved heads.Ghost.ghost x; let u = () in refine_ u in
    let u = () in
    Effective_copy_order.copy_ordered c.saved full_scope heads.Ghost.ghost next_heads.Ghost.ghost witness.Ghost.ghost root depth bounds order d (refine_ raw_witness) result (refine_ u);
    Effective_copy_order.sweep_ordered raw c2.saved next_heads.Ghost.ghost trail framing result;
    ());
  let next_scope : (((x : node Pref.t) @ immutable -> {u : unit |
      not (H.mem c2.saved x) || source_ok c2.saved x})) Ghost.t = {Ghost.ghost = ghost_ (fun x ->
    let full_scope : ((y : node Pref.t) @ immutable -> {u : unit |
        if H.mem c.saved y then source_ok c.saved y else H.at c.saved y === None}) @ total = fun y ->
      scope.Ghost.ghost y; let u = () in refine_ u in
    let raw = heap c.saved root depth d in let trail = Pooled_spec.touched d in
    let framing : ((y : node Pref.t) @ immutable -> {u : unit | swept_at raw c2.saved trail y}) @ total = fun y ->
      let u = () in M.result_at c.saved heads.Ghost.ghost root depth d y (refine_ u); refine_ u in
    let u = () in M.history_scope c.saved heads.Ghost.ghost full_scope root depth d x (refine_ u);
    Copy_cleanup_proofs.sweep_scope raw c2.saved trail framing x (refine_ u); refine_ u)} in
  let next_clean : (((x : node Pref.t) @ immutable -> {u : unit |
      match H.at c2.saved x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> clean.Ghost.ghost x; let u = () in
      M.clean_result c.saved heads.Ghost.ghost root depth d x (refine_ u); refine_ u)} in
  ghost_ (let u = () in M.saved_observe c.saved heads.Ghost.ghost root depth d root (refine_ u));
  let state : {t : node Pref.token | Pref.own t === c2.saved && H.mem c2.saved root} = refine_ state in
  let refine_ state = state in

  let refine_ second = C.instantiate c2 next_heads next_scope next_clean next_witness (refine_ depth) base root (refine_ state) in
  let refine_ same = Pref.equal result second.#value in assert (not same);
  ()

let () = run false; run true

let[@def] (here @ total) (x : node Pref.t @ immutable total) : R.representative @ immutable total =
  {R.root = x; path = U.Here}

let rec pool_size n = function Empty -> n | Entry (_, rest) -> pool_size (n + 1) rest

let rec deep : int -> int -> (h : node Pref.heap Ghost.t) @ immutable ->
    (scope : (((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem h.Ghost.ghost x then source_ok h.Ghost.ghost x else H.at h.Ghost.ghost x === None})) Ghost.t) @ total ->
    (clean : (((x : node Pref.t) @ immutable -> {u : unit |
      match H.at h.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t) @ total ->
    (terminal : (((x : node Pref.t) @ immutable -> {u : unit |
      not (H.mem h.Ghost.ghost x) || U.terminal h.Ghost.ghost x})) Ghost.t) @ total ->
    (p : node Pref.t) @ immutable ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && H.mem h.Ghost.ghost p}) @ unique -> unit =
  fun count expected h scope clean terminal p state ->
  let refine_ state = state in
  if count = 0 then (
    let depth = 1 in let base = Empty in
    let c : C.context = {saved = h.Ghost.ghost; epoch = ghost_ p; depth = ghost_ depth; base = ghost_ base} in
    let heads : E.heads Ghost.t = {Ghost.ghost = ghost_ here} in
    let scope : (((x : node Pref.t) @ immutable -> {u : unit |
        not (H.mem c.saved x) || source_ok c.saved x})) Ghost.t = {Ghost.ghost = ghost_ (fun x ->
      scope.Ghost.ghost x; let u = () in refine_ u)} in
    let clean : (((x : node Pref.t) @ immutable -> {u : unit |
        match H.at c.saved x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t =
      {Ghost.ghost = ghost_ (refine_ clean.Ghost.ghost)} in
    let witness : (((x : node Pref.t) @ immutable -> {u : unit |
        E.valid_head c.saved heads.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (fun x ->
      terminal.Ghost.ghost x; E.valid_head_def c.saved heads.Ghost.ghost x; here_def x;
      let path = U.Here in U.resolves_def c.saved x x path; let u = () in refine_ u)} in
    let refine_ out = C.instantiate c heads scope clean witness (refine_ depth) base p (refine_ state) in
    assert (pool_size 0 out.#pool = expected))
  else (
    let desc = Arrow (p, p) in
    let node = {desc; level = Generic; memo = Empty_memo; visited = false} in
    let refine_ step = Pref.alloc node state in let q = step.value in let state = step.state in
    let after : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
    let next_scope : (((x : node Pref.t) @ immutable -> {u : unit |
        if H.mem after.Ghost.ghost x then source_ok after.Ghost.ghost x else H.at after.Ghost.ghost x === None})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x ->
        payload_scoped_def h.Ghost.ghost node; let u = () in
        Copy_model_proofs.put_scope h.Ghost.ghost scope.Ghost.ghost q node x (refine_ u); refine_ u)} in
    let next_clean : (((x : node Pref.t) @ immutable -> {u : unit |
        match H.at after.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> clean.Ghost.ghost x;
        Copy_heap_proofs.put_frame h.Ghost.ghost q node x; let u = () in refine_ u)} in
    let next_terminal : (((x : node Pref.t) @ immutable -> {u : unit |
        not (H.mem after.Ghost.ghost x) || U.terminal after.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> terminal.Ghost.ghost x;
        Copy_heap_proofs.put_frame h.Ghost.ghost q node x;
        U.terminal_def h.Ghost.ghost x; U.terminal_def after.Ghost.ghost x;
        U.observe_def h.Ghost.ghost x; U.observe_def after.Ghost.ghost x; let u = () in refine_ u)} in
    deep (count - 1) expected after next_scope next_clean next_terminal q (refine_ state))

let () =
  let refine_ state = Pref.empty () in
  let leaf = {desc = Var; level = Generic; memo = Empty_memo; visited = false} in
  let refine_ step = Pref.alloc leaf state in let p = step.value in let state = step.state in
  let h : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let scope : (((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem h.Ghost.ghost x then source_ok h.Ghost.ghost x else H.at h.Ghost.ghost x === None})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> source_ok_def h.Ghost.ghost x; let u = () in refine_ u)} in
  let clean : (((x : node Pref.t) @ immutable -> {u : unit |
      match H.at h.Ghost.ghost x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in refine_ u)} in
  let terminal : (((x : node Pref.t) @ immutable -> {u : unit |
      not (H.mem h.Ghost.ghost x) || U.terminal h.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> U.terminal_def h.Ghost.ghost x; U.observe_def h.Ghost.ghost x; let u = () in refine_ u)} in
  deep 200000 200001 h scope clean terminal p (refine_ state)
