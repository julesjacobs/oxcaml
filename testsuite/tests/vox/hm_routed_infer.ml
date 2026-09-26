open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_finite_spec
open Generalize_spec
open Hm_environment_spec
open Hm_runtime_spec
open Hm_effective_execution_spec
open Hm_effective_runtime
module D = Hm_declarative
module F = Fast_environment
module T = Fast_term
module E = Effective_level
module R = Representative_level
module G = Hm_effective_forest
module A = Hm_effective_allocation
module Dp = Hm_effective_driver_proofs
module S = Level_pool_store
module C = Hm_routed_context
module B = Borrow_iarray.Owned_array
module K = Hm_pool_capacity
module Rp = Level_pool_routing
module Annotation = Hm_annotation_trace

external[@layout_poly] raise_any : ('a : any).
  exn -> 'a @ portable unique = "%raise"

type inference = #{value : node Pref.t option @@ aliased; state : node Pref.token;
  trace : Hm_annotation_trace.trace @@ aliased; pool : pool @@ ghost; execution : execution @@ ghost;
  physical : pool @@ aliased; pools : pool B.t; routing : C.context @@ ghost}

type goal = {heap : node Pref.heap @@ ghost; depth : int @@ ghost; pool : pool @@ ghost;
  env : env @@ ghost; term : D.term @@ ghost; origin : S.store @@ ghost}

let[@def] (completed @ total) (goal : goal @ immutable)
    (after : node Pref.heap @ immutable) (pool : pool @ immutable)
    (execution : execution @ immutable) (value : node Pref.t option @ immutable)
    (routing : C.context @ immutable) (physical : pool @ immutable)
    (buckets : pool iarray @ immutable) = ghost_ (
  ran goal.heap goal.depth goal.pool goal.env execution after pool
    && source execution === goal.term && value === result execution
    && C.recorded after routing && routing.origin === goal.origin
    && routing.store.S.pending === physical
    && routing.store.S.buckets === buckets
    && (value === None || routing.store.S.depth = goal.depth))

let rec work : (collect_trace : bool) -> (goal : goal) @ immutable -> (h : node Pref.heap Ghost.t) @ immutable ->
    (heads : E.heads Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (depth : int) -> (pool_spec : pool Ghost.t) @ immutable ->
    (facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost depth pool_spec.Ghost.ghost x})) Ghost.t) @ total ->
    (runtime_env : F.forest) @ immutable -> (runtime_term : T.term) @ immutable ->
    (physical : pool) @ immutable -> (pools : pool B.t) @ unique ->
    (routing : C.context) @ immutable ghost ->
    (state : {t : node Pref.token | Pref.own t === h.Ghost.ghost && depth >= 0 && pool_scoped h.Ghost.ghost pool_spec.Ghost.ghost
      && C.recorded h.Ghost.ghost routing && routing.origin === goal.origin
      && routing.store.S.depth = depth && routing.store.S.pending === physical
      && routing.store.S.buckets === B.contents pools
      && K.fits runtime_term depth (Iarray.length goal.origin.S.buckets)
      && F.valid_forest runtime_env && T.valid runtime_term && env_owned h.Ghost.ghost (F.flatten runtime_env)
      && D.scoped_term (env_depth (F.flatten runtime_env)) (T.source runtime_term)}) @ unique ->
    (use : ((r : {r : inference | ran h.Ghost.ghost depth pool_spec.Ghost.ghost (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_term && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique)) ->
    {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun collect_trace goal h heads trees depth pool_spec facts runtime_env runtime_term physical pools routing state use ->
  let pool = ghost_ pool_spec.Ghost.ghost in
  let facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost depth pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; ())} in
  ghost_ (let limit = Iarray.length goal.origin.S.buckets in
    K.fits_def runtime_term depth limit;
    C.capacity h.Ghost.ghost routing ();
    C.scoped_runtime h.Ghost.ghost routing heads.Ghost.ghost depth pool facts.Ghost.ghost (); ());
  let env = ghost_ (F.flatten runtime_env) in let term = ghost_ (T.source runtime_term) in
  ghost_ (T.valid_def runtime_term; T.source_def runtime_term; let n = env_depth env in D.scoped_term_def n term; ());
  let valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; runtime_at_def h.Ghost.ghost heads.Ghost.ghost depth pool x;
      safe_def h.Ghost.ghost heads.Ghost.ghost x; ())} in
  match runtime_term with
  | T.Bound index ->
    let i = ghost_ index.T.original in
    ghost_ (Dp.env_lookup_owned h.Ghost.ghost env i ();
      F.lookup_encoded env i index.T.number; ());
    let premise : ({u : unit | F.valid_forest runtime_env && index.T.number >= 0}) Ghost.t = {Ghost.ghost = ghost_ (refine_ ())} in
    let refine_ found = F.lookup runtime_env index.T.number premise in
    let p : {p : node Pref.t | lookup env i === Some p && H.mem h.Ghost.ghost p} = match found with
      | Some p -> refine_ p | None -> ghost_ (let _ : {u : unit | false} = refine_ () in ()); assert false in
    let c = {Effective_copy_spec.saved = ghost_ h.Ghost.ghost; epoch = ghost_ p;
      depth = ghost_ depth; base = ghost_ physical} in
    let scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem c.Effective_copy_spec.saved x) || source_ok c.Effective_copy_spec.saved x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; runtime_at_def h.Ghost.ghost heads.Ghost.ghost depth pool x;
        safe_def h.Ghost.ghost heads.Ghost.ghost x; ())} in
    let clean : (((x : node Pref.t) @ immutable -> {u : unit | match H.at c.Effective_copy_spec.saved x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; runtime_at_def h.Ghost.ghost heads.Ghost.ghost depth pool x;
        safe_def h.Ghost.ghost heads.Ghost.ghost x; ())} in
    let valid_copy : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head c.Effective_copy_spec.saved heads.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ valid.Ghost.ghost)} in
    let refine_ copied = Certified_copy.instantiate c heads scope clean valid_copy (refine_ depth) physical p (refine_ state) in
    let copied_virtual = ghost_ (Pooled_spec.registered pool copied.#epoch copied.#history) in
    ghost_ (Copy_certificate_spec.certifies_def h.Ghost.ghost copied.#certificate copied.#epoch depth copied.#history p copied.#value);
    let refine_ routing = ghost_ (C.copied h.Ghost.ghost routing copied.#epoch copied.#history copied.#certificate ()) in
    let physical = copied.#pool in
  let execution = ghost_ (RVar (i, copied.#value, copied.#epoch, copied.#history, copied.#certificate)) in
  let after = ghost_ (Pref.own (borrow_ copied.#state)) in
  ghost_ (copy_heap_def h.Ghost.ghost copied.#epoch depth copied.#history; ran_def h.Ghost.ghost depth pool env execution after copied_virtual;
    source_def execution; result_def execution);
  let trace = if collect_trace then Annotation.Variable_use copied.#value else Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = Some copied.#value; state = copied.#state; pool = copied_virtual; trace; execution; physical; pools; routing} in use (refine_ out)
  | T.Truth ->
    let desc = Bool in
    ghost_ (cell_def desc depth; let v = cell desc depth in payload_scoped_def h.Ghost.ghost v; ());
  ghost_ (C.scoped_runtime h.Ghost.ghost routing heads.Ghost.ghost depth pool facts.Ghost.ghost (); ());
  let refine_ allocated = Effective_allocator.allocate h depth desc physical (refine_ state) in
  let allocated_virtual = ghost_ (Entry (allocated.#value, pool)) in
  ghost_ (allocated_def h.Ghost.ghost depth allocated.#value desc);
  ghost_ (C.allocation_pool h.Ghost.ghost depth allocated.#value desc pool ());
  let refine_ routing = ghost_ (C.allocated h.Ghost.ghost routing allocated.#value desc ()) in
  let physical = allocated.#pool in
  ghost_ (allocated_def h.Ghost.ghost depth allocated.#value desc);
  let execution = ghost_ (RBool allocated.#value) in
  let after = ghost_ (Pref.own (borrow_ allocated.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after allocated_virtual;
    source_def execution; result_def execution);
  let trace = if collect_trace then Annotation.Boolean_literal allocated.#value else Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = Some allocated.#value; state = allocated.#state; pool = allocated_virtual; trace; execution; physical; pools; routing} in use (refine_ out)
  | T.False ->
    let desc = Bool in
    ghost_ (cell_def desc depth; let v = cell desc depth in payload_scoped_def h.Ghost.ghost v; ());
  ghost_ (C.scoped_runtime h.Ghost.ghost routing heads.Ghost.ghost depth pool facts.Ghost.ghost (); ());
  let refine_ allocated = Effective_allocator.allocate h depth desc physical (refine_ state) in
  let allocated_virtual = ghost_ (Entry (allocated.#value, pool)) in
  ghost_ (allocated_def h.Ghost.ghost depth allocated.#value desc);
  ghost_ (C.allocation_pool h.Ghost.ghost depth allocated.#value desc pool ());
  let refine_ routing = ghost_ (C.allocated h.Ghost.ghost routing allocated.#value desc ()) in
  let physical = allocated.#pool in
  ghost_ (allocated_def h.Ghost.ghost depth allocated.#value desc);
  let execution = ghost_ (RFalse allocated.#value) in
  let after = ghost_ (Pref.own (borrow_ allocated.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after allocated_virtual;
    source_def execution; result_def execution);
  let trace = if collect_trace then Annotation.False_literal allocated.#value else Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = Some allocated.#value; state = allocated.#state; pool = allocated_virtual; trace; execution; physical; pools; routing} in use (refine_ out)
  | T.Word word ->
    let desc = Word in
    ghost_ (cell_def desc depth; let v = cell desc depth in payload_scoped_def h.Ghost.ghost v; ());
  ghost_ (C.scoped_runtime h.Ghost.ghost routing heads.Ghost.ghost depth pool facts.Ghost.ghost (); ());
  let refine_ allocated = Effective_allocator.allocate h depth desc physical (refine_ state) in
  let allocated_virtual = ghost_ (Entry (allocated.#value, pool)) in
  ghost_ (allocated_def h.Ghost.ghost depth allocated.#value desc);
  ghost_ (C.allocation_pool h.Ghost.ghost depth allocated.#value desc pool ());
  let refine_ routing = ghost_ (C.allocated h.Ghost.ghost routing allocated.#value desc ()) in
  let physical = allocated.#pool in
  ghost_ (allocated_def h.Ghost.ghost depth allocated.#value desc);
  let execution = ghost_ (RWord (word, allocated.#value)) in
  let after = ghost_ (Pref.own (borrow_ allocated.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after allocated_virtual;
    source_def execution; result_def execution);
  let trace = if collect_trace then Annotation.Word_literal (word, allocated.#value) else Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = Some allocated.#value; state = allocated.#state; pool = allocated_virtual; trace; execution; physical; pools; routing} in use (refine_ out)
  | T.Nil ->
    let var = Var in
    ghost_ (cell_def var depth; payload_scoped_def h.Ghost.ghost (cell var depth);
      C.scoped_runtime h.Ghost.ghost routing heads.Ghost.ghost depth pool facts.Ghost.ghost ());
    let refine_ element = Effective_allocator.allocate h depth var physical (refine_ state) in
    let arg = element.#value in
    let middle : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ element.#state))} in
    let pool1 = ghost_ (Entry (arg, pool)) in
    ghost_ (allocated_def h.Ghost.ghost depth arg var;
      C.allocation_pool h.Ghost.ghost depth arg var pool ());
    let refine_ routing = ghost_ (C.allocated h.Ghost.ghost routing arg var ()) in
    let desc = List arg in
    ghost_ (cell_def desc depth; payload_scoped_def middle.Ghost.ghost (cell desc depth));
    let refine_ allocated = Effective_allocator.allocate middle depth desc element.#pool (refine_ element.#state) in
    let final_pool = ghost_ (Entry (allocated.#value, pool1)) in
    ghost_ (allocated_def middle.Ghost.ghost depth allocated.#value desc;
      C.allocation_pool middle.Ghost.ghost depth allocated.#value desc pool1 ());
    let refine_ routing = ghost_ (C.allocated middle.Ghost.ghost routing allocated.#value desc ()) in
    let execution = ghost_ (RNil (arg, allocated.#value)) in
    let after = ghost_ (Pref.own (borrow_ allocated.#state)) in
    ghost_ (ran_def h.Ghost.ghost depth pool env execution after final_pool;
      source_def execution; result_def execution);
    let trace = if collect_trace then Annotation.Empty_list_literal allocated.#value else Annotation.Failed in
    ghost_ (Hm_annotation_trace_spec.records_def trace execution);
    let out = #{value = Some allocated.#value; state = allocated.#state; pool = final_pool;
      trace; execution; physical = allocated.#pool; pools; routing} in use (refine_ out)
  | T.Lambda runtime_body ->
    let var = Var in
  ghost_ (cell_def var depth; let v = cell var depth in payload_scoped_def h.Ghost.ghost v; ());
  ghost_ (C.scoped_runtime h.Ghost.ghost routing heads.Ghost.ghost depth pool facts.Ghost.ghost (); ());
  let refine_ arg_allocation = Effective_allocator.allocate h depth var physical (refine_ state) in
  let arg_allocation_virtual = ghost_ (Entry (arg_allocation.#value, pool)) in
  ghost_ (allocated_def h.Ghost.ghost depth arg_allocation.#value var);
  ghost_ (C.allocation_pool h.Ghost.ghost depth arg_allocation.#value var pool ());
  let refine_ routing = ghost_ (C.allocated h.Ghost.ghost routing arg_allocation.#value var ()) in
  let physical = arg_allocation.#pool in
  ghost_ (allocated_def h.Ghost.ghost depth arg_allocation.#value var);
  let argument = arg_allocation.#value in let arg_pool = arg_allocation_virtual in
  let state = arg_allocation.#state in
  let arg_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let arg_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem arg_heap.Ghost.ghost x then finite arg_heap.Ghost.ghost t else observe arg_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let next = G.allocated_forest h.Ghost.ghost trees.Ghost.ghost depth argument var () in
      let refine_ t = next x in refine_ t)} in
  let[@def] arg_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select arg_heap.Ghost.ghost arg_trees.Ghost.ghost x in r) in
  let arg_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ arg_heads_selected} in
  let arg_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head arg_heap.Ghost.ghost arg_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> arg_heads_selected_def x;
      let refine_ r = Forest_heads.select arg_heap.Ghost.ghost arg_trees.Ghost.ghost x in
      E.valid_head_def arg_heap.Ghost.ghost arg_heads.Ghost.ghost x; ())} in
  let arg_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at arg_heap.Ghost.ghost arg_heads.Ghost.ghost depth arg_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x;
      allocated_def h.Ghost.ghost depth argument var;
      A.allocate_runtime h.Ghost.ghost heads.Ghost.ghost arg_heads.Ghost.ghost depth pool argument var valid.Ghost.ghost (refine_ arg_valid.Ghost.ghost) x (); ())} in
  let env_input : {f : F.forest | F.valid_forest f} = refine_ runtime_env in
  let refine_ next_runtime_env = F.cons argument env_input in
  let next_env = ghost_ (Bind (argument, env)) in
  ghost_ (let v = cell var depth in Dp.allocation_env h.Ghost.ghost argument v env ();
    Copy_heap_proofs.put_frame h.Ghost.ghost argument v argument;
    env_owned_def arg_heap.Ghost.ghost next_env; env_depth_def next_env);
  let call_pool_0 : pool Ghost.t = {Ghost.ghost = ghost_ arg_pool} in
  let resume : (answer : {r : inference | ran arg_heap.Ghost.ghost depth call_pool_0.Ghost.ghost (F.flatten next_runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_body && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun answer ->
    let physical = answer.#physical in let pools = answer.#pools in
    let routing = ghost_ answer.#routing in
  let body_trace = answer.#trace in
  let body_run = ghost_ answer.#execution in let body_pool = answer.#pool in let state = answer.#state in
  let middle : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RLam (argument, body_run, middle.Ghost.ghost, body_pool, None)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after body_pool;
    source_def execution; result_def execution);
  let trace = Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = None; state = state; pool = body_pool; trace; execution; physical; pools; routing} in use (refine_ out)
  | Some target ->
  let body_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem middle.Ghost.ghost x then finite middle.Ghost.ghost t else observe middle.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let refine_ t = G.run_forest arg_heap.Ghost.ghost arg_trees.Ghost.ghost depth arg_pool next_env body_run middle.Ghost.ghost body_pool x () in refine_ t)} in
  let[@def] body_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select middle.Ghost.ghost body_trees.Ghost.ghost x in r) in
  let body_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ body_heads_selected} in
  let body_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle.Ghost.ghost body_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> body_heads_selected_def x;
      let refine_ r = Forest_heads.select middle.Ghost.ghost body_trees.Ghost.ghost x in
      E.valid_head_def middle.Ghost.ghost body_heads.Ghost.ghost x; ())} in
  let body_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle.Ghost.ghost body_heads.Ghost.ghost depth body_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> Hm_effective_invariant.run_invariant arg_heap.Ghost.ghost arg_heads.Ghost.ghost arg_trees.Ghost.ghost depth arg_pool arg_facts.Ghost.ghost next_env body_run middle.Ghost.ghost body_heads.Ghost.ghost body_valid.Ghost.ghost body_pool x (); ())} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe middle.Ghost.ghost body_heads.Ghost.ghost x}) @ total = fun x ->
      body_facts.Ghost.ghost x; runtime_at_def middle.Ghost.ghost body_heads.Ghost.ghost depth body_pool x; () in
    Dp.run_pool_scoped arg_heap.Ghost.ghost depth arg_pool next_env body_run middle.Ghost.ghost body_heads.Ghost.ghost safe body_pool ();
    Dp.run_env_owned arg_heap.Ghost.ghost depth arg_pool next_env body_run middle.Ghost.ghost body_pool (); ());
  ghost_ (let v = cell var depth in Copy_heap_proofs.put_frame h.Ghost.ghost argument v argument;
    Hm_effective_membership.run_extends arg_heap.Ghost.ghost depth arg_pool next_env body_run middle.Ghost.ghost body_pool argument ();
    Dp.run_result arg_heap.Ghost.ghost arg_trees.Ghost.ghost depth arg_pool next_env body_run middle.Ghost.ghost body_pool target (); ());
  let desc = Arrow (argument, target) in
  ghost_ (cell_def desc depth; let v = cell desc depth in payload_scoped_def middle.Ghost.ghost v; ());
  ghost_ (C.scoped_runtime middle.Ghost.ghost routing body_heads.Ghost.ghost depth body_pool body_facts.Ghost.ghost (); ());
  let refine_ allocated = Effective_allocator.allocate middle depth desc physical (refine_ state) in
  let allocated_virtual = ghost_ (Entry (allocated.#value, body_pool)) in
  ghost_ (allocated_def middle.Ghost.ghost depth allocated.#value desc);
  ghost_ (C.allocation_pool middle.Ghost.ghost depth allocated.#value desc body_pool ());
  let refine_ routing = ghost_ (C.allocated middle.Ghost.ghost routing allocated.#value desc ()) in
  let physical = allocated.#pool in
  ghost_ (allocated_def middle.Ghost.ghost depth allocated.#value desc);
  let execution = ghost_ (RLam (argument, body_run, middle.Ghost.ghost, body_pool, Some allocated.#value)) in
  let after = ghost_ (Pref.own (borrow_ allocated.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after allocated_virtual;
    source_def execution; result_def execution);
  let trace = if collect_trace then Annotation.Abstraction (allocated.#value, argument, body_trace) else Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = Some allocated.#value; state = allocated.#state; pool = allocated_virtual; trace; execution; physical; pools; routing} in use (refine_ out)
  in
  let call_facts_0 : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at arg_heap.Ghost.ghost arg_heads.Ghost.ghost depth call_pool_0.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> arg_facts.Ghost.ghost x; ())} in
  work collect_trace goal arg_heap arg_heads arg_trees depth call_pool_0 call_facts_0 next_runtime_env runtime_body physical pools routing (refine_ state) resume
  | T.CaseList (runtime_scrutinee, runtime_empty, runtime_nonempty) ->
    let case_node_6 = T.bound 2 in
    ghost_ (T.decode_def 2; T.decode_def 1; T.decode_def 0; D.present_def (D.S (D.S (D.S (D.S (env_depth env))))) (D.S (D.S (D.Z))); D.present_def (D.S (D.S (D.S (env_depth env)))) (D.S (D.Z)); D.present_def (D.S (D.S (env_depth env))) (D.Z); T.valid_def case_node_6; T.source_def case_node_6;
      K.fits_def case_node_6 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (env_depth env))))) (T.source case_node_6));
    let case_node_5 = T.Lambda case_node_6 in
    ghost_ ( T.valid_def case_node_5; T.source_def case_node_5;
      K.fits_def case_node_5 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (env_depth env)))) (T.source case_node_5));
    let case_node_11 = T.bound 3 in
    ghost_ (T.decode_def 3; T.decode_def 2; T.decode_def 1; T.decode_def 0; D.present_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (D.S (D.S (D.S (D.Z)))); D.present_def (D.S (D.S (D.S (D.S (env_depth env))))) (D.S (D.S (D.Z))); D.present_def (D.S (D.S (D.S (env_depth env)))) (D.S (D.Z)); D.present_def (D.S (D.S (env_depth env))) (D.Z); T.valid_def case_node_11; T.source_def case_node_11;
      K.fits_def case_node_11 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (T.source case_node_11));
    let case_node_15 = T.bound 2 in
    ghost_ (T.decode_def 2; T.decode_def 1; T.decode_def 0; D.present_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (D.S (D.S (D.Z))); D.present_def (D.S (D.S (D.S (D.S (env_depth env))))) (D.S (D.Z)); D.present_def (D.S (D.S (D.S (env_depth env)))) (D.Z); T.valid_def case_node_15; T.source_def case_node_15;
      K.fits_def case_node_15 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (T.source case_node_15));
    let case_node_16 = T.bound 4 in
    ghost_ (T.decode_def 4; T.decode_def 3; T.decode_def 2; T.decode_def 1; T.decode_def 0; D.present_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (D.S (D.S (D.S (D.S (D.Z))))); D.present_def (D.S (D.S (D.S (D.S (env_depth env))))) (D.S (D.S (D.S (D.Z)))); D.present_def (D.S (D.S (D.S (env_depth env)))) (D.S (D.S (D.Z))); D.present_def (D.S (D.S (env_depth env))) (D.S (D.Z)); D.present_def (D.S (env_depth env)) (D.Z); T.valid_def case_node_16; T.source_def case_node_16;
      K.fits_def case_node_16 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (T.source case_node_16));
    let case_node_14 = T.Apply (case_node_15, case_node_16) in
    ghost_ ( T.valid_def case_node_14; T.source_def case_node_14;
      K.fits_def case_node_14 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (T.source case_node_14));
    let case_node_17 = T.bound 1 in
    ghost_ (T.decode_def 1; T.decode_def 0; D.present_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (D.S (D.Z)); D.present_def (D.S (D.S (D.S (D.S (env_depth env))))) (D.Z); T.valid_def case_node_17; T.source_def case_node_17;
      K.fits_def case_node_17 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (T.source case_node_17));
    let case_node_13 = T.Apply (case_node_14, case_node_17) in
    ghost_ ( T.valid_def case_node_13; T.source_def case_node_13;
      K.fits_def case_node_13 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (T.source case_node_13));
    let case_node_18 = T.Nil in
    ghost_ ( T.valid_def case_node_18; T.source_def case_node_18;
      K.fits_def case_node_18 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (T.source case_node_18));
    let case_node_12 = T.Cons (case_node_13, case_node_18) in
    ghost_ ( T.valid_def case_node_12; T.source_def case_node_12;
      K.fits_def case_node_12 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (T.source case_node_12));
    let case_node_10 = T.Cons (case_node_11, case_node_12) in
    ghost_ ( T.valid_def case_node_10; T.source_def case_node_10;
      K.fits_def case_node_10 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (D.S (env_depth env)))))) (T.source case_node_10));
    let case_node_9 = T.Lambda case_node_10 in
    ghost_ ( T.valid_def case_node_9; T.source_def case_node_9;
      K.fits_def case_node_9 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (env_depth env))))) (T.source case_node_9));
    let case_node_20 = T.bound 3 in
    ghost_ (T.decode_def 3; T.decode_def 2; T.decode_def 1; T.decode_def 0; D.present_def (D.S (D.S (D.S (D.S (env_depth env))))) (D.S (D.S (D.S (D.Z)))); D.present_def (D.S (D.S (D.S (env_depth env)))) (D.S (D.S (D.Z))); D.present_def (D.S (D.S (env_depth env))) (D.S (D.Z)); D.present_def (D.S (env_depth env)) (D.Z); T.valid_def case_node_20; T.source_def case_node_20;
      K.fits_def case_node_20 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (env_depth env))))) (T.source case_node_20));
    let case_node_23 = T.bound 0 in
    ghost_ (T.decode_def 0; D.present_def (D.S (D.S (D.S (D.S (env_depth env))))) (D.Z); T.valid_def case_node_23; T.source_def case_node_23;
      K.fits_def case_node_23 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (env_depth env))))) (T.source case_node_23));
    let case_node_24 = T.Nil in
    ghost_ ( T.valid_def case_node_24; T.source_def case_node_24;
      K.fits_def case_node_24 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (env_depth env))))) (T.source case_node_24));
    let case_node_22 = T.Cons (case_node_23, case_node_24) in
    ghost_ ( T.valid_def case_node_22; T.source_def case_node_22;
      K.fits_def case_node_22 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (env_depth env))))) (T.source case_node_22));
    let case_node_25 = T.Nil in
    ghost_ ( T.valid_def case_node_25; T.source_def case_node_25;
      K.fits_def case_node_25 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (env_depth env))))) (T.source case_node_25));
    let case_node_21 = T.Cons (case_node_22, case_node_25) in
    ghost_ ( T.valid_def case_node_21; T.source_def case_node_21;
      K.fits_def case_node_21 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (env_depth env))))) (T.source case_node_21));
    let case_node_19 = T.Cons (case_node_20, case_node_21) in
    ghost_ ( T.valid_def case_node_19; T.source_def case_node_19;
      K.fits_def case_node_19 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (env_depth env))))) (T.source case_node_19));
    let case_node_8 = T.Apply (case_node_9, case_node_19) in
    ghost_ ( T.valid_def case_node_8; T.source_def case_node_8;
      K.fits_def case_node_8 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (env_depth env))))) (T.source case_node_8));
    let case_node_7 = T.Lambda case_node_8 in
    ghost_ ( T.valid_def case_node_7; T.source_def case_node_7;
      K.fits_def case_node_7 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (env_depth env)))) (T.source case_node_7));
    let case_node_4 = T.Apply (case_node_5, case_node_7) in
    ghost_ ( T.valid_def case_node_4; T.source_def case_node_4;
      K.fits_def case_node_4 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (env_depth env)))) (T.source case_node_4));
    let case_node_3 = T.Lambda case_node_4 in
    ghost_ ( T.valid_def case_node_3; T.source_def case_node_3;
      K.fits_def case_node_3 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (env_depth env))) (T.source case_node_3));
    let case_node_2 = T.Lambda case_node_3 in
    ghost_ ( T.valid_def case_node_2; T.source_def case_node_2;
      K.fits_def case_node_2 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (env_depth env)) (T.source case_node_2));
    let case_node_1 = T.Lambda case_node_2 in
    ghost_ ( T.valid_def case_node_1; T.source_def case_node_1;
      K.fits_def case_node_1 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source case_node_1));
    let case_head = T.Lambda runtime_nonempty in
    ghost_ (T.valid_def case_head; T.source_def case_head;
      K.fits_def case_head depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (env_depth env)) (T.source case_head));
    let case_branch = T.Lambda case_head in
    ghost_ (T.valid_def case_branch; T.source_def case_branch;
      K.fits_def case_branch depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source case_branch));
    let case_scrutinee = T.Apply (case_node_1, runtime_scrutinee) in
    ghost_ (T.valid_def case_scrutinee; T.source_def case_scrutinee;
      K.fits_def case_scrutinee depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source case_scrutinee));
    let case_empty = T.Apply (case_scrutinee, runtime_empty) in
    ghost_ (T.valid_def case_empty; T.source_def case_empty;
      K.fits_def case_empty depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source case_empty));
    let args = T.Apply (case_empty, case_branch) in
    ghost_ (T.valid_def args; T.source_def args;
      K.fits_def args depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source args));
    ghost_ (Hm_list_case_constraints.selector_def ();
      Hm_list_case_constraints.encoded_def (T.source runtime_scrutinee) (T.source runtime_empty) (T.source runtime_nonempty));
    let call_pool : pool Ghost.t = {Ghost.ghost = ghost_ pool} in
    let resume : (answer : {r : inference | ran h.Ghost.ghost depth call_pool.Ghost.ghost (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source args && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun answer ->
      let execution = ghost_ (RCaseList (T.source runtime_scrutinee, T.source runtime_empty,
        T.source runtime_nonempty, answer.#execution)) in
      let trace = if collect_trace then Annotation.List_case answer.#trace else Annotation.Failed in
      let after = ghost_ (Pref.own (borrow_ answer.#state)) in
      ghost_ (ran_def h.Ghost.ghost depth pool env execution after answer.#pool;
        source_def execution; result_def execution;
        Hm_annotation_trace_spec.records_def trace execution);
      let out = #{value = answer.#value; state = answer.#state; pool = answer.#pool;
        trace; execution; physical = answer.#physical; pools = answer.#pools; routing = answer.#routing} in
      use (refine_ out) in
    let call_facts : (((x : node Pref.t) @ immutable ->
      {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost depth call_pool.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; ())} in
    work collect_trace goal h heads trees depth call_pool call_facts runtime_env args physical pools routing (refine_ state) resume
  | T.If (runtime_condition, runtime_yes, runtime_no) ->
    let if_node_6 = T.bound 2 in
    ghost_ (T.decode_def 2; T.decode_def 1; T.decode_def 0; D.present_def (D.S (D.S (D.S (D.S (env_depth env))))) (D.S (D.S (D.Z))); D.present_def (D.S (D.S (D.S (env_depth env)))) (D.S (D.Z)); D.present_def (D.S (D.S (env_depth env))) (D.Z); T.valid_def if_node_6; T.source_def if_node_6;
      K.fits_def if_node_6 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (D.S (env_depth env))))) (T.source if_node_6));
    let if_node_5 = T.Lambda if_node_6 in
    ghost_ ( T.valid_def if_node_5; T.source_def if_node_5;
      K.fits_def if_node_5 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (env_depth env)))) (T.source if_node_5));
    let if_node_8 = T.bound 1 in
    ghost_ (T.decode_def 1; T.decode_def 0; D.present_def (D.S (D.S (D.S (env_depth env)))) (D.S (D.Z)); D.present_def (D.S (D.S (env_depth env))) (D.Z); T.valid_def if_node_8; T.source_def if_node_8;
      K.fits_def if_node_8 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (env_depth env)))) (T.source if_node_8));
    let if_node_10 = T.bound 0 in
    ghost_ (T.decode_def 0; D.present_def (D.S (D.S (D.S (env_depth env)))) (D.Z); T.valid_def if_node_10; T.source_def if_node_10;
      K.fits_def if_node_10 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (env_depth env)))) (T.source if_node_10));
    let if_node_11 = T.Nil in
    ghost_ ( T.valid_def if_node_11; T.source_def if_node_11;
      K.fits_def if_node_11 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (env_depth env)))) (T.source if_node_11));
    let if_node_9 = T.Cons (if_node_10, if_node_11) in
    ghost_ ( T.valid_def if_node_9; T.source_def if_node_9;
      K.fits_def if_node_9 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (env_depth env)))) (T.source if_node_9));
    let if_node_7 = T.Cons (if_node_8, if_node_9) in
    ghost_ ( T.valid_def if_node_7; T.source_def if_node_7;
      K.fits_def if_node_7 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (env_depth env)))) (T.source if_node_7));
    let if_node_4 = T.Apply (if_node_5, if_node_7) in
    ghost_ ( T.valid_def if_node_4; T.source_def if_node_4;
      K.fits_def if_node_4 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (D.S (env_depth env)))) (T.source if_node_4));
    let if_node_3 = T.Lambda if_node_4 in
    ghost_ ( T.valid_def if_node_3; T.source_def if_node_3;
      K.fits_def if_node_3 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (D.S (env_depth env))) (T.source if_node_3));
    let if_node_2 = T.Lambda if_node_3 in
    ghost_ ( T.valid_def if_node_2; T.source_def if_node_2;
      K.fits_def if_node_2 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (D.S (env_depth env)) (T.source if_node_2));
    let if_node_1 = T.Lambda if_node_2 in
    ghost_ ( T.valid_def if_node_1; T.source_def if_node_1;
      K.fits_def if_node_1 depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source if_node_1));
    let if_truth = T.Truth in
    ghost_ (T.valid_def if_truth; T.source_def if_truth;
      K.fits_def if_truth depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source if_truth));
    let if_nil = T.Nil in
    ghost_ (T.valid_def if_nil; T.source_def if_nil;
      K.fits_def if_nil depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source if_nil));
    let if_tail = T.Cons (if_truth, if_nil) in
    ghost_ (T.valid_def if_tail; T.source_def if_tail;
      K.fits_def if_tail depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source if_tail));
    let if_condition = T.Cons (runtime_condition, if_tail) in
    ghost_ (T.valid_def if_condition; T.source_def if_condition;
      K.fits_def if_condition depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source if_condition));
    let if_checked = T.Apply (if_node_1, if_condition) in
    ghost_ (T.valid_def if_checked; T.source_def if_checked;
      K.fits_def if_checked depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source if_checked));
    let if_yes = T.Apply (if_checked, runtime_yes) in
    ghost_ (T.valid_def if_yes; T.source_def if_yes;
      K.fits_def if_yes depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source if_yes));
    let args = T.Apply (if_yes, runtime_no) in
    ghost_ (T.valid_def args; T.source_def args;
      K.fits_def args depth (Iarray.length goal.origin.S.buckets);
      D.scoped_term_def (env_depth env) (T.source args));
    ghost_ (Hm_conditional_constraints.selector_def ();
      Hm_conditional_constraints.condition_def (T.source runtime_condition);
      Hm_conditional_constraints.encoded_def (T.source runtime_condition) (T.source runtime_yes) (T.source runtime_no));
    let call_pool : pool Ghost.t = {Ghost.ghost = ghost_ pool} in
    let resume : (answer : {r : inference | ran h.Ghost.ghost depth call_pool.Ghost.ghost (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source args && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun answer ->
      let execution = ghost_ (RIf (T.source runtime_condition, T.source runtime_yes,
        T.source runtime_no, answer.#execution)) in
      let trace = if collect_trace then Annotation.Conditional answer.#trace else Annotation.Failed in
      let after = ghost_ (Pref.own (borrow_ answer.#state)) in
      ghost_ (ran_def h.Ghost.ghost depth pool env execution after answer.#pool;
        source_def execution; result_def execution;
        Hm_annotation_trace_spec.records_def trace execution);
      let out = #{value = answer.#value; state = answer.#state; pool = answer.#pool;
        trace; execution; physical = answer.#physical; pools = answer.#pools; routing = answer.#routing} in
      use (refine_ out) in
    let call_facts : (((x : node Pref.t) @ immutable ->
      {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost depth call_pool.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; ())} in
    work collect_trace goal h heads trees depth call_pool call_facts runtime_env args physical pools routing (refine_ state) resume
  | T.Primitive (op, runtime_left, runtime_right) ->
    let zero = T.Word {Hmc_word64.lo = 0; hi = 0} in
    let nil = T.Nil in let last = T.Cons (zero, nil) in
    let rest = T.Cons (runtime_right, last) in let args = T.Cons (runtime_left, rest) in
    ghost_ (T.valid_def zero; T.valid_def nil; T.valid_def last; T.valid_def rest; T.valid_def args;
      T.source_def zero; T.source_def nil; T.source_def last; T.source_def rest; T.source_def args;
      Hm_primitive_constraints.arguments_def (T.source runtime_left) (T.source runtime_right);
      let limit = Iarray.length goal.origin.S.buckets in
      K.fits_def zero depth limit; K.fits_def nil depth limit; K.fits_def last depth limit;
      K.fits_def rest depth limit; K.fits_def args depth limit;
      let n = env_depth env in
      D.scoped_term_def n (T.source zero); D.scoped_term_def n (T.source nil);
      D.scoped_term_def n (T.source last); D.scoped_term_def n (T.source rest);
      D.scoped_term_def n (T.source args));
    let call_pool : pool Ghost.t = {Ghost.ghost = ghost_ pool} in
    let resume : (answer : {r : inference | ran h.Ghost.ghost depth call_pool.Ghost.ghost (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source args && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun answer ->
      let physical = answer.#physical in let pools = answer.#pools in
      let routing = ghost_ answer.#routing in let child_trace = answer.#trace in
      let body = ghost_ answer.#execution in let body_pool = ghost_ answer.#pool in
      let state = answer.#state in
      let middle : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
      match answer.#value with
      | None ->
        let execution = ghost_ (RPrimitive (op, T.source runtime_left, T.source runtime_right,
          body, middle.Ghost.ghost, body_pool, None)) in
        ghost_ (ran_def h.Ghost.ghost depth pool env execution middle.Ghost.ghost body_pool;
          source_def execution; result_def execution);
        let trace = if collect_trace then Annotation.Primitive (op, None, child_trace) else Annotation.Failed in
        ghost_ (Hm_annotation_trace_spec.records_def trace execution);
        let out = #{value = None; state; pool = body_pool; trace; execution; physical; pools; routing} in
        use (refine_ out)
      | Some _ ->
        let ts : (((x : node Pref.t) @ immutable ->
          {t : tree | tree_root t === x && (if H.mem middle.Ghost.ghost x then finite middle.Ghost.ghost t else observe middle.Ghost.ghost x === None)} @ immutable)) Ghost.t =
          {Ghost.ghost = ghost_ (fun x -> let refine_ t = G.run_forest h.Ghost.ghost trees.Ghost.ghost depth pool env body middle.Ghost.ghost body_pool x () in refine_ t)} in
        let[@def] mid_heads_selected : E.heads @ ghost = ghost_ (fun x ->
          let refine_ r = Forest_heads.select middle.Ghost.ghost ts.Ghost.ghost x in r) in
        let mid_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ mid_heads_selected} in
        let mid_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle.Ghost.ghost mid_heads.Ghost.ghost x})) Ghost.t =
          {Ghost.ghost = ghost_ (fun x -> mid_heads_selected_def x;
            let refine_ r = Forest_heads.select middle.Ghost.ghost ts.Ghost.ghost x in
            E.valid_head_def middle.Ghost.ghost mid_heads.Ghost.ghost x; ())} in
        let mid_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle.Ghost.ghost mid_heads.Ghost.ghost depth body_pool x})) Ghost.t =
          {Ghost.ghost = ghost_ (fun x -> Hm_effective_invariant.run_invariant h.Ghost.ghost heads.Ghost.ghost trees.Ghost.ghost depth pool facts.Ghost.ghost env body middle.Ghost.ghost mid_heads.Ghost.ghost mid_valid.Ghost.ghost body_pool x (); ())} in
        ghost_ (let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe middle.Ghost.ghost mid_heads.Ghost.ghost x}) @ total = fun x ->
          mid_facts.Ghost.ghost x; runtime_at_def middle.Ghost.ghost mid_heads.Ghost.ghost depth body_pool x; () in
          Dp.run_pool_scoped h.Ghost.ghost depth pool env body middle.Ghost.ghost mid_heads.Ghost.ghost safe body_pool ();
          C.scoped_runtime middle.Ghost.ghost routing mid_heads.Ghost.ghost depth body_pool mid_facts.Ghost.ghost ());
        let desc = primitive_desc op in
        ghost_ (primitive_desc_def op; cell_def desc depth; payload_scoped_def middle.Ghost.ghost (cell desc depth));
        let refine_ allocated = Effective_allocator.allocate middle depth desc physical (refine_ state) in
        let final_pool = ghost_ (Entry (allocated.#value, body_pool)) in
        ghost_ (allocated_def middle.Ghost.ghost depth allocated.#value desc;
          C.allocation_pool middle.Ghost.ghost depth allocated.#value desc body_pool ());
        let refine_ routing = ghost_ (C.allocated middle.Ghost.ghost routing allocated.#value desc ()) in
        let value = Some allocated.#value in
        let execution = ghost_ (RPrimitive (op, T.source runtime_left, T.source runtime_right,
          body, middle.Ghost.ghost, body_pool, value)) in
        let after = ghost_ (Pref.own (borrow_ allocated.#state)) in
        ghost_ (ran_def h.Ghost.ghost depth pool env execution after final_pool;
          source_def execution; result_def execution);
        let trace = if collect_trace then Annotation.Primitive (op, value, child_trace) else Annotation.Failed in
        ghost_ (Hm_annotation_trace_spec.records_def trace execution);
        let out = #{value; state = allocated.#state; pool = final_pool; trace; execution;
          physical = allocated.#pool; pools; routing} in use (refine_ out)
    in
    let call_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost depth call_pool.Ghost.ghost x})) Ghost.t =
      {Ghost.ghost = ghost_ (refine_ facts.Ghost.ghost)} in
    work collect_trace goal h heads trees depth call_pool call_facts runtime_env args physical pools routing (refine_ state) resume
  | T.Cons (runtime_left, runtime_right) ->
  let call_pool_2 : pool Ghost.t = {Ghost.ghost = ghost_ pool} in
  let resume_left : (answer : {r : inference | ran h.Ghost.ghost depth call_pool_2.Ghost.ghost (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_left && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun answer ->
    let physical = answer.#physical in let pools = answer.#pools in
    let routing = ghost_ answer.#routing in
  let left_trace = answer.#trace in
  let left_run = ghost_ answer.#execution in let pool1 = answer.#pool in let state = answer.#state in
  let h1 : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RCons_left (left_run, T.source runtime_right)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after pool1;
    source_def execution; result_def execution);
  let trace = Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = None; state = state; pool = pool1; trace; execution; physical; pools; routing} in use (refine_ out)
  | Some fn ->
  let first_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h1.Ghost.ghost x then finite h1.Ghost.ghost t else observe h1.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let refine_ t = G.run_forest h.Ghost.ghost trees.Ghost.ghost depth pool env left_run h1.Ghost.ghost pool1 x () in refine_ t)} in
  let[@def] first_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select h1.Ghost.ghost first_trees.Ghost.ghost x in r) in
  let first_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ first_heads_selected} in
  let first_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1.Ghost.ghost first_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> first_heads_selected_def x;
      let refine_ r = Forest_heads.select h1.Ghost.ghost first_trees.Ghost.ghost x in
      E.valid_head_def h1.Ghost.ghost first_heads.Ghost.ghost x; ())} in
  let first_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1.Ghost.ghost first_heads.Ghost.ghost depth pool1 x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> Hm_effective_invariant.run_invariant h.Ghost.ghost heads.Ghost.ghost trees.Ghost.ghost depth pool facts.Ghost.ghost env left_run h1.Ghost.ghost first_heads.Ghost.ghost first_valid.Ghost.ghost pool1 x (); ())} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe h1.Ghost.ghost first_heads.Ghost.ghost x}) @ total = fun x ->
      first_facts.Ghost.ghost x; runtime_at_def h1.Ghost.ghost first_heads.Ghost.ghost depth pool1 x; () in
    Dp.run_pool_scoped h.Ghost.ghost depth pool env left_run h1.Ghost.ghost first_heads.Ghost.ghost safe pool1 ();
    Dp.run_env_owned h.Ghost.ghost depth pool env left_run h1.Ghost.ghost pool1 (); ());
  let call_pool_1 : pool Ghost.t = {Ghost.ghost = ghost_ pool1} in
  let resume_right : (answer : {r : inference | ran h1.Ghost.ghost depth call_pool_1.Ghost.ghost (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_right && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun answer ->
    let physical = answer.#physical in let pools = answer.#pools in
    let routing = ghost_ answer.#routing in
  let right_trace = answer.#trace in
  let right_run = ghost_ answer.#execution in let pool2 = answer.#pool in let state = answer.#state in
  let h2 : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RCons_right (left_run, right_run, h1.Ghost.ghost, pool1)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after pool2;
    source_def execution; result_def execution);
  let trace = Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = None; state = state; pool = pool2; trace; execution; physical; pools; routing} in use (refine_ out)
  | Some actual ->
  let second_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h2.Ghost.ghost x then finite h2.Ghost.ghost t else observe h2.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let refine_ t = G.run_forest h1.Ghost.ghost first_trees.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 x () in refine_ t)} in
  let[@def] second_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select h2.Ghost.ghost second_trees.Ghost.ghost x in r) in
  let second_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ second_heads_selected} in
  let second_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h2.Ghost.ghost second_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> second_heads_selected_def x;
      let refine_ r = Forest_heads.select h2.Ghost.ghost second_trees.Ghost.ghost x in
      E.valid_head_def h2.Ghost.ghost second_heads.Ghost.ghost x; ())} in
  let second_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2.Ghost.ghost second_heads.Ghost.ghost depth pool2 x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> Hm_effective_invariant.run_invariant h1.Ghost.ghost first_heads.Ghost.ghost first_trees.Ghost.ghost depth pool1 first_facts.Ghost.ghost env right_run h2.Ghost.ghost second_heads.Ghost.ghost second_valid.Ghost.ghost pool2 x (); ())} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe h2.Ghost.ghost second_heads.Ghost.ghost x}) @ total = fun x ->
      second_facts.Ghost.ghost x; runtime_at_def h2.Ghost.ghost second_heads.Ghost.ghost depth pool2 x; () in
    Dp.run_pool_scoped h1.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost second_heads.Ghost.ghost safe pool2 ();
    Dp.run_env_owned h1.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 (); ());
  ghost_ (second_facts.Ghost.ghost actual; Hm_effective_result.result_below h1.Ghost.ghost first_trees.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 second_heads.Ghost.ghost actual ();
    E.effective_below_def h2.Ghost.ghost second_heads.Ghost.ghost actual depth; E.effective_active_def h2.Ghost.ghost second_heads.Ghost.ghost actual; ());
  ghost_ (first_facts.Ghost.ghost fn;
    Hm_effective_result.result_below h.Ghost.ghost trees.Ghost.ghost depth pool env left_run h1.Ghost.ghost pool1 first_heads.Ghost.ghost fn ();
    first_valid.Ghost.ghost fn; second_valid.Ghost.ghost fn;
    Hm_effective_paths.run_below h1.Ghost.ghost first_heads.Ghost.ghost second_heads.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 fn depth ();
    E.effective_below_def h2.Ghost.ghost second_heads.Ghost.ghost fn depth);
  let var = List fn in
  ghost_ (cell_def var depth; let v = cell var depth in payload_scoped_def h2.Ghost.ghost v; ());
  ghost_ (C.scoped_runtime h2.Ghost.ghost routing second_heads.Ghost.ghost depth pool2 second_facts.Ghost.ghost (); ());
  let refine_ result_allocation = Effective_allocator.allocate h2 depth var physical (refine_ state) in
  let result_allocation_virtual = ghost_ (Entry (result_allocation.#value, pool2)) in
  ghost_ (allocated_def h2.Ghost.ghost depth result_allocation.#value var);
  ghost_ (C.allocation_pool h2.Ghost.ghost depth result_allocation.#value var pool2 ());
  let refine_ routing = ghost_ (C.allocated h2.Ghost.ghost routing result_allocation.#value var ()) in
  let physical = result_allocation.#pool in
  ghost_ (allocated_def h2.Ghost.ghost depth result_allocation.#value var);
  let result_node = result_allocation.#value in let result_pool = result_allocation_virtual in
  let state = result_allocation.#state in
  let result_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let result_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem result_heap.Ghost.ghost x then finite result_heap.Ghost.ghost t else observe result_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let next = G.allocated_forest h2.Ghost.ghost second_trees.Ghost.ghost depth result_node var () in
      let refine_ t = next x in refine_ t)} in
  let[@def] result_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select result_heap.Ghost.ghost result_trees.Ghost.ghost x in r) in
  let result_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ result_heads_selected} in
  let result_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head result_heap.Ghost.ghost result_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> result_heads_selected_def x;
      let refine_ r = Forest_heads.select result_heap.Ghost.ghost result_trees.Ghost.ghost x in
      E.valid_head_def result_heap.Ghost.ghost result_heads.Ghost.ghost x; ())} in
  let result_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at result_heap.Ghost.ghost result_heads.Ghost.ghost depth result_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> second_facts.Ghost.ghost x;
      allocated_def h2.Ghost.ghost depth result_node var;
      A.allocate_runtime h2.Ghost.ghost second_heads.Ghost.ghost result_heads.Ghost.ghost depth pool2 result_node var second_valid.Ghost.ghost (refine_ result_valid.Ghost.ghost) x (); ())} in
  ghost_ (result_valid.Ghost.ghost result_node; A.allocated_below h2.Ghost.ghost depth result_node var result_heads.Ghost.ghost ();
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost result_node depth; ());
  ghost_ (second_valid.Ghost.ghost actual; result_valid.Ghost.ghost actual;
    let v = cell var depth in A.saved_below h2.Ghost.ghost second_heads.Ghost.ghost result_heads.Ghost.ghost result_node v actual depth ();
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost actual depth; ());
  ghost_ (E.effective_active_def result_heap.Ghost.ghost result_heads.Ghost.ghost result_node;
    E.effective_active_def result_heap.Ghost.ghost result_heads.Ghost.ghost actual);
  let d : int Ghost.t = {Ghost.ghost = ghost_ depth} in let pool_proof : pool Ghost.t = {Ghost.ghost = ghost_ result_pool} in
  let unify_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at result_heap.Ghost.ghost result_heads.Ghost.ghost d.Ghost.ghost pool_proof.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (refine_ result_facts.Ghost.ghost)} in
  let refine_ solved = Effective_hm_unify.unify result_heap result_heads d pool_proof unify_facts result_trees actual result_node (refine_ state) in
  let after_unify = ghost_ (Pref.own (borrow_ solved.#state)) in
  let refine_ routing = ghost_ (C.unified result_heap.Ghost.ghost routing actual result_node solved.#ok after_unify solved.#derivation ()) in
  let value = if solved.#ok then Some result_node else None in
  let execution = ghost_ (RCons (left_run, right_run, h1.Ghost.ghost, pool1, h2.Ghost.ghost, pool2, result_node, solved.#ok, solved.#derivation)) in
  let after = ghost_ (Pref.own (borrow_ solved.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after result_pool;
    source_def execution; result_def execution);
  let trace = if collect_trace then Annotation.List_constructor (value, left_trace, right_trace) else Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = value; state = solved.#state; pool = result_pool; trace; execution; physical; pools; routing} in use (refine_ out)
  in
  let call_facts_1 : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1.Ghost.ghost first_heads.Ghost.ghost depth call_pool_1.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> first_facts.Ghost.ghost x; ())} in
  work collect_trace goal h1 first_heads first_trees depth call_pool_1 call_facts_1 runtime_env runtime_right physical pools routing (refine_ state) resume_right
  in
  let call_facts_2 : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost depth call_pool_2.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; ())} in
  work collect_trace goal h heads trees depth call_pool_2 call_facts_2 runtime_env runtime_left physical pools routing (refine_ state) resume_left
  | T.Apply (runtime_left, runtime_right) ->
  let call_pool_2 : pool Ghost.t = {Ghost.ghost = ghost_ pool} in
  let resume_left : (answer : {r : inference | ran h.Ghost.ghost depth call_pool_2.Ghost.ghost (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_left && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun answer ->
    let physical = answer.#physical in let pools = answer.#pools in
    let routing = ghost_ answer.#routing in
  let left_trace = answer.#trace in
  let left_run = ghost_ answer.#execution in let pool1 = answer.#pool in let state = answer.#state in
  let h1 : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RApp_left (left_run, T.source runtime_right)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after pool1;
    source_def execution; result_def execution);
  let trace = Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = None; state = state; pool = pool1; trace; execution; physical; pools; routing} in use (refine_ out)
  | Some fn ->
  let first_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h1.Ghost.ghost x then finite h1.Ghost.ghost t else observe h1.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let refine_ t = G.run_forest h.Ghost.ghost trees.Ghost.ghost depth pool env left_run h1.Ghost.ghost pool1 x () in refine_ t)} in
  let[@def] first_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select h1.Ghost.ghost first_trees.Ghost.ghost x in r) in
  let first_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ first_heads_selected} in
  let first_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1.Ghost.ghost first_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> first_heads_selected_def x;
      let refine_ r = Forest_heads.select h1.Ghost.ghost first_trees.Ghost.ghost x in
      E.valid_head_def h1.Ghost.ghost first_heads.Ghost.ghost x; ())} in
  let first_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1.Ghost.ghost first_heads.Ghost.ghost depth pool1 x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> Hm_effective_invariant.run_invariant h.Ghost.ghost heads.Ghost.ghost trees.Ghost.ghost depth pool facts.Ghost.ghost env left_run h1.Ghost.ghost first_heads.Ghost.ghost first_valid.Ghost.ghost pool1 x (); ())} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe h1.Ghost.ghost first_heads.Ghost.ghost x}) @ total = fun x ->
      first_facts.Ghost.ghost x; runtime_at_def h1.Ghost.ghost first_heads.Ghost.ghost depth pool1 x; () in
    Dp.run_pool_scoped h.Ghost.ghost depth pool env left_run h1.Ghost.ghost first_heads.Ghost.ghost safe pool1 ();
    Dp.run_env_owned h.Ghost.ghost depth pool env left_run h1.Ghost.ghost pool1 (); ());
  let call_pool_1 : pool Ghost.t = {Ghost.ghost = ghost_ pool1} in
  let resume_right : (answer : {r : inference | ran h1.Ghost.ghost depth call_pool_1.Ghost.ghost (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_right && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun answer ->
    let physical = answer.#physical in let pools = answer.#pools in
    let routing = ghost_ answer.#routing in
  let right_trace = answer.#trace in
  let right_run = ghost_ answer.#execution in let pool2 = answer.#pool in let state = answer.#state in
  let h2 : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RApp_right (left_run, right_run, h1.Ghost.ghost, pool1)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after pool2;
    source_def execution; result_def execution);
  let trace = Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = None; state = state; pool = pool2; trace; execution; physical; pools; routing} in use (refine_ out)
  | Some actual ->
  let second_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h2.Ghost.ghost x then finite h2.Ghost.ghost t else observe h2.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let refine_ t = G.run_forest h1.Ghost.ghost first_trees.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 x () in refine_ t)} in
  let[@def] second_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select h2.Ghost.ghost second_trees.Ghost.ghost x in r) in
  let second_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ second_heads_selected} in
  let second_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h2.Ghost.ghost second_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> second_heads_selected_def x;
      let refine_ r = Forest_heads.select h2.Ghost.ghost second_trees.Ghost.ghost x in
      E.valid_head_def h2.Ghost.ghost second_heads.Ghost.ghost x; ())} in
  let second_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2.Ghost.ghost second_heads.Ghost.ghost depth pool2 x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> Hm_effective_invariant.run_invariant h1.Ghost.ghost first_heads.Ghost.ghost first_trees.Ghost.ghost depth pool1 first_facts.Ghost.ghost env right_run h2.Ghost.ghost second_heads.Ghost.ghost second_valid.Ghost.ghost pool2 x (); ())} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe h2.Ghost.ghost second_heads.Ghost.ghost x}) @ total = fun x ->
      second_facts.Ghost.ghost x; runtime_at_def h2.Ghost.ghost second_heads.Ghost.ghost depth pool2 x; () in
    Dp.run_pool_scoped h1.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost second_heads.Ghost.ghost safe pool2 ();
    Dp.run_env_owned h1.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 (); ());
  ghost_ (second_facts.Ghost.ghost actual; Hm_effective_result.result_below h1.Ghost.ghost first_trees.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 second_heads.Ghost.ghost actual ();
    E.effective_below_def h2.Ghost.ghost second_heads.Ghost.ghost actual depth; E.effective_active_def h2.Ghost.ghost second_heads.Ghost.ghost actual; ());
  let var = Var in
  ghost_ (cell_def var depth; let v = cell var depth in payload_scoped_def h2.Ghost.ghost v; ());
  ghost_ (C.scoped_runtime h2.Ghost.ghost routing second_heads.Ghost.ghost depth pool2 second_facts.Ghost.ghost (); ());
  let refine_ result_allocation = Effective_allocator.allocate h2 depth var physical (refine_ state) in
  let result_allocation_virtual = ghost_ (Entry (result_allocation.#value, pool2)) in
  ghost_ (allocated_def h2.Ghost.ghost depth result_allocation.#value var);
  ghost_ (C.allocation_pool h2.Ghost.ghost depth result_allocation.#value var pool2 ());
  let refine_ routing = ghost_ (C.allocated h2.Ghost.ghost routing result_allocation.#value var ()) in
  let physical = result_allocation.#pool in
  ghost_ (allocated_def h2.Ghost.ghost depth result_allocation.#value var);
  let result_node = result_allocation.#value in let result_pool = result_allocation_virtual in
  let state = result_allocation.#state in
  let result_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let result_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem result_heap.Ghost.ghost x then finite result_heap.Ghost.ghost t else observe result_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let next = G.allocated_forest h2.Ghost.ghost second_trees.Ghost.ghost depth result_node var () in
      let refine_ t = next x in refine_ t)} in
  let[@def] result_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select result_heap.Ghost.ghost result_trees.Ghost.ghost x in r) in
  let result_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ result_heads_selected} in
  let result_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head result_heap.Ghost.ghost result_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> result_heads_selected_def x;
      let refine_ r = Forest_heads.select result_heap.Ghost.ghost result_trees.Ghost.ghost x in
      E.valid_head_def result_heap.Ghost.ghost result_heads.Ghost.ghost x; ())} in
  let result_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at result_heap.Ghost.ghost result_heads.Ghost.ghost depth result_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> second_facts.Ghost.ghost x;
      allocated_def h2.Ghost.ghost depth result_node var;
      A.allocate_runtime h2.Ghost.ghost second_heads.Ghost.ghost result_heads.Ghost.ghost depth pool2 result_node var second_valid.Ghost.ghost (refine_ result_valid.Ghost.ghost) x (); ())} in
  ghost_ (result_valid.Ghost.ghost result_node; A.allocated_below h2.Ghost.ghost depth result_node var result_heads.Ghost.ghost ();
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost result_node depth; ());
  ghost_ (second_valid.Ghost.ghost actual; result_valid.Ghost.ghost actual;
    let v = cell var depth in A.saved_below h2.Ghost.ghost second_heads.Ghost.ghost result_heads.Ghost.ghost result_node v actual depth ();
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost actual depth; ());
  let desc = Arrow (actual, result_node) in
  ghost_ (cell_def desc depth; let v = cell desc depth in payload_scoped_def result_heap.Ghost.ghost v; ());
  ghost_ (C.scoped_runtime result_heap.Ghost.ghost routing result_heads.Ghost.ghost depth result_pool result_facts.Ghost.ghost (); ());
  let refine_ arrow_allocation = Effective_allocator.allocate result_heap depth desc physical (refine_ state) in
  let arrow_allocation_virtual = ghost_ (Entry (arrow_allocation.#value, result_pool)) in
  ghost_ (allocated_def result_heap.Ghost.ghost depth arrow_allocation.#value desc);
  ghost_ (C.allocation_pool result_heap.Ghost.ghost depth arrow_allocation.#value desc result_pool ());
  let refine_ routing = ghost_ (C.allocated result_heap.Ghost.ghost routing arrow_allocation.#value desc ()) in
  let physical = arrow_allocation.#pool in
  ghost_ (allocated_def result_heap.Ghost.ghost depth arrow_allocation.#value desc);
  let arrow_node = arrow_allocation.#value in let arrow_pool = arrow_allocation_virtual in
  let state = arrow_allocation.#state in
  let arrow_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let arrow_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem arrow_heap.Ghost.ghost x then finite arrow_heap.Ghost.ghost t else observe arrow_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let next = G.allocated_forest result_heap.Ghost.ghost result_trees.Ghost.ghost depth arrow_node desc () in
      let refine_ t = next x in refine_ t)} in
  let[@def] arrow_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select arrow_heap.Ghost.ghost arrow_trees.Ghost.ghost x in r) in
  let arrow_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ arrow_heads_selected} in
  let arrow_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> arrow_heads_selected_def x;
      let refine_ r = Forest_heads.select arrow_heap.Ghost.ghost arrow_trees.Ghost.ghost x in
      E.valid_head_def arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost x; ())} in
  let arrow_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost depth arrow_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> result_facts.Ghost.ghost x;
      allocated_def result_heap.Ghost.ghost depth arrow_node desc;
      A.allocate_runtime result_heap.Ghost.ghost result_heads.Ghost.ghost arrow_heads.Ghost.ghost depth result_pool arrow_node desc result_valid.Ghost.ghost (refine_ arrow_valid.Ghost.ghost) x (); ())} in
  ghost_ (first_facts.Ghost.ghost fn; Hm_effective_result.result_below h.Ghost.ghost trees.Ghost.ghost depth pool env left_run h1.Ghost.ghost pool1 first_heads.Ghost.ghost fn ();
    E.effective_below_def h1.Ghost.ghost first_heads.Ghost.ghost fn depth; E.effective_active_def h1.Ghost.ghost first_heads.Ghost.ghost fn; ());
  ghost_ (first_valid.Ghost.ghost fn; second_valid.Ghost.ghost fn; Hm_effective_paths.run_below h1.Ghost.ghost first_heads.Ghost.ghost second_heads.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 fn depth ();
    result_valid.Ghost.ghost fn; let v = cell var depth in A.saved_below h2.Ghost.ghost second_heads.Ghost.ghost result_heads.Ghost.ghost result_node v fn depth ();
    arrow_valid.Ghost.ghost fn; let w = cell desc depth in A.saved_below result_heap.Ghost.ghost result_heads.Ghost.ghost arrow_heads.Ghost.ghost arrow_node w fn depth ();
    E.effective_below_def arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost fn depth; E.effective_active_def arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost fn; ());
  ghost_ (arrow_valid.Ghost.ghost arrow_node; A.allocated_below result_heap.Ghost.ghost depth arrow_node desc arrow_heads.Ghost.ghost ();
    E.effective_below_def arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost arrow_node depth; ());
  ghost_ (E.effective_active_def arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost arrow_node);
  let d : int Ghost.t = {Ghost.ghost = ghost_ depth} in let pool_proof : pool Ghost.t = {Ghost.ghost = ghost_ arrow_pool} in
  let unify_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost d.Ghost.ghost pool_proof.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (refine_ arrow_facts.Ghost.ghost)} in
  let refine_ solved = Effective_hm_unify.unify arrow_heap arrow_heads d pool_proof unify_facts arrow_trees fn arrow_node (refine_ state) in
  let after_unify = ghost_ (Pref.own (borrow_ solved.#state)) in
  let refine_ routing = ghost_ (C.unified arrow_heap.Ghost.ghost routing fn arrow_node solved.#ok after_unify solved.#derivation ()) in
  let value = if solved.#ok then Some result_node else None in
  let execution = ghost_ (RApp (left_run, right_run, h1.Ghost.ghost, pool1, h2.Ghost.ghost, pool2, result_node, arrow_node, solved.#ok, solved.#derivation)) in
  let after = ghost_ (Pref.own (borrow_ solved.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after arrow_pool;
    source_def execution; result_def execution);
  let trace = if collect_trace then Annotation.Application (value, left_trace, right_trace) else Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = value; state = solved.#state; pool = arrow_pool; trace; execution; physical; pools; routing} in use (refine_ out)
  in
  let call_facts_1 : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1.Ghost.ghost first_heads.Ghost.ghost depth call_pool_1.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> first_facts.Ghost.ghost x; ())} in
  work collect_trace goal h1 first_heads first_trees depth call_pool_1 call_facts_1 runtime_env runtime_right physical pools routing (refine_ state) resume_right
  in
  let call_facts_2 : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost depth call_pool_2.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; ())} in
  work collect_trace goal h heads trees depth call_pool_2 call_facts_2 runtime_env runtime_left physical pools routing (refine_ state) resume_left
  | T.Recursive runtime_body ->
    let var = Var in
  ghost_ (cell_def var depth; let v = cell var depth in payload_scoped_def h.Ghost.ghost v; ());
  ghost_ (C.scoped_runtime h.Ghost.ghost routing heads.Ghost.ghost depth pool facts.Ghost.ghost (); ());
  let refine_ arg_allocation = Effective_allocator.allocate h depth var physical (refine_ state) in
  let arg_allocation_virtual = ghost_ (Entry (arg_allocation.#value, pool)) in
  ghost_ (allocated_def h.Ghost.ghost depth arg_allocation.#value var);
  ghost_ (C.allocation_pool h.Ghost.ghost depth arg_allocation.#value var pool ());
  let refine_ routing = ghost_ (C.allocated h.Ghost.ghost routing arg_allocation.#value var ()) in
  let physical = arg_allocation.#pool in
  ghost_ (allocated_def h.Ghost.ghost depth arg_allocation.#value var);
  let argument = arg_allocation.#value in let arg_pool = arg_allocation_virtual in
  let state = arg_allocation.#state in
  let arg_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let arg_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem arg_heap.Ghost.ghost x then finite arg_heap.Ghost.ghost t else observe arg_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let next = G.allocated_forest h.Ghost.ghost trees.Ghost.ghost depth argument var () in
      let refine_ t = next x in refine_ t)} in
  let[@def] arg_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select arg_heap.Ghost.ghost arg_trees.Ghost.ghost x in r) in
  let arg_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ arg_heads_selected} in
  let arg_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head arg_heap.Ghost.ghost arg_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> arg_heads_selected_def x;
      let refine_ r = Forest_heads.select arg_heap.Ghost.ghost arg_trees.Ghost.ghost x in
      E.valid_head_def arg_heap.Ghost.ghost arg_heads.Ghost.ghost x; ())} in
  let arg_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at arg_heap.Ghost.ghost arg_heads.Ghost.ghost depth arg_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x;
      allocated_def h.Ghost.ghost depth argument var;
      A.allocate_runtime h.Ghost.ghost heads.Ghost.ghost arg_heads.Ghost.ghost depth pool argument var valid.Ghost.ghost (refine_ arg_valid.Ghost.ghost) x (); ())} in
  ghost_ (cell_def var depth; let v = cell var depth in payload_scoped_def arg_heap.Ghost.ghost v; ());
  ghost_ (C.scoped_runtime arg_heap.Ghost.ghost routing arg_heads.Ghost.ghost depth arg_pool arg_facts.Ghost.ghost (); ());
  let refine_ result_allocation = Effective_allocator.allocate arg_heap depth var physical (refine_ state) in
  let result_allocation_virtual = ghost_ (Entry (result_allocation.#value, arg_pool)) in
  ghost_ (allocated_def arg_heap.Ghost.ghost depth result_allocation.#value var);
  ghost_ (C.allocation_pool arg_heap.Ghost.ghost depth result_allocation.#value var arg_pool ());
  let refine_ routing = ghost_ (C.allocated arg_heap.Ghost.ghost routing result_allocation.#value var ()) in
  let physical = result_allocation.#pool in
  ghost_ (allocated_def arg_heap.Ghost.ghost depth result_allocation.#value var);
  let result_node = result_allocation.#value in let result_pool = result_allocation_virtual in
  let state = result_allocation.#state in
  let result_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let result_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem result_heap.Ghost.ghost x then finite result_heap.Ghost.ghost t else observe result_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let next = G.allocated_forest arg_heap.Ghost.ghost arg_trees.Ghost.ghost depth result_node var () in
      let refine_ t = next x in refine_ t)} in
  let[@def] result_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select result_heap.Ghost.ghost result_trees.Ghost.ghost x in r) in
  let result_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ result_heads_selected} in
  let result_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head result_heap.Ghost.ghost result_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> result_heads_selected_def x;
      let refine_ r = Forest_heads.select result_heap.Ghost.ghost result_trees.Ghost.ghost x in
      E.valid_head_def result_heap.Ghost.ghost result_heads.Ghost.ghost x; ())} in
  let result_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at result_heap.Ghost.ghost result_heads.Ghost.ghost depth result_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> arg_facts.Ghost.ghost x;
      allocated_def arg_heap.Ghost.ghost depth result_node var;
      A.allocate_runtime arg_heap.Ghost.ghost arg_heads.Ghost.ghost result_heads.Ghost.ghost depth arg_pool result_node var arg_valid.Ghost.ghost (refine_ result_valid.Ghost.ghost) x (); ())} in
  ghost_ (arg_valid.Ghost.ghost argument; A.allocated_below h.Ghost.ghost depth argument var arg_heads.Ghost.ghost ();
    E.effective_below_def arg_heap.Ghost.ghost arg_heads.Ghost.ghost argument depth; ());
  ghost_ (result_valid.Ghost.ghost result_node; A.allocated_below arg_heap.Ghost.ghost depth result_node var result_heads.Ghost.ghost ();
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost result_node depth; ());
  ghost_ (result_valid.Ghost.ghost argument; let v = cell var depth in A.saved_below arg_heap.Ghost.ghost arg_heads.Ghost.ghost result_heads.Ghost.ghost result_node v argument depth ();
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost argument depth; ());
  let desc = Arrow (argument, result_node) in
  ghost_ (cell_def desc depth; let v = cell desc depth in payload_scoped_def result_heap.Ghost.ghost v; ());
  ghost_ (C.scoped_runtime result_heap.Ghost.ghost routing result_heads.Ghost.ghost depth result_pool result_facts.Ghost.ghost (); ());
  let refine_ self_allocation = Effective_allocator.allocate result_heap depth desc physical (refine_ state) in
  let self_allocation_virtual = ghost_ (Entry (self_allocation.#value, result_pool)) in
  ghost_ (allocated_def result_heap.Ghost.ghost depth self_allocation.#value desc);
  ghost_ (C.allocation_pool result_heap.Ghost.ghost depth self_allocation.#value desc result_pool ());
  let refine_ routing = ghost_ (C.allocated result_heap.Ghost.ghost routing self_allocation.#value desc ()) in
  let physical = self_allocation.#pool in
  ghost_ (allocated_def result_heap.Ghost.ghost depth self_allocation.#value desc);
  let self_node = self_allocation.#value in let self_pool = self_allocation_virtual in
  let state = self_allocation.#state in
  let self_heap : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let self_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem self_heap.Ghost.ghost x then finite self_heap.Ghost.ghost t else observe self_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let next = G.allocated_forest result_heap.Ghost.ghost result_trees.Ghost.ghost depth self_node desc () in
      let refine_ t = next x in refine_ t)} in
  let[@def] self_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select self_heap.Ghost.ghost self_trees.Ghost.ghost x in r) in
  let self_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ self_heads_selected} in
  let self_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head self_heap.Ghost.ghost self_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> self_heads_selected_def x;
      let refine_ r = Forest_heads.select self_heap.Ghost.ghost self_trees.Ghost.ghost x in
      E.valid_head_def self_heap.Ghost.ghost self_heads.Ghost.ghost x; ())} in
  let self_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at self_heap.Ghost.ghost self_heads.Ghost.ghost depth self_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> result_facts.Ghost.ghost x;
      allocated_def result_heap.Ghost.ghost depth self_node desc;
      A.allocate_runtime result_heap.Ghost.ghost result_heads.Ghost.ghost self_heads.Ghost.ghost depth result_pool self_node desc result_valid.Ghost.ghost (refine_ self_valid.Ghost.ghost) x (); ())} in
  let env_input : {f : F.forest | F.valid_forest f} = refine_ runtime_env in
  let refine_ with_self = F.cons self_node env_input in
  let env_input : {f : F.forest | F.valid_forest f} = refine_ with_self in
  let refine_ next_runtime_env = F.cons argument env_input in
  let next_env = ghost_ (Bind (argument, Bind (self_node, env))) in
  ghost_ (let v = cell var depth in
    Dp.allocation_env h.Ghost.ghost argument v env ();
    Dp.allocation_env arg_heap.Ghost.ghost result_node v env ();
    let w = cell desc depth in Dp.allocation_env result_heap.Ghost.ghost self_node w env ();
    Copy_heap_proofs.put_frame h.Ghost.ghost argument v argument;
    Copy_heap_proofs.put_frame arg_heap.Ghost.ghost result_node v argument;
    Copy_heap_proofs.put_frame result_heap.Ghost.ghost self_node w argument;
    Copy_heap_proofs.put_frame result_heap.Ghost.ghost self_node w self_node;
    let inner = Bind (self_node, env) in env_owned_def self_heap.Ghost.ghost inner; env_depth_def inner;
    env_owned_def self_heap.Ghost.ghost next_env; env_depth_def next_env);
  let call_pool_3 : pool Ghost.t = {Ghost.ghost = ghost_ self_pool} in
  let resume : (answer : {r : inference | ran self_heap.Ghost.ghost depth call_pool_3.Ghost.ghost (F.flatten next_runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_body && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun answer ->
    let physical = answer.#physical in let pools = answer.#pools in
    let routing = ghost_ answer.#routing in
  let body_trace = answer.#trace in
  let body_run = ghost_ answer.#execution in let body_pool = answer.#pool in let state = answer.#state in
  let middle : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RRec (argument, result_node, self_node, body_run, middle.Ghost.ghost, body_pool, Aborted)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after body_pool;
    source_def execution; result_def execution);
  let trace = Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = None; state = state; pool = body_pool; trace; execution; physical; pools; routing} in use (refine_ out)
  | Some target ->
  let body_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem middle.Ghost.ghost x then finite middle.Ghost.ghost t else observe middle.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let refine_ t = G.run_forest self_heap.Ghost.ghost self_trees.Ghost.ghost depth self_pool next_env body_run middle.Ghost.ghost body_pool x () in refine_ t)} in
  let[@def] body_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select middle.Ghost.ghost body_trees.Ghost.ghost x in r) in
  let body_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ body_heads_selected} in
  let body_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle.Ghost.ghost body_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> body_heads_selected_def x;
      let refine_ r = Forest_heads.select middle.Ghost.ghost body_trees.Ghost.ghost x in
      E.valid_head_def middle.Ghost.ghost body_heads.Ghost.ghost x; ())} in
  let body_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle.Ghost.ghost body_heads.Ghost.ghost depth body_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> Hm_effective_invariant.run_invariant self_heap.Ghost.ghost self_heads.Ghost.ghost self_trees.Ghost.ghost depth self_pool self_facts.Ghost.ghost next_env body_run middle.Ghost.ghost body_heads.Ghost.ghost body_valid.Ghost.ghost body_pool x (); ())} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe middle.Ghost.ghost body_heads.Ghost.ghost x}) @ total = fun x ->
      body_facts.Ghost.ghost x; runtime_at_def middle.Ghost.ghost body_heads.Ghost.ghost depth body_pool x; () in
    Dp.run_pool_scoped self_heap.Ghost.ghost depth self_pool next_env body_run middle.Ghost.ghost body_heads.Ghost.ghost safe body_pool ();
    Dp.run_env_owned self_heap.Ghost.ghost depth self_pool next_env body_run middle.Ghost.ghost body_pool (); ());
  ghost_ (body_facts.Ghost.ghost target; Hm_effective_result.result_below self_heap.Ghost.ghost self_trees.Ghost.ghost depth self_pool next_env body_run middle.Ghost.ghost body_pool body_heads.Ghost.ghost target ();
    E.effective_below_def middle.Ghost.ghost body_heads.Ghost.ghost target depth; E.effective_active_def middle.Ghost.ghost body_heads.Ghost.ghost target; ());
  ghost_ (result_valid.Ghost.ghost result_node; A.allocated_below arg_heap.Ghost.ghost depth result_node var result_heads.Ghost.ghost ();
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost result_node depth; ());
  ghost_ (self_valid.Ghost.ghost result_node; let w = cell desc depth in A.saved_below result_heap.Ghost.ghost result_heads.Ghost.ghost self_heads.Ghost.ghost self_node w result_node depth ();
    body_valid.Ghost.ghost result_node;
    Hm_effective_paths.run_below self_heap.Ghost.ghost self_heads.Ghost.ghost body_heads.Ghost.ghost depth self_pool next_env body_run middle.Ghost.ghost body_pool result_node depth ();
    E.effective_below_def middle.Ghost.ghost body_heads.Ghost.ghost result_node depth; E.effective_active_def middle.Ghost.ghost body_heads.Ghost.ghost result_node; ());
  let d : int Ghost.t = {Ghost.ghost = ghost_ depth} in let pool_proof : pool Ghost.t = {Ghost.ghost = ghost_ body_pool} in
  let unify_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle.Ghost.ghost body_heads.Ghost.ghost d.Ghost.ghost pool_proof.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (refine_ body_facts.Ghost.ghost)} in
  let refine_ solved = Effective_hm_unify.unify middle body_heads d pool_proof unify_facts body_trees target result_node (refine_ state) in
  let after_unify = ghost_ (Pref.own (borrow_ solved.#state)) in
  let refine_ routing = ghost_ (C.unified middle.Ghost.ghost routing target result_node solved.#ok after_unify solved.#derivation ()) in
  let value = if solved.#ok then Some self_node else None in
  let execution = ghost_ (RRec (argument, result_node, self_node, body_run, middle.Ghost.ghost, body_pool, Unified (solved.#ok, solved.#derivation))) in
  let after = ghost_ (Pref.own (borrow_ solved.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after body_pool;
    source_def execution; result_def execution);
  let trace = if collect_trace then Annotation.Recursion (value, argument, result_node, body_trace) else Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = value; state = solved.#state; pool = body_pool; trace; execution; physical; pools; routing} in use (refine_ out)
  in
  let call_facts_3 : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at self_heap.Ghost.ghost self_heads.Ghost.ghost depth call_pool_3.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> self_facts.Ghost.ghost x; ())} in
  work collect_trace goal self_heap self_heads self_trees depth call_pool_3 call_facts_3 next_runtime_env runtime_body physical pools routing (refine_ state) resume
  | T.Let (runtime_rhs, runtime_body) ->
    let child_depth = depth + 1 in
    if child_depth < 0 then
    raise_any (Failure "type inference level capacity") else (
    ghost_ (let limit = Iarray.length goal.origin.S.buckets in
      K.fits_def runtime_rhs child_depth limit;
      C.capacity h.Ghost.ghost routing ());
    let saved_pools : {a : pool B.t | 0 <= depth && depth < Iarray.length (B.contents a)} = refine_ pools in
    let refine_ pools = Rp.save depth physical saved_pools in
    let refine_ routing = ghost_ (C.entered h.Ghost.ghost routing ()) in
    let physical : pool = Empty in
    let child_pool : pool = Empty in
    ghost_ (pool_scoped_def h.Ghost.ghost child_pool);
    let child_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost child_depth child_pool x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; enter_runtime h.Ghost.ghost heads.Ghost.ghost depth pool x (); ())} in
  let call_pool_5 : pool Ghost.t = {Ghost.ghost = ghost_ child_pool} in
  let resume_rhs : (answer : {r : inference | ran h.Ghost.ghost child_depth call_pool_5.Ghost.ghost (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_rhs && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = child_depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun answer ->
    let physical = answer.#physical in let pools = answer.#pools in
    let routing = ghost_ answer.#routing in
  let rhs_trace = answer.#trace in
  let rhs_run = ghost_ answer.#execution in let rhs_pool = answer.#pool in let state = answer.#state in
  let middle : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RLet_left (rhs_run, T.source runtime_body)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after rhs_pool;
    source_def execution; result_def execution);
  let trace = Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = None; state = state; pool = rhs_pool; trace; execution; physical; pools; routing} in use (refine_ out)
  | Some bound_node ->
  let rhs_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem middle.Ghost.ghost x then finite middle.Ghost.ghost t else observe middle.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let refine_ t = G.run_forest h.Ghost.ghost trees.Ghost.ghost child_depth child_pool env rhs_run middle.Ghost.ghost rhs_pool x () in refine_ t)} in
  let[@def] rhs_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select middle.Ghost.ghost rhs_trees.Ghost.ghost x in r) in
  let rhs_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ rhs_heads_selected} in
  let rhs_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle.Ghost.ghost rhs_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> rhs_heads_selected_def x;
      let refine_ r = Forest_heads.select middle.Ghost.ghost rhs_trees.Ghost.ghost x in
      E.valid_head_def middle.Ghost.ghost rhs_heads.Ghost.ghost x; ())} in
  let rhs_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle.Ghost.ghost rhs_heads.Ghost.ghost child_depth rhs_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> Hm_effective_invariant.run_invariant h.Ghost.ghost heads.Ghost.ghost trees.Ghost.ghost child_depth child_pool child_facts.Ghost.ghost env rhs_run middle.Ghost.ghost rhs_heads.Ghost.ghost rhs_valid.Ghost.ghost rhs_pool x (); ())} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe middle.Ghost.ghost rhs_heads.Ghost.ghost x}) @ total = fun x ->
      rhs_facts.Ghost.ghost x; runtime_at_def middle.Ghost.ghost rhs_heads.Ghost.ghost child_depth rhs_pool x; () in
    Dp.run_pool_scoped h.Ghost.ghost child_depth child_pool env rhs_run middle.Ghost.ghost rhs_heads.Ghost.ghost safe rhs_pool ();
    Dp.run_env_owned h.Ghost.ghost child_depth child_pool env rhs_run middle.Ghost.ghost rhs_pool (); ());
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe middle.Ghost.ghost rhs_heads.Ghost.ghost x}) @ total = fun x ->
      rhs_facts.Ghost.ghost x; runtime_at_def middle.Ghost.ghost rhs_heads.Ghost.ghost child_depth rhs_pool x; () in
    Dp.run_saved_pool h.Ghost.ghost child_depth child_pool env rhs_run middle.Ghost.ghost rhs_heads.Ghost.ghost safe rhs_pool pool ();
    Dp.run_result h.Ghost.ghost trees.Ghost.ghost child_depth child_pool env rhs_run middle.Ghost.ghost rhs_pool bound_node (); ());
  ghost_ (C.capacity middle.Ghost.ghost routing ();
    C.scoped_runtime middle.Ghost.ghost routing rhs_heads.Ghost.ghost child_depth rhs_pool rhs_facts.Ghost.ghost (); ());
  ghost_ (
    let[@def] positions : S.locations = fun x ->
      let refine_ i = C.position middle.Ghost.ghost routing x () in i in
    let located : ((x : node Pref.t) @ immutable -> {u : unit | S.located middle.Ghost.ghost routing.store positions x}) @ total = fun x ->
      positions_def x; let refine_ i = C.position middle.Ghost.ghost routing x () in
      S.location_at middle.Ghost.ghost routing.store positions x; () in
    let logical : ((x : node Pref.t) @ immutable -> {u : unit | R.representative_covered middle.Ghost.ghost (routing.store.S.depth - 1) rhs_pool x}) @ total = fun x ->
      rhs_facts.Ghost.ghost x; runtime_at_def middle.Ghost.ghost rhs_heads.Ghost.ghost child_depth rhs_pool x; () in
    S.same_closing middle.Ghost.ghost routing.store positions rhs_pool located logical (); ());
  let input_pools : {a : pool B.t | 0 <= depth && depth < Iarray.length (B.contents a)} = refine_ pools in
  let refine_ closed = Rp.close_and_route middle depth physical (refine_ state) input_pools in
  let state = closed.#state in
  ghost_ (let after = Pref.own (borrow_ state) in let empty : pool = Empty in
    let retained = Representative_pool_spec.transfer_rep after physical empty in
    Level_pool_routing_spec.route_length after retained routing.store.S.buckets; ());
  let input_pools : {a : pool B.t | 0 <= depth && depth < Iarray.length (B.contents a)} = refine_ closed.#pools in
  let refine_ taken = Rp.take depth input_pools in
  let physical = taken.#pending in let pools = taken.#pools in
  let before_close = ghost_ routing in
  let refine_ routing = ghost_ (C.left middle.Ghost.ghost before_close ()) in
  ghost_ (S.closed_store_def middle.Ghost.ghost before_close.store);
  ghost_ (Pool_closing_equivalence.closed_transfer_scoped middle.Ghost.ghost depth rhs_pool pool ());
  let parent_pool = ghost_ (Representative_pool_spec.transfer_rep (Representative_pool_spec.close_heap middle.Ghost.ghost depth rhs_pool) rhs_pool pool) in
  let start : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let parent_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem start.Ghost.ghost x then finite start.Ghost.ghost t else observe start.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let refine_ t = G.representative_closed_forest middle.Ghost.ghost rhs_trees.Ghost.ghost depth rhs_pool x () in refine_ t)} in
  let parent_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at start.Ghost.ghost rhs_heads.Ghost.ghost depth parent_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> Hm_effective_closing.close_after_run h.Ghost.ghost heads.Ghost.ghost rhs_heads.Ghost.ghost depth pool facts.Ghost.ghost env rhs_run middle.Ghost.ghost rhs_pool (refine_ rhs_facts.Ghost.ghost) x (); ())} in
  let env_input : {f : F.forest | F.valid_forest f} = refine_ runtime_env in
  let refine_ next_runtime_env = F.cons bound_node env_input in
  let next_env = ghost_ (Bind (bound_node, env)) in
  ghost_ (
    let frame : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle.Ghost.ghost x) || H.mem start.Ghost.ghost x}) @ total = fun x ->
      Representative_pool_spec.close_heap_def middle.Ghost.ghost depth rhs_pool;
      R.representatives_scoped middle.Ghost.ghost rhs_pool ();
      let filtered = R.representatives middle.Ghost.ghost rhs_pool in
      Generalize_proofs.closed_observe middle.Ghost.ghost depth filtered x ();
      closed_at_def middle.Ghost.ghost start.Ghost.ghost depth filtered x; () in
    Dp.env_extend middle.Ghost.ghost start.Ghost.ghost frame env (); frame bound_node;
    env_owned_def start.Ghost.ghost next_env; env_depth_def next_env);
  let call_pool_4 : pool Ghost.t = {Ghost.ghost = ghost_ parent_pool} in
  let resume_body : (answer : {r : inference | ran start.Ghost.ghost depth call_pool_4.Ghost.ghost (F.flatten next_runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_body && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = depth)}) @ unique -> {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun answer ->
    let physical = answer.#physical in let pools = answer.#pools in
    let routing = ghost_ answer.#routing in
  let execution = ghost_ (RLet (rhs_run, answer.#execution, middle.Ghost.ghost, rhs_pool)) in
  let after = ghost_ (Pref.own (borrow_ answer.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after answer.#pool;
    source_def execution; result_def execution);
  let trace = if collect_trace then Annotation.Let_binding (rhs_trace, answer.#trace) else Annotation.Failed in
  ghost_ (Hm_annotation_trace_spec.records_def trace execution);
  let out = #{value = answer.#value; state = answer.#state; pool = answer.#pool; trace; execution; physical; pools; routing} in use (refine_ out)
  in
  let call_facts_4 : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at start.Ghost.ghost rhs_heads.Ghost.ghost depth call_pool_4.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> parent_facts.Ghost.ghost x; ())} in
  work collect_trace goal start rhs_heads parent_trees depth call_pool_4 call_facts_4 next_runtime_env runtime_body physical pools routing (refine_ state) resume_body
  in
  let call_facts_5 : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost child_depth call_pool_5.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> child_facts.Ghost.ghost x; ())} in
  work collect_trace goal h heads trees child_depth call_pool_5 call_facts_5 runtime_env runtime_rhs physical pools routing (refine_ state) resume_rhs
  )

let closed_compiled_with_trace : (collect_trace : bool) ->
    (input : {e : T.term | T.valid e && D.scoped_term D.Z (T.source e)}) @ immutable ->
    {r : inference |
      ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty
        r.#execution (Pref.own r.#state) r.#pool
      && source r.#execution === T.source input && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique =
  fun collect_trace input ->
    let refine_ pools = K.create input in
    let values = ghost_ (B.contents (borrow_ pools)) in
    let physical : pool = Empty in
    let origin = ghost_ {S.depth = 0; pending = physical; buckets = values} in
    let routing = {C.origin = ghost_ origin; store = ghost_ origin; execution = ghost_ Level_pool_execution.Idle} in
    let refine_ state = Pref.empty () in
    let h : node Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
    let heads : E.heads Ghost.t = {Ghost.ghost = ghost_ (fun x -> {R.root = x; path = Here})} in
    let trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> let t = Free x in tree_root_def t; observe_def h.Ghost.ghost x; refine_ t)} in
    let pool : pool = Empty in
    let facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost 0 pool x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> runtime_at_def h.Ghost.ghost heads.Ghost.ghost 0 pool x;
        safe_def h.Ghost.ghost heads.Ghost.ghost x; depth_bound_def h.Ghost.ghost heads.Ghost.ghost 0 x;
        E.valid_head_def h.Ghost.ghost heads.Ghost.ghost x; E.effective_ordered_def h.Ghost.ghost heads.Ghost.ghost x;
        R.representative_covered_def h.Ghost.ghost (-1) pool x; terminal_def h.Ghost.ghost x; observe_def h.Ghost.ghost x;
        ())} in
    let runtime_env = F.Nil in let env : env = Hm_environment_spec.Empty in
    ghost_ (F.valid_forest_def runtime_env; F.flatten_def runtime_env;
      pool_scoped_def h.Ghost.ghost pool; env_owned_def h.Ghost.ghost env; env_depth_def env);
    let goal = {heap = ghost_ h.Ghost.ghost; depth = ghost_ 0; pool = ghost_ pool;
      env = ghost_ env; term = ghost_ (T.source input); origin = ghost_ origin} in
    let call_pool_6 : pool Ghost.t = {Ghost.ghost = ghost_ pool} in
  let use : (r : {r : inference | ran h.Ghost.ghost 0 call_pool_6.Ghost.ghost (F.flatten runtime_env)
        r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source input
        && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed) && C.recorded (Pref.own r.#state) r.#routing && r.#routing.origin === goal.origin && r.#routing.store.S.pending === r.#physical && r.#routing.store.S.buckets === B.contents r.#pools && (r.#value === None || r.#routing.store.S.depth = 0)}) @ unique ->
      {r : inference | completed goal (Pref.own r.#state) r.#pool r.#execution r.#value r.#routing r.#physical (B.contents r.#pools) && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique = fun r ->
      let value = r.#value in let physical = r.#physical in
      ghost_ (completed_def goal (Pref.own (borrow_ r.#state)) r.#pool
        r.#execution value r.#routing physical (B.contents (borrow_ r.#pools)));
      #{value; physical; trace = r.#trace; state = r.#state; pools = r.#pools;
        pool = r.#pool; execution = r.#execution; routing = r.#routing} in
    ghost_ (let empty = H.empty () in let idle = Level_pool_execution.Idle in
      Level_pool_execution.ran_def empty origin idle empty origin;
      C.recorded_def h.Ghost.ghost routing);
    let call_facts_6 : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost 0 call_pool_6.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; ())} in
  let refine_ out = work collect_trace goal h heads trees 0 call_pool_6 call_facts_6 runtime_env input physical pools routing (refine_ state) use in
  let value = out.#value in let physical = out.#physical in
  ghost_ (completed_def goal (Pref.own (borrow_ out.#state)) out.#pool
    out.#execution value out.#routing physical (B.contents (borrow_ out.#pools)));
  #{value; physical; trace = out.#trace; state = out.#state; pools = out.#pools;
    pool = out.#pool; execution = out.#execution; routing = out.#routing}

type answer = #{value : node Pref.t option @@ aliased; state : node Pref.token;
  trace : Hm_annotation_trace.trace @@ aliased; pool : pool @@ ghost; execution : execution @@ ghost}

let closed_hm_with_trace : (collect_trace : bool) -> (e : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
    {r : answer |
      ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty
        r.#execution (Pref.own r.#state) r.#pool
      && source r.#execution === e && r.#value === result r.#execution && (if collect_trace then Hm_annotation_trace_spec.records r.#trace r.#execution else r.#trace === Annotation.Failed)} @ unique =
  fun collect_trace e ->
    let refine_ input = T.compile e in
    let input : {e : T.term | T.valid e && D.scoped_term D.Z (T.source e)} = refine_ input in
    let refine_ out = closed_compiled_with_trace collect_trace input in let answer = #{value = out.#value; trace = out.#trace; state = out.#state;
      pool = ghost_ out.#pool; execution = ghost_ out.#execution} in
    refine_ answer

let closed_compiled :
    (input : {e : T.term | T.valid e && D.scoped_term D.Z (T.source e)}) @ immutable ->
    {r : inference |
      ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty
        r.#execution (Pref.own r.#state) r.#pool
      && source r.#execution === T.source input && r.#value === result r.#execution && Hm_annotation_trace_spec.records r.#trace r.#execution} @ unique =
  fun input -> closed_compiled_with_trace true input

let closed_hm : (e : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
    {r : answer |
      ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty
        r.#execution (Pref.own r.#state) r.#pool
      && source r.#execution === e && r.#value === result r.#execution && Hm_annotation_trace_spec.records r.#trace r.#execution} @ unique =
  fun e -> closed_hm_with_trace true e
