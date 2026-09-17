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

type goal = {heap : Pref.heap @@ ghost; depth : int @@ ghost; pool : pool @@ ghost;
  env : env @@ ghost; term : D.term @@ ghost}

let rec work : (goal : goal) @ immutable -> (h : Pref.heap Ghost.t) @ immutable ->
    (heads : E.heads Ghost.t) @ total ->
    (trees : (((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h.Ghost.ghost x then finite h.Ghost.ghost t else observe h.Ghost.ghost x === None)} @ immutable)) Ghost.t) @ total ->
    (depth : int) -> (pool : pool) @ immutable ->
    (facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost depth pool x})) Ghost.t) @ total ->
    (runtime_env : F.forest) @ immutable -> (runtime_term : T.term) @ immutable ->
    (state : {t : Pref.token | Pref.own t === h.Ghost.ghost && depth >= 0 && pool_scoped h.Ghost.ghost pool
      && F.valid_forest runtime_env && T.valid runtime_term && env_owned h.Ghost.ghost (F.flatten runtime_env)
      && D.scoped_term (env_depth (F.flatten runtime_env)) (T.source runtime_term)}) @ unique ->
    (use : ((r : {r : inference | ran h.Ghost.ghost depth pool (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_term && r.#value === result r.#execution}) @ unique -> {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool && source r.#execution === goal.term && r.#value === result r.#execution} @ unique)) ->
    {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun goal h heads trees depth pool facts runtime_env runtime_term state use ->
  let refine_ state = state in
  let env = ghost_ (F.flatten runtime_env) in let term = ghost_ (T.source runtime_term) in
  ghost_ (T.valid_def runtime_term; T.source_def runtime_term; let n = env_depth env in D.scoped_term_def n term; ());
  let valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h.Ghost.ghost heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; runtime_at_def h.Ghost.ghost heads.Ghost.ghost depth pool x;
      safe_def h.Ghost.ghost heads.Ghost.ghost x; let u = () in refine_ u)} in
  match runtime_term with
  | T.Bound index ->
    let i = ghost_ index.T.original in
    ghost_ (let u = () in Dp.env_lookup_owned h.Ghost.ghost env i (refine_ u);
      F.lookup_encoded env i index.T.number; ());
    let premise : ({u : unit | F.valid_forest runtime_env && index.T.number >= 0}) Ghost.t = {Ghost.ghost = ghost_ (refine_ ())} in
    let refine_ found = F.lookup runtime_env index.T.number premise in
    let p : {p : node Pref.t | lookup env i === Some p && H.mem h.Ghost.ghost p} = match found with
      | Some p -> refine_ p | None -> ghost_ (let impossible : {u : unit | false} = refine_ () in let refine_ impossible = impossible in ()); assert false in
    let refine_ p = p in
    let c = {Effective_copy_runtime.saved = ghost_ h.Ghost.ghost; epoch = ghost_ p;
      depth = ghost_ depth; base = ghost_ pool} in
    let scope : (((x : node Pref.t) @ immutable -> {u : unit | not (H.mem c.Effective_copy_runtime.saved x) || source_ok c.Effective_copy_runtime.saved x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; runtime_at_def h.Ghost.ghost heads.Ghost.ghost depth pool x;
        safe_def h.Ghost.ghost heads.Ghost.ghost x; let u = () in refine_ u)} in
    let clean : (((x : node Pref.t) @ immutable -> {u : unit | match H.at c.Effective_copy_runtime.saved x with None -> true | Some v -> v.memo === Empty_memo})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; runtime_at_def h.Ghost.ghost heads.Ghost.ghost depth pool x;
        safe_def h.Ghost.ghost heads.Ghost.ghost x; let u = () in refine_ u)} in
    let valid_copy : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head c.Effective_copy_runtime.saved heads.Ghost.ghost x})) Ghost.t = {Ghost.ghost = ghost_ (refine_ valid.Ghost.ghost)} in
    let refine_ copied = Certified_copy.instantiate c heads scope clean valid_copy (refine_ depth) pool p (refine_ state) in
  let execution = ghost_ (RVar (i, copied.#value, copied.#epoch, copied.#history, copied.#certificate)) in
  let after = ghost_ (Pref.own (borrow_ copied.#state)) in
  ghost_ (copy_heap_def h.Ghost.ghost copied.#epoch depth copied.#history; ran_def h.Ghost.ghost depth pool env execution after copied.#pool;
    source_def execution; result_def execution);
  let out = #{value = Some copied.#value; state = copied.#state; pool = copied.#pool; execution} in use (refine_ out)
  | T.Truth ->
    let desc = Bool in
    ghost_ (cell_def desc depth; let v = cell desc depth in payload_scoped_def h.Ghost.ghost v; ());
    let refine_ allocated = Effective_allocator.allocate h depth desc pool (refine_ state) in
  ghost_ (allocated_def h.Ghost.ghost depth allocated.#value desc);
  let execution = ghost_ (RBool allocated.#value) in
  let after = ghost_ (Pref.own (borrow_ allocated.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after allocated.#pool;
    source_def execution; result_def execution);
  let out = #{value = Some allocated.#value; state = allocated.#state; pool = allocated.#pool; execution} in use (refine_ out)
  | T.Lambda runtime_body ->
    let var = Var in
  ghost_ (cell_def var depth; let v = cell var depth in payload_scoped_def h.Ghost.ghost v; ());
  let refine_ arg_allocation = Effective_allocator.allocate h depth var pool (refine_ state) in
  ghost_ (allocated_def h.Ghost.ghost depth arg_allocation.#value var);
  let argument = arg_allocation.#value in let arg_pool = arg_allocation.#pool in
  let state = arg_allocation.#state in
  let arg_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let arg_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem arg_heap.Ghost.ghost x then finite arg_heap.Ghost.ghost t else observe arg_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      let next = G.allocated_forest h.Ghost.ghost trees.Ghost.ghost depth argument var (refine_ u) in
      let refine_ t = next x in refine_ t)} in
  let[@def] arg_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select arg_heap.Ghost.ghost arg_trees.Ghost.ghost x in r) in
  let arg_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ arg_heads_selected} in
  let arg_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head arg_heap.Ghost.ghost arg_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> arg_heads_selected_def x;
      let refine_ r = Forest_heads.select arg_heap.Ghost.ghost arg_trees.Ghost.ghost x in
      E.valid_head_def arg_heap.Ghost.ghost arg_heads.Ghost.ghost x; let u = () in refine_ u)} in
  let arg_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at arg_heap.Ghost.ghost arg_heads.Ghost.ghost depth arg_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x;
      allocated_def h.Ghost.ghost depth argument var;
      let u = () in A.allocate_runtime h.Ghost.ghost heads.Ghost.ghost arg_heads.Ghost.ghost depth pool argument var valid.Ghost.ghost (refine_ arg_valid.Ghost.ghost) x (refine_ u); refine_ u)} in
  let env_input : {f : F.forest | F.valid_forest f} = refine_ runtime_env in
  let refine_ next_runtime_env = F.cons argument env_input in
  let refine_ env_input = env_input in
  let next_env = ghost_ (Bind (argument, env)) in
  ghost_ (let v = cell var depth in let u = () in Dp.allocation_env h.Ghost.ghost argument v env (refine_ u);
    Copy_heap_proofs.put_frame h.Ghost.ghost argument v argument;
    env_owned_def arg_heap.Ghost.ghost next_env; env_depth_def next_env);
  let resume : (answer : {r : inference | ran arg_heap.Ghost.ghost depth arg_pool (F.flatten next_runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_body && r.#value === result r.#execution}) @ unique -> {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun answer ->
    let refine_ answer = answer in
  let body_run = ghost_ answer.#execution in let body_pool = answer.#pool in let state = answer.#state in
  let middle : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RLam (argument, body_run, middle.Ghost.ghost, body_pool, None)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after body_pool;
    source_def execution; result_def execution);
  let out = #{value = None; state = state; pool = body_pool; execution} in use (refine_ out)
  | Some target ->
  let body_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem middle.Ghost.ghost x then finite middle.Ghost.ghost t else observe middle.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      let refine_ t = G.run_forest arg_heap.Ghost.ghost arg_trees.Ghost.ghost depth arg_pool next_env body_run middle.Ghost.ghost body_pool x (refine_ u) in refine_ t)} in
  let[@def] body_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select middle.Ghost.ghost body_trees.Ghost.ghost x in r) in
  let body_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ body_heads_selected} in
  let body_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle.Ghost.ghost body_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> body_heads_selected_def x;
      let refine_ r = Forest_heads.select middle.Ghost.ghost body_trees.Ghost.ghost x in
      E.valid_head_def middle.Ghost.ghost body_heads.Ghost.ghost x; let u = () in refine_ u)} in
  let body_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle.Ghost.ghost body_heads.Ghost.ghost depth body_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      Hm_effective_invariant.run_invariant arg_heap.Ghost.ghost arg_heads.Ghost.ghost arg_trees.Ghost.ghost depth arg_pool arg_facts.Ghost.ghost next_env body_run middle.Ghost.ghost body_heads.Ghost.ghost body_valid.Ghost.ghost body_pool x (refine_ u); refine_ u)} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe middle.Ghost.ghost body_heads.Ghost.ghost x}) @ total = fun x ->
      body_facts.Ghost.ghost x; runtime_at_def middle.Ghost.ghost body_heads.Ghost.ghost depth body_pool x; let u = () in refine_ u in
    let u = () in Dp.run_pool_scoped arg_heap.Ghost.ghost depth arg_pool next_env body_run middle.Ghost.ghost body_heads.Ghost.ghost safe body_pool (refine_ u);
    Dp.run_env_owned arg_heap.Ghost.ghost depth arg_pool next_env body_run middle.Ghost.ghost body_pool (refine_ u); ());
  ghost_ (let v = cell var depth in Copy_heap_proofs.put_frame h.Ghost.ghost argument v argument;
    let u = () in Hm_effective_membership.run_extends arg_heap.Ghost.ghost depth arg_pool next_env body_run middle.Ghost.ghost body_pool argument (refine_ u);
    Dp.run_result arg_heap.Ghost.ghost arg_trees.Ghost.ghost depth arg_pool next_env body_run middle.Ghost.ghost body_pool target (refine_ u); ());
  let desc = Arrow (argument, target) in
  ghost_ (cell_def desc depth; let v = cell desc depth in payload_scoped_def middle.Ghost.ghost v; ());
  let refine_ allocated = Effective_allocator.allocate middle depth desc body_pool (refine_ state) in
  ghost_ (allocated_def middle.Ghost.ghost depth allocated.#value desc);
  let execution = ghost_ (RLam (argument, body_run, middle.Ghost.ghost, body_pool, Some allocated.#value)) in
  let after = ghost_ (Pref.own (borrow_ allocated.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after allocated.#pool;
    source_def execution; result_def execution);
  let out = #{value = Some allocated.#value; state = allocated.#state; pool = allocated.#pool; execution} in use (refine_ out)
  in
  work goal arg_heap arg_heads arg_trees depth arg_pool arg_facts next_runtime_env runtime_body (refine_ state) resume
  | T.Apply (runtime_left, runtime_right) ->
  let resume_left : (answer : {r : inference | ran h.Ghost.ghost depth pool (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_left && r.#value === result r.#execution}) @ unique -> {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun answer ->
    let refine_ answer = answer in
  let left_run = ghost_ answer.#execution in let pool1 = answer.#pool in let state = answer.#state in
  let h1 : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RApp_left (left_run, T.source runtime_right)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after pool1;
    source_def execution; result_def execution);
  let out = #{value = None; state = state; pool = pool1; execution} in use (refine_ out)
  | Some fn ->
  let first_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h1.Ghost.ghost x then finite h1.Ghost.ghost t else observe h1.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      let refine_ t = G.run_forest h.Ghost.ghost trees.Ghost.ghost depth pool env left_run h1.Ghost.ghost pool1 x (refine_ u) in refine_ t)} in
  let[@def] first_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select h1.Ghost.ghost first_trees.Ghost.ghost x in r) in
  let first_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ first_heads_selected} in
  let first_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h1.Ghost.ghost first_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> first_heads_selected_def x;
      let refine_ r = Forest_heads.select h1.Ghost.ghost first_trees.Ghost.ghost x in
      E.valid_head_def h1.Ghost.ghost first_heads.Ghost.ghost x; let u = () in refine_ u)} in
  let first_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h1.Ghost.ghost first_heads.Ghost.ghost depth pool1 x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      Hm_effective_invariant.run_invariant h.Ghost.ghost heads.Ghost.ghost trees.Ghost.ghost depth pool facts.Ghost.ghost env left_run h1.Ghost.ghost first_heads.Ghost.ghost first_valid.Ghost.ghost pool1 x (refine_ u); refine_ u)} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe h1.Ghost.ghost first_heads.Ghost.ghost x}) @ total = fun x ->
      first_facts.Ghost.ghost x; runtime_at_def h1.Ghost.ghost first_heads.Ghost.ghost depth pool1 x; let u = () in refine_ u in
    let u = () in Dp.run_pool_scoped h.Ghost.ghost depth pool env left_run h1.Ghost.ghost first_heads.Ghost.ghost safe pool1 (refine_ u);
    Dp.run_env_owned h.Ghost.ghost depth pool env left_run h1.Ghost.ghost pool1 (refine_ u); ());
  let resume_right : (answer : {r : inference | ran h1.Ghost.ghost depth pool1 (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_right && r.#value === result r.#execution}) @ unique -> {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun answer ->
    let refine_ answer = answer in
  let right_run = ghost_ answer.#execution in let pool2 = answer.#pool in let state = answer.#state in
  let h2 : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RApp_right (left_run, right_run, h1.Ghost.ghost, pool1)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after pool2;
    source_def execution; result_def execution);
  let out = #{value = None; state = state; pool = pool2; execution} in use (refine_ out)
  | Some actual ->
  let second_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem h2.Ghost.ghost x then finite h2.Ghost.ghost t else observe h2.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      let refine_ t = G.run_forest h1.Ghost.ghost first_trees.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 x (refine_ u) in refine_ t)} in
  let[@def] second_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select h2.Ghost.ghost second_trees.Ghost.ghost x in r) in
  let second_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ second_heads_selected} in
  let second_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h2.Ghost.ghost second_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> second_heads_selected_def x;
      let refine_ r = Forest_heads.select h2.Ghost.ghost second_trees.Ghost.ghost x in
      E.valid_head_def h2.Ghost.ghost second_heads.Ghost.ghost x; let u = () in refine_ u)} in
  let second_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h2.Ghost.ghost second_heads.Ghost.ghost depth pool2 x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      Hm_effective_invariant.run_invariant h1.Ghost.ghost first_heads.Ghost.ghost first_trees.Ghost.ghost depth pool1 first_facts.Ghost.ghost env right_run h2.Ghost.ghost second_heads.Ghost.ghost second_valid.Ghost.ghost pool2 x (refine_ u); refine_ u)} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe h2.Ghost.ghost second_heads.Ghost.ghost x}) @ total = fun x ->
      second_facts.Ghost.ghost x; runtime_at_def h2.Ghost.ghost second_heads.Ghost.ghost depth pool2 x; let u = () in refine_ u in
    let u = () in Dp.run_pool_scoped h1.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost second_heads.Ghost.ghost safe pool2 (refine_ u);
    Dp.run_env_owned h1.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 (refine_ u); ());
  ghost_ (second_facts.Ghost.ghost actual; let u = () in
    Hm_effective_result.result_below h1.Ghost.ghost first_trees.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 second_heads.Ghost.ghost actual (refine_ u);
    E.effective_below_def h2.Ghost.ghost second_heads.Ghost.ghost actual depth; E.effective_active_def h2.Ghost.ghost second_heads.Ghost.ghost actual; ());
  let var = Var in
  ghost_ (cell_def var depth; let v = cell var depth in payload_scoped_def h2.Ghost.ghost v; ());
  let refine_ result_allocation = Effective_allocator.allocate h2 depth var pool2 (refine_ state) in
  ghost_ (allocated_def h2.Ghost.ghost depth result_allocation.#value var);
  let result_node = result_allocation.#value in let result_pool = result_allocation.#pool in
  let state = result_allocation.#state in
  let result_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let result_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem result_heap.Ghost.ghost x then finite result_heap.Ghost.ghost t else observe result_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      let next = G.allocated_forest h2.Ghost.ghost second_trees.Ghost.ghost depth result_node var (refine_ u) in
      let refine_ t = next x in refine_ t)} in
  let[@def] result_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select result_heap.Ghost.ghost result_trees.Ghost.ghost x in r) in
  let result_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ result_heads_selected} in
  let result_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head result_heap.Ghost.ghost result_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> result_heads_selected_def x;
      let refine_ r = Forest_heads.select result_heap.Ghost.ghost result_trees.Ghost.ghost x in
      E.valid_head_def result_heap.Ghost.ghost result_heads.Ghost.ghost x; let u = () in refine_ u)} in
  let result_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at result_heap.Ghost.ghost result_heads.Ghost.ghost depth result_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> second_facts.Ghost.ghost x;
      allocated_def h2.Ghost.ghost depth result_node var;
      let u = () in A.allocate_runtime h2.Ghost.ghost second_heads.Ghost.ghost result_heads.Ghost.ghost depth pool2 result_node var second_valid.Ghost.ghost (refine_ result_valid.Ghost.ghost) x (refine_ u); refine_ u)} in
  ghost_ (result_valid.Ghost.ghost result_node; let u = () in A.allocated_below h2.Ghost.ghost depth result_node var result_heads.Ghost.ghost (refine_ u);
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost result_node depth; ());
  ghost_ (second_valid.Ghost.ghost actual; result_valid.Ghost.ghost actual;
    let v = cell var depth in let u = () in A.saved_below h2.Ghost.ghost second_heads.Ghost.ghost result_heads.Ghost.ghost result_node v actual depth (refine_ u);
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost actual depth; ());
  let desc = Arrow (actual, result_node) in
  ghost_ (cell_def desc depth; let v = cell desc depth in payload_scoped_def result_heap.Ghost.ghost v; ());
  let refine_ arrow_allocation = Effective_allocator.allocate result_heap depth desc result_pool (refine_ state) in
  ghost_ (allocated_def result_heap.Ghost.ghost depth arrow_allocation.#value desc);
  let arrow_node = arrow_allocation.#value in let arrow_pool = arrow_allocation.#pool in
  let state = arrow_allocation.#state in
  let arrow_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let arrow_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem arrow_heap.Ghost.ghost x then finite arrow_heap.Ghost.ghost t else observe arrow_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      let next = G.allocated_forest result_heap.Ghost.ghost result_trees.Ghost.ghost depth arrow_node desc (refine_ u) in
      let refine_ t = next x in refine_ t)} in
  let[@def] arrow_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select arrow_heap.Ghost.ghost arrow_trees.Ghost.ghost x in r) in
  let arrow_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ arrow_heads_selected} in
  let arrow_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> arrow_heads_selected_def x;
      let refine_ r = Forest_heads.select arrow_heap.Ghost.ghost arrow_trees.Ghost.ghost x in
      E.valid_head_def arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost x; let u = () in refine_ u)} in
  let arrow_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost depth arrow_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> result_facts.Ghost.ghost x;
      allocated_def result_heap.Ghost.ghost depth arrow_node desc;
      let u = () in A.allocate_runtime result_heap.Ghost.ghost result_heads.Ghost.ghost arrow_heads.Ghost.ghost depth result_pool arrow_node desc result_valid.Ghost.ghost (refine_ arrow_valid.Ghost.ghost) x (refine_ u); refine_ u)} in
  ghost_ (first_facts.Ghost.ghost fn; let u = () in
    Hm_effective_result.result_below h.Ghost.ghost trees.Ghost.ghost depth pool env left_run h1.Ghost.ghost pool1 first_heads.Ghost.ghost fn (refine_ u);
    E.effective_below_def h1.Ghost.ghost first_heads.Ghost.ghost fn depth; E.effective_active_def h1.Ghost.ghost first_heads.Ghost.ghost fn; ());
  ghost_ (first_valid.Ghost.ghost fn; second_valid.Ghost.ghost fn; let u = () in Hm_effective_paths.run_below h1.Ghost.ghost first_heads.Ghost.ghost second_heads.Ghost.ghost depth pool1 env right_run h2.Ghost.ghost pool2 fn depth (refine_ u);
    result_valid.Ghost.ghost fn; let v = cell var depth in A.saved_below h2.Ghost.ghost second_heads.Ghost.ghost result_heads.Ghost.ghost result_node v fn depth (refine_ u);
    arrow_valid.Ghost.ghost fn; let w = cell desc depth in A.saved_below result_heap.Ghost.ghost result_heads.Ghost.ghost arrow_heads.Ghost.ghost arrow_node w fn depth (refine_ u);
    E.effective_below_def arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost fn depth; E.effective_active_def arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost fn; ());
  ghost_ (arrow_valid.Ghost.ghost arrow_node; let u = () in A.allocated_below result_heap.Ghost.ghost depth arrow_node desc arrow_heads.Ghost.ghost (refine_ u);
    E.effective_below_def arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost arrow_node depth; ());
  ghost_ (E.effective_active_def arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost arrow_node);
  let d : int Ghost.t = {Ghost.ghost = ghost_ depth} in let pool_proof : pool Ghost.t = {Ghost.ghost = ghost_ arrow_pool} in
  let unify_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at arrow_heap.Ghost.ghost arrow_heads.Ghost.ghost d.Ghost.ghost pool_proof.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (refine_ arrow_facts.Ghost.ghost)} in
  let refine_ solved = Effective_hm_unify.unify arrow_heap arrow_heads d pool_proof unify_facts arrow_trees fn arrow_node (refine_ state) in
  let value = if solved.#ok then Some result_node else None in
  let execution = ghost_ (RApp (left_run, right_run, h1.Ghost.ghost, pool1, h2.Ghost.ghost, pool2, result_node, arrow_node, solved.#ok, solved.#derivation)) in
  let after = ghost_ (Pref.own (borrow_ solved.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after arrow_pool;
    source_def execution; result_def execution);
  let out = #{value = value; state = solved.#state; pool = arrow_pool; execution} in use (refine_ out)
  in
  work goal h1 first_heads first_trees depth pool1 first_facts runtime_env runtime_right (refine_ state) resume_right
  in
  work goal h heads trees depth pool facts runtime_env runtime_left (refine_ state) resume_left
  | T.Recursive runtime_body ->
    let var = Var in
  ghost_ (cell_def var depth; let v = cell var depth in payload_scoped_def h.Ghost.ghost v; ());
  let refine_ arg_allocation = Effective_allocator.allocate h depth var pool (refine_ state) in
  ghost_ (allocated_def h.Ghost.ghost depth arg_allocation.#value var);
  let argument = arg_allocation.#value in let arg_pool = arg_allocation.#pool in
  let state = arg_allocation.#state in
  let arg_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let arg_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem arg_heap.Ghost.ghost x then finite arg_heap.Ghost.ghost t else observe arg_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      let next = G.allocated_forest h.Ghost.ghost trees.Ghost.ghost depth argument var (refine_ u) in
      let refine_ t = next x in refine_ t)} in
  let[@def] arg_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select arg_heap.Ghost.ghost arg_trees.Ghost.ghost x in r) in
  let arg_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ arg_heads_selected} in
  let arg_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head arg_heap.Ghost.ghost arg_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> arg_heads_selected_def x;
      let refine_ r = Forest_heads.select arg_heap.Ghost.ghost arg_trees.Ghost.ghost x in
      E.valid_head_def arg_heap.Ghost.ghost arg_heads.Ghost.ghost x; let u = () in refine_ u)} in
  let arg_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at arg_heap.Ghost.ghost arg_heads.Ghost.ghost depth arg_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x;
      allocated_def h.Ghost.ghost depth argument var;
      let u = () in A.allocate_runtime h.Ghost.ghost heads.Ghost.ghost arg_heads.Ghost.ghost depth pool argument var valid.Ghost.ghost (refine_ arg_valid.Ghost.ghost) x (refine_ u); refine_ u)} in
  ghost_ (cell_def var depth; let v = cell var depth in payload_scoped_def arg_heap.Ghost.ghost v; ());
  let refine_ result_allocation = Effective_allocator.allocate arg_heap depth var arg_pool (refine_ state) in
  ghost_ (allocated_def arg_heap.Ghost.ghost depth result_allocation.#value var);
  let result_node = result_allocation.#value in let result_pool = result_allocation.#pool in
  let state = result_allocation.#state in
  let result_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let result_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem result_heap.Ghost.ghost x then finite result_heap.Ghost.ghost t else observe result_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      let next = G.allocated_forest arg_heap.Ghost.ghost arg_trees.Ghost.ghost depth result_node var (refine_ u) in
      let refine_ t = next x in refine_ t)} in
  let[@def] result_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select result_heap.Ghost.ghost result_trees.Ghost.ghost x in r) in
  let result_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ result_heads_selected} in
  let result_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head result_heap.Ghost.ghost result_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> result_heads_selected_def x;
      let refine_ r = Forest_heads.select result_heap.Ghost.ghost result_trees.Ghost.ghost x in
      E.valid_head_def result_heap.Ghost.ghost result_heads.Ghost.ghost x; let u = () in refine_ u)} in
  let result_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at result_heap.Ghost.ghost result_heads.Ghost.ghost depth result_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> arg_facts.Ghost.ghost x;
      allocated_def arg_heap.Ghost.ghost depth result_node var;
      let u = () in A.allocate_runtime arg_heap.Ghost.ghost arg_heads.Ghost.ghost result_heads.Ghost.ghost depth arg_pool result_node var arg_valid.Ghost.ghost (refine_ result_valid.Ghost.ghost) x (refine_ u); refine_ u)} in
  ghost_ (arg_valid.Ghost.ghost argument; let u = () in A.allocated_below h.Ghost.ghost depth argument var arg_heads.Ghost.ghost (refine_ u);
    E.effective_below_def arg_heap.Ghost.ghost arg_heads.Ghost.ghost argument depth; ());
  ghost_ (result_valid.Ghost.ghost result_node; let u = () in A.allocated_below arg_heap.Ghost.ghost depth result_node var result_heads.Ghost.ghost (refine_ u);
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost result_node depth; ());
  ghost_ (result_valid.Ghost.ghost argument; let v = cell var depth in let u = () in
    A.saved_below arg_heap.Ghost.ghost arg_heads.Ghost.ghost result_heads.Ghost.ghost result_node v argument depth (refine_ u);
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost argument depth; ());
  let desc = Arrow (argument, result_node) in
  ghost_ (cell_def desc depth; let v = cell desc depth in payload_scoped_def result_heap.Ghost.ghost v; ());
  let refine_ self_allocation = Effective_allocator.allocate result_heap depth desc result_pool (refine_ state) in
  ghost_ (allocated_def result_heap.Ghost.ghost depth self_allocation.#value desc);
  let self_node = self_allocation.#value in let self_pool = self_allocation.#pool in
  let state = self_allocation.#state in
  let self_heap : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let self_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem self_heap.Ghost.ghost x then finite self_heap.Ghost.ghost t else observe self_heap.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      let next = G.allocated_forest result_heap.Ghost.ghost result_trees.Ghost.ghost depth self_node desc (refine_ u) in
      let refine_ t = next x in refine_ t)} in
  let[@def] self_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select self_heap.Ghost.ghost self_trees.Ghost.ghost x in r) in
  let self_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ self_heads_selected} in
  let self_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head self_heap.Ghost.ghost self_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> self_heads_selected_def x;
      let refine_ r = Forest_heads.select self_heap.Ghost.ghost self_trees.Ghost.ghost x in
      E.valid_head_def self_heap.Ghost.ghost self_heads.Ghost.ghost x; let u = () in refine_ u)} in
  let self_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at self_heap.Ghost.ghost self_heads.Ghost.ghost depth self_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> result_facts.Ghost.ghost x;
      allocated_def result_heap.Ghost.ghost depth self_node desc;
      let u = () in A.allocate_runtime result_heap.Ghost.ghost result_heads.Ghost.ghost self_heads.Ghost.ghost depth result_pool self_node desc result_valid.Ghost.ghost (refine_ self_valid.Ghost.ghost) x (refine_ u); refine_ u)} in
  let env_input : {f : F.forest | F.valid_forest f} = refine_ runtime_env in
  let refine_ with_self = F.cons self_node env_input in
  let refine_ env_input = env_input in
  let env_input : {f : F.forest | F.valid_forest f} = refine_ with_self in
  let refine_ next_runtime_env = F.cons argument env_input in
  let refine_ env_input = env_input in
  let next_env = ghost_ (Bind (argument, Bind (self_node, env))) in
  ghost_ (let u = () in let v = cell var depth in
    Dp.allocation_env h.Ghost.ghost argument v env (refine_ u);
    Dp.allocation_env arg_heap.Ghost.ghost result_node v env (refine_ u);
    let w = cell desc depth in Dp.allocation_env result_heap.Ghost.ghost self_node w env (refine_ u);
    Copy_heap_proofs.put_frame h.Ghost.ghost argument v argument;
    Copy_heap_proofs.put_frame arg_heap.Ghost.ghost result_node v argument;
    Copy_heap_proofs.put_frame result_heap.Ghost.ghost self_node w argument;
    Copy_heap_proofs.put_frame result_heap.Ghost.ghost self_node w self_node;
    let inner = Bind (self_node, env) in env_owned_def self_heap.Ghost.ghost inner; env_depth_def inner;
    env_owned_def self_heap.Ghost.ghost next_env; env_depth_def next_env);
  let resume : (answer : {r : inference | ran self_heap.Ghost.ghost depth self_pool (F.flatten next_runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_body && r.#value === result r.#execution}) @ unique -> {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun answer ->
    let refine_ answer = answer in
  let body_run = ghost_ answer.#execution in let body_pool = answer.#pool in let state = answer.#state in
  let middle : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RRec (argument, result_node, self_node, body_run, middle.Ghost.ghost, body_pool, Aborted)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after body_pool;
    source_def execution; result_def execution);
  let out = #{value = None; state = state; pool = body_pool; execution} in use (refine_ out)
  | Some target ->
  let body_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem middle.Ghost.ghost x then finite middle.Ghost.ghost t else observe middle.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      let refine_ t = G.run_forest self_heap.Ghost.ghost self_trees.Ghost.ghost depth self_pool next_env body_run middle.Ghost.ghost body_pool x (refine_ u) in refine_ t)} in
  let[@def] body_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select middle.Ghost.ghost body_trees.Ghost.ghost x in r) in
  let body_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ body_heads_selected} in
  let body_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle.Ghost.ghost body_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> body_heads_selected_def x;
      let refine_ r = Forest_heads.select middle.Ghost.ghost body_trees.Ghost.ghost x in
      E.valid_head_def middle.Ghost.ghost body_heads.Ghost.ghost x; let u = () in refine_ u)} in
  let body_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle.Ghost.ghost body_heads.Ghost.ghost depth body_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      Hm_effective_invariant.run_invariant self_heap.Ghost.ghost self_heads.Ghost.ghost self_trees.Ghost.ghost depth self_pool self_facts.Ghost.ghost next_env body_run middle.Ghost.ghost body_heads.Ghost.ghost body_valid.Ghost.ghost body_pool x (refine_ u); refine_ u)} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe middle.Ghost.ghost body_heads.Ghost.ghost x}) @ total = fun x ->
      body_facts.Ghost.ghost x; runtime_at_def middle.Ghost.ghost body_heads.Ghost.ghost depth body_pool x; let u = () in refine_ u in
    let u = () in Dp.run_pool_scoped self_heap.Ghost.ghost depth self_pool next_env body_run middle.Ghost.ghost body_heads.Ghost.ghost safe body_pool (refine_ u);
    Dp.run_env_owned self_heap.Ghost.ghost depth self_pool next_env body_run middle.Ghost.ghost body_pool (refine_ u); ());
  ghost_ (body_facts.Ghost.ghost target; let u = () in
    Hm_effective_result.result_below self_heap.Ghost.ghost self_trees.Ghost.ghost depth self_pool next_env body_run middle.Ghost.ghost body_pool body_heads.Ghost.ghost target (refine_ u);
    E.effective_below_def middle.Ghost.ghost body_heads.Ghost.ghost target depth; E.effective_active_def middle.Ghost.ghost body_heads.Ghost.ghost target; ());
  ghost_ (result_valid.Ghost.ghost result_node; let u = () in A.allocated_below arg_heap.Ghost.ghost depth result_node var result_heads.Ghost.ghost (refine_ u);
    E.effective_below_def result_heap.Ghost.ghost result_heads.Ghost.ghost result_node depth; ());
  ghost_ (self_valid.Ghost.ghost result_node; let w = cell desc depth in let u = () in
    A.saved_below result_heap.Ghost.ghost result_heads.Ghost.ghost self_heads.Ghost.ghost self_node w result_node depth (refine_ u);
    body_valid.Ghost.ghost result_node;
    Hm_effective_paths.run_below self_heap.Ghost.ghost self_heads.Ghost.ghost body_heads.Ghost.ghost depth self_pool next_env body_run middle.Ghost.ghost body_pool result_node depth (refine_ u);
    E.effective_below_def middle.Ghost.ghost body_heads.Ghost.ghost result_node depth; E.effective_active_def middle.Ghost.ghost body_heads.Ghost.ghost result_node; ());
  let d : int Ghost.t = {Ghost.ghost = ghost_ depth} in let pool_proof : pool Ghost.t = {Ghost.ghost = ghost_ body_pool} in
  let unify_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle.Ghost.ghost body_heads.Ghost.ghost d.Ghost.ghost pool_proof.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (refine_ body_facts.Ghost.ghost)} in
  let refine_ solved = Effective_hm_unify.unify middle body_heads d pool_proof unify_facts body_trees target result_node (refine_ state) in
  let value = if solved.#ok then Some self_node else None in
  let execution = ghost_ (RRec (argument, result_node, self_node, body_run, middle.Ghost.ghost, body_pool, Unified (solved.#ok, solved.#derivation))) in
  let after = ghost_ (Pref.own (borrow_ solved.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after body_pool;
    source_def execution; result_def execution);
  let out = #{value = value; state = solved.#state; pool = body_pool; execution} in use (refine_ out)
  in
  work goal self_heap self_heads self_trees depth self_pool self_facts next_runtime_env runtime_body (refine_ state) resume
  | T.Let (runtime_rhs, runtime_body) ->
    let child_depth = depth + 1 in
    if child_depth < 0 then assert false else (
    let child_pool : pool = Empty in
    ghost_ (pool_scoped_def h.Ghost.ghost child_pool);
    let child_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at h.Ghost.ghost heads.Ghost.ghost child_depth child_pool x})) Ghost.t =
      {Ghost.ghost = ghost_ (fun x -> facts.Ghost.ghost x; let u = () in enter_runtime h.Ghost.ghost heads.Ghost.ghost depth pool x (refine_ u); refine_ u)} in
  let resume_rhs : (answer : {r : inference | ran h.Ghost.ghost child_depth child_pool (F.flatten runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_rhs && r.#value === result r.#execution}) @ unique -> {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun answer ->
    let refine_ answer = answer in
  let rhs_run = ghost_ answer.#execution in let rhs_pool = answer.#pool in let state = answer.#state in
  let middle : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  match answer.#value with
  | None ->
  let execution = ghost_ (RLet_left (rhs_run, T.source runtime_body)) in
  let after = ghost_ (Pref.own (borrow_ state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after rhs_pool;
    source_def execution; result_def execution);
  let out = #{value = None; state = state; pool = rhs_pool; execution} in use (refine_ out)
  | Some bound_node ->
  let rhs_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem middle.Ghost.ghost x then finite middle.Ghost.ghost t else observe middle.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      let refine_ t = G.run_forest h.Ghost.ghost trees.Ghost.ghost child_depth child_pool env rhs_run middle.Ghost.ghost rhs_pool x (refine_ u) in refine_ t)} in
  let[@def] rhs_heads_selected : E.heads @ ghost = ghost_ (fun x ->
    let refine_ r = Forest_heads.select middle.Ghost.ghost rhs_trees.Ghost.ghost x in r) in
  let rhs_heads : E.heads Ghost.t = {Ghost.ghost = ghost_ rhs_heads_selected} in
  let rhs_valid : (((x : node Pref.t) @ immutable -> {u : unit | E.valid_head middle.Ghost.ghost rhs_heads.Ghost.ghost x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> rhs_heads_selected_def x;
      let refine_ r = Forest_heads.select middle.Ghost.ghost rhs_trees.Ghost.ghost x in
      E.valid_head_def middle.Ghost.ghost rhs_heads.Ghost.ghost x; let u = () in refine_ u)} in
  let rhs_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at middle.Ghost.ghost rhs_heads.Ghost.ghost child_depth rhs_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in
      Hm_effective_invariant.run_invariant h.Ghost.ghost heads.Ghost.ghost trees.Ghost.ghost child_depth child_pool child_facts.Ghost.ghost env rhs_run middle.Ghost.ghost rhs_heads.Ghost.ghost rhs_valid.Ghost.ghost rhs_pool x (refine_ u); refine_ u)} in
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe middle.Ghost.ghost rhs_heads.Ghost.ghost x}) @ total = fun x ->
      rhs_facts.Ghost.ghost x; runtime_at_def middle.Ghost.ghost rhs_heads.Ghost.ghost child_depth rhs_pool x; let u = () in refine_ u in
    let u = () in Dp.run_pool_scoped h.Ghost.ghost child_depth child_pool env rhs_run middle.Ghost.ghost rhs_heads.Ghost.ghost safe rhs_pool (refine_ u);
    Dp.run_env_owned h.Ghost.ghost child_depth child_pool env rhs_run middle.Ghost.ghost rhs_pool (refine_ u); ());
  ghost_ (
    let safe : ((x : node Pref.t) @ immutable -> {u : unit | Hm_effective_runtime.safe middle.Ghost.ghost rhs_heads.Ghost.ghost x}) @ total = fun x ->
      rhs_facts.Ghost.ghost x; runtime_at_def middle.Ghost.ghost rhs_heads.Ghost.ghost child_depth rhs_pool x; let u = () in refine_ u in
    let u = () in Dp.run_saved_pool h.Ghost.ghost child_depth child_pool env rhs_run middle.Ghost.ghost rhs_heads.Ghost.ghost safe rhs_pool pool (refine_ u);
    Dp.run_result h.Ghost.ghost trees.Ghost.ghost child_depth child_pool env rhs_run middle.Ghost.ghost rhs_pool bound_node (refine_ u); ());
  let refine_ closed = Representative_pool.close_and_transfer middle depth rhs_pool pool (refine_ state) in
  let parent_pool = closed.#parent in let state = closed.#state in
  let start : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
  let parent_trees : (((x : node Pref.t) @ immutable ->
    {t : tree | tree_root t === x && (if H.mem start.Ghost.ghost x then finite start.Ghost.ghost t else observe start.Ghost.ghost x === None)} @ immutable)) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in let refine_ t = G.representative_closed_forest middle.Ghost.ghost rhs_trees.Ghost.ghost depth rhs_pool x (refine_ u) in refine_ t)} in
  let parent_facts : (((x : node Pref.t) @ immutable -> {u : unit | runtime_at start.Ghost.ghost rhs_heads.Ghost.ghost depth parent_pool x})) Ghost.t =
    {Ghost.ghost = ghost_ (fun x -> let u = () in Hm_effective_closing.close_after_run h.Ghost.ghost heads.Ghost.ghost rhs_heads.Ghost.ghost depth pool facts.Ghost.ghost env rhs_run middle.Ghost.ghost rhs_pool (refine_ rhs_facts.Ghost.ghost) x (refine_ u); refine_ u)} in
  let env_input : {f : F.forest | F.valid_forest f} = refine_ runtime_env in
  let refine_ next_runtime_env = F.cons bound_node env_input in
  let refine_ env_input = env_input in
  let next_env = ghost_ (Bind (bound_node, env)) in
  ghost_ (
    let frame : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem middle.Ghost.ghost x) || H.mem start.Ghost.ghost x}) @ total = fun x ->
      let u = () in Representative_pool_spec.close_heap_def middle.Ghost.ghost depth rhs_pool;
      R.representatives_scoped middle.Ghost.ghost rhs_pool (refine_ u);
      let filtered = R.representatives middle.Ghost.ghost rhs_pool in
      Generalize_proofs.closed_observe middle.Ghost.ghost depth filtered x (refine_ u);
      closed_at_def middle.Ghost.ghost start.Ghost.ghost depth filtered x; refine_ u in
    let u = () in Dp.env_extend middle.Ghost.ghost start.Ghost.ghost frame env (refine_ u); frame bound_node;
    env_owned_def start.Ghost.ghost next_env; env_depth_def next_env);
  let resume_body : (answer : {r : inference | ran start.Ghost.ghost depth parent_pool (F.flatten next_runtime_env) r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source runtime_body && r.#value === result r.#execution}) @ unique -> {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun answer ->
    let refine_ answer = answer in
  let execution = ghost_ (RLet (rhs_run, answer.#execution, middle.Ghost.ghost, rhs_pool)) in
  let after = ghost_ (Pref.own (borrow_ answer.#state)) in
  ghost_ ( ran_def h.Ghost.ghost depth pool env execution after answer.#pool;
    source_def execution; result_def execution);
  let out = #{value = answer.#value; state = answer.#state; pool = answer.#pool; execution} in use (refine_ out)
  in
  work goal start rhs_heads parent_trees depth parent_pool parent_facts next_runtime_env runtime_body (refine_ state) resume_body
  in
  work goal h heads trees child_depth child_pool child_facts runtime_env runtime_rhs (refine_ state) resume_rhs
  )

let closed_compiled :
    (input : {e : T.term | T.valid e && D.scoped_term D.Z (T.source e)}) @ immutable ->
    {r : inference | let refine_ input = input in
      ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty
        r.#execution (Pref.own r.#state) r.#pool
      && source r.#execution === T.source input && r.#value === result r.#execution} @ unique =
  fun input ->
    let refine_ input = input in let refine_ state = Pref.empty () in
    let h : Pref.heap Ghost.t = {Ghost.ghost = ghost_ (Pref.own (borrow_ state))} in
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
        let u = () in refine_ u)} in
    let runtime_env = F.Nil in let env : env = Hm_environment_spec.Empty in
    ghost_ (F.valid_forest_def runtime_env; F.flatten_def runtime_env;
      pool_scoped_def h.Ghost.ghost pool; env_owned_def h.Ghost.ghost env; env_depth_def env);
    let goal = {heap = ghost_ h.Ghost.ghost; depth = ghost_ 0; pool = ghost_ pool;
      env = ghost_ env; term = ghost_ (T.source input)} in
    let use : (r : {r : inference | ran h.Ghost.ghost 0 pool (F.flatten runtime_env)
        r.#execution (Pref.own r.#state) r.#pool && source r.#execution === T.source input
        && r.#value === result r.#execution}) @ unique ->
      {r : inference | ran goal.heap goal.depth goal.pool goal.env r.#execution (Pref.own r.#state) r.#pool
        && source r.#execution === goal.term && r.#value === result r.#execution} @ unique = fun r ->
      let refine_ r = r in refine_ r in
    let refine_ out = work goal h heads trees 0 pool facts runtime_env input (refine_ state) use in refine_ out

let closed_hm : (e : {e : D.term | D.scoped_term D.Z e}) @ immutable ->
    {r : inference | let refine_ e = e in
      ran (H.empty ()) 0 Generalize_spec.Empty Hm_environment_spec.Empty
        r.#execution (Pref.own r.#state) r.#pool
      && source r.#execution === e && r.#value === result r.#execution} @ unique =
  fun e ->
    let refine_ e = e in let refine_ input = T.compile e in
    let input : {e : T.term | T.valid e && D.scoped_term D.Z (T.source e)} = refine_ input in
    let refine_ out = closed_compiled input in let refine_ input = input in refine_ out
