open Copy_spec
open Level_unifier_spec
open Generalize_spec
open Hm_environment_spec
open Hm_effective_execution_spec
open Level_finite_spec
module A = Hm_annotation_trace
module S = Hm_annotation_trace_spec
module Forest = Hm_effective_forest
module Model = Hm_effective_model
module R = Representative_pool_spec
module D = Hm_declarative

let[@def] rec (equations @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (trace : A.trace @ immutable) = ghost_ (
  match trace with
  | A.Failed -> false
  | A.Variable_use _ -> true
  | A.Boolean_literal p | A.False_literal p -> rho p === Boolean
  | A.Word_literal (_, p) -> rho p === Word64
  | A.Empty_list_literal p -> (match rho p with List_type _ -> true | _ -> false)
  | A.Abstraction (p, argument, child) -> equations rho child &&
    (match A.root child with None -> false | Some b -> rho p === Function (rho argument, rho b))
  | A.Application (Some p, left, right) -> equations rho left && equations rho right &&
    (match A.root left, A.root right with Some f, Some a -> rho f === Function (rho a, rho p) | _ -> false)
  | A.List_constructor (Some p, left, right) -> equations rho left && equations rho right &&
    (match A.root left, A.root right with Some head, Some tail -> rho p === List_type (rho head) && rho tail === rho p | _ -> false)
  | A.Recursion (Some p, argument, result, child) -> equations rho child &&
    rho p === Function (rho argument, rho result) &&
    (match A.root child with None -> false | Some b -> rho b === rho result)
  | A.Conditional child | A.List_case child -> equations rho child
  | A.Primitive (op, Some p, child) -> equations rho child &&
    rho p === (match op with D.Add | D.Subtract -> Word64 | D.Equal_word | D.Unsigned_less -> Boolean)
  | A.Let_binding (rhs, body) -> equations rho rhs && equations rho body
  | _ -> false)

let (closed_model @ total) : (heap : node Pref.heap) @ immutable -> (depth : int) -> (pool : pool) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total -> (x : node Pref.t) @ immutable ->
    {u : unit | pool_scoped heap pool && node_equation (R.close_heap heap depth pool) rho x} ->
    {u : unit | node_equation heap rho x} @ ghost = fun heap depth pool rho x premise -> ghost_ (
  R.close_heap_def heap depth pool; Representative_level.representatives_scoped heap pool ();
  let filtered = Representative_level.representatives heap pool in
  Generalize_proofs.closed_model heap depth filtered rho x ();
  Copy_spec.equation_def heap rho x; Copy_spec.equation_def (R.close_heap heap depth pool) rho x;
  observe_def heap x; observe_def (R.close_heap heap depth pool) x;
  node_equation_def heap rho x; node_equation_def (R.close_heap heap depth pool) rho x)

let rec (run @ total) : (heap : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem heap x then finite heap t else observe heap x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (execution : execution) @ immutable -> (after : node Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x})) @ total ->
    (trace : A.trace) @ immutable ->
    {u : unit | ran heap depth pool env execution after final_pool
      && S.records trace execution && not (result execution === None)} ->
    {u : unit | equations rho trace} @ ghost =
  fun heap trees depth pool env execution after final_pool rho model trace premise -> ghost_ (
    ran_def heap depth pool env execution after final_pool; result_def execution;
    S.records_def trace execution; equations_def rho trace;
    match trace, execution with
    | A.Boolean_literal p, RBool _ | A.False_literal p, RFalse _ ->
      model p; node_equation_def after rho p; observe_def after p; cell_def Bool depth
    | A.Word_literal (_, p), RWord _ ->
      model p; node_equation_def after rho p; observe_def after p; cell_def Word depth
    | A.Empty_list_literal p, RNil (argument, _) ->
      model p; node_equation_def after rho p; observe_def after p; cell_def (List argument) depth
    | A.Abstraction (p, argument, child), RLam (_, body, middle, body_pool, _) ->
      let start = H.put heap argument (cell Var depth) in
      let allocated = Forest.allocated_forest heap trees depth argument Var () in
      let child_trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem start x then finite start t else observe start x === None)} @ immutable) @ total =
        fun x -> allocated x in
      let child_pool = Entry (argument, pool) in let child_env = Bind (argument, env) in
      let middle_trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
        Forest.run_forest start child_trees depth child_pool child_env body middle body_pool x () in
      (match result body with None -> unreachable_ () | Some b ->
        let desc = Arrow (argument, b) in allocated_def middle depth p desc;
        let child_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
          let _tree = middle_trees x in model x;
          Model.allocation_restrict middle p (cell desc depth) rho x () in
        run start child_trees depth child_pool child_env body middle body_pool rho child_model child ();
        S.root_agrees child body ();
        model p; node_equation_def after rho p; observe_def after p; cell_def desc depth)
    | A.Application (_, left, right), RApp (l, r, h1, p1, h2, p2, p, arrow, _, derivation) ->
      let trees1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        Forest.run_forest heap trees depth pool env l h1 p1 x () in
      let trees2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
        Forest.run_forest h1 trees1 depth p1 env r h2 p2 x () in
      (match result l, result r with
      | Some f, Some a ->
        let h3 = H.put h2 p (cell Var depth) in
        let desc = Arrow (a, p) in let h4 = H.put h3 arrow (cell desc depth) in
        let allocated = Forest.allocated_forest h2 trees2 depth p Var () in
      let trees3 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total =
        fun x -> allocated x in
        allocated_def h2 depth p Var; allocated_def h3 depth arrow desc;
        let model4 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h4 rho x}) @ total = fun x ->
          Effective_unifier_model.success_forward_at h4 rho f arrow after derivation model x () in
        let model3 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h3 rho x}) @ total = fun x ->
          let _tree = trees3 x in model4 x; Model.allocation_restrict h3 arrow (cell desc depth) rho x () in
        let model2 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h2 rho x}) @ total = fun x ->
          let _tree = trees2 x in model3 x; Model.allocation_restrict h2 p (cell Var depth) rho x () in
        let model1 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h1 rho x}) @ total = fun x ->
          Model.run_restrict h1 trees1 depth p1 env r h2 p2 rho model2 x () in
        run heap trees depth pool env l h1 p1 rho model1 left ();
        run h1 trees1 depth p1 env r h2 p2 rho model2 right ();
        S.root_agrees left l (); S.root_agrees right r ();
        Effective_unifier_model.success_forward_at h4 rho f arrow after derivation model arrow ();
        node_equation_def h4 rho arrow; observe_def h4 arrow; cell_def desc depth
      | _ -> unreachable_ ())
    | A.List_constructor (_, left, right), RCons (l, r, h1, p1, h2, p2, p, _, derivation) ->
      let trees1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total = fun x ->
        Forest.run_forest heap trees depth pool env l h1 p1 x () in
      let trees2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total = fun x ->
        Forest.run_forest h1 trees1 depth p1 env r h2 p2 x () in
      (match result l, result r with
      | Some head, Some tail ->
        let desc = List head in let h3 = H.put h2 p (cell desc depth) in
        allocated_def h2 depth p desc;
        let model3 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h3 rho x}) @ total = fun x ->
          Effective_unifier_model.success_forward_at h3 rho tail p after derivation model x () in
        let model2 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h2 rho x}) @ total = fun x ->
          let _tree = trees2 x in model3 x; Model.allocation_restrict h2 p (cell desc depth) rho x () in
        let model1 : ((x : node Pref.t) @ immutable -> {u : unit | node_equation h1 rho x}) @ total = fun x ->
          Model.run_restrict h1 trees1 depth p1 env r h2 p2 rho model2 x () in
        run heap trees depth pool env l h1 p1 rho model1 left ();
        run h1 trees1 depth p1 env r h2 p2 rho model2 right ();
        S.root_agrees left l (); S.root_agrees right r ();
        Effective_unifier_model.success_forward_at h3 rho tail p after derivation model p ();
        node_equation_def h3 rho p; observe_def h3 p; cell_def desc depth
      | _ -> unreachable_ ())
    | A.Conditional child, RIf (_, _, _, body) | A.List_case child, RCaseList (_, _, _, body) ->
      run heap trees depth pool env body after final_pool rho model child ()
    | A.Primitive (_, _, child), RPrimitive (op, _, _, body, middle, body_pool, out) ->
      (match result body, out with
      | Some _, Some p ->
        let middle_trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
          (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
          Forest.run_forest heap trees depth pool env body middle body_pool x () in
        let desc = primitive_desc op in allocated_def middle depth p desc;
        let child_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
          let _tree = middle_trees x in model x; Model.allocation_restrict middle p (cell desc depth) rho x () in
        run heap trees depth pool env body middle body_pool rho child_model child ();
        model p; node_equation_def after rho p; observe_def after p; cell_def desc depth; primitive_desc_def op
      | _ -> unreachable_ ())
    | A.Recursion (_, _, _, child), RRec (argument, res, self, body, middle, body_pool, finish) ->
      let h1 = H.put heap argument (cell Var depth) in
      let h2 = H.put h1 res (cell Var depth) in
      let desc = Arrow (argument, res) in let h3 = H.put h2 self (cell desc depth) in
      let allocated = Forest.allocated_forest heap trees depth argument Var () in
      let trees1 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h1 x then finite h1 t else observe h1 x === None)} @ immutable) @ total =
        fun x -> allocated x in
      let allocated = Forest.allocated_forest h1 trees1 depth res Var () in
      let trees2 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h2 x then finite h2 t else observe h2 x === None)} @ immutable) @ total =
        fun x -> allocated x in
      let allocated = Forest.allocated_forest h2 trees2 depth self desc () in
      let trees3 : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem h3 x then finite h3 t else observe h3 x === None)} @ immutable) @ total =
        fun x -> allocated x in
      let child_pool = Entry (self, Entry (res, Entry (argument, pool))) in
      let child_env = Bind (argument, Bind (self, env)) in
      (match result body, finish with
      | Some b, Unified (true, derivation) ->
        let child_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
          Effective_unifier_model.success_forward_at middle rho b res after derivation model x () in
        run h3 trees3 depth child_pool child_env body middle body_pool rho child_model child ();
        S.root_agrees child body ();
        Effective_unifier_model.success_forward_at middle rho b res after derivation model self ();
        Model.run_restrict h3 trees3 depth child_pool child_env body middle body_pool rho child_model self ();
        node_equation_def h3 rho self; observe_def h3 self; cell_def desc depth
      | _ -> unreachable_ ())
    | A.Let_binding (rhs, child), RLet (r, body, middle, child_pool) ->
      let middle_trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
        (if H.mem middle x then finite middle t else observe middle x === None)} @ immutable) @ total = fun x ->
        Forest.run_forest heap trees (depth + 1) Empty env r middle child_pool x () in
      (match result r with None -> unreachable_ () | Some p ->
        let closed = R.close_heap middle depth child_pool in
        let closed_trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
          (if H.mem closed x then finite closed t else observe closed x === None)} @ immutable) @ total = fun x ->
          Forest.representative_closed_forest middle middle_trees depth child_pool x () in
        let parent = R.transfer_rep closed child_pool pool in let child_env = Bind (p, env) in
        let middle_model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation middle rho x}) @ total = fun x ->
          Model.run_restrict closed closed_trees depth parent child_env body after final_pool rho model x ();
          closed_model middle depth child_pool rho x () in
        run heap trees (depth + 1) Empty env r middle child_pool rho middle_model rhs ();
        run closed closed_trees depth parent child_env body after final_pool rho model child ())
    | _ -> ())

let (readback @ total) : (heap : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem heap x then finite heap t else observe heap x === None)} @ immutable)) @ total ->
    (depth : int) -> (pool : pool) @ immutable -> (env : env) @ immutable ->
    (execution : execution) @ immutable -> (after : node Pref.heap) @ immutable -> (final_pool : pool) @ immutable ->
    (final_trees : ((x : node Pref.t) @ immutable -> {t : tree | tree_root t === x &&
      (if H.mem after x then finite after t else observe after x === None)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (agrees : ((x : node Pref.t) @ immutable -> {u : unit | rho x === readback (final_trees x)})) @ total ->
    (trace : A.trace) @ immutable ->
    {u : unit | ran heap depth pool env execution after final_pool
      && S.records trace execution && not (result execution === None)} ->
    {u : unit | equations rho trace} @ ghost =
  fun heap trees depth pool env execution after final_pool final_trees rho agrees trace premise -> ghost_ (
    let model : ((x : node Pref.t) @ immutable -> {u : unit | node_equation after rho x}) @ total = fun x ->
      Level_finite_proofs.readback_model_at after final_trees rho (fun x -> agrees x) x in
    run heap trees depth pool env execution after final_pool rho model trace ())

let[@def] (annotates @ total) (source : D.term @ immutable) (trace : A.trace @ immutable)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total) (ty : ty @ immutable) = ghost_ (
  Hm_annotation_shape.matches source trace && equations rho trace &&
  match A.root trace with None -> false | Some p -> rho p === ty)
