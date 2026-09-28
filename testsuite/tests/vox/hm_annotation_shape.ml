module D = Hm_declarative
module A = Hm_annotation_trace
module S = Hm_annotation_trace_spec
module E = Hm_effective_execution_spec
module P = Generalize_spec
module Env = Hm_environment_spec
module H = Pref.Heap
module R = Representative_pool_spec

let[@def] rec (matches @ total) (source : D.term @ immutable) (trace : A.trace @ immutable) = ghost_ (
  match source, trace with
  | D.Bound _, A.Variable_use _
  | D.Truth, A.Boolean_literal _
  | D.False, A.False_literal _
  | D.Nil, A.Empty_list_literal _ -> true
  | D.Word word, A.Word_literal (other, _) -> word === other
  | D.Lambda body, A.Abstraction (_, _, child) -> matches body child
  | D.Apply (left, right), A.Application (Some _, l, r)
  | D.Cons (left, right), A.List_constructor (Some _, l, r) -> matches left l && matches right r
  | D.Recursive body, A.Recursion (Some _, _, _, child) -> matches body child
  | D.Let (rhs, body), A.Let_binding (r, b) -> matches rhs r && matches body b
  | D.If (condition, yes, no), A.Conditional child ->
    matches (Hm_conditional_constraints.encoded condition yes no) child
  | D.CaseList (scrutinee, empty, nonempty), A.List_case child ->
    matches (Hm_list_case_constraints.encoded scrutinee empty nonempty) child
  | D.Primitive (op, left, right), A.Primitive (other, Some _, child) ->
    op === other && matches (Hm_primitive_constraints.arguments left right) child
  | _ -> false)

let rec (root @ total) : (source : D.term) @ immutable -> (trace : A.trace) @ immutable ->
    {u : unit | matches source trace} -> {u : unit | not (A.root trace === None)} @ ghost =
  fun source trace premise -> ghost_ (
    matches_def source trace; A.root_def trace;
    match source, trace with
    | D.If (condition, yes, no), A.Conditional child ->
      root (Hm_conditional_constraints.encoded condition yes no) child ()
    | D.CaseList (scrutinee, empty, nonempty), A.List_case child ->
      root (Hm_list_case_constraints.encoded scrutinee empty nonempty) child ()
    | D.Let (_, body), A.Let_binding (_, child) -> root body child ()
    | _ -> ())

let rec (successful @ total) : (heap : Copy_spec.node Pref.heap) @ immutable -> (depth : int) ->
    (pool : P.pool) @ immutable -> (env : Env.env) @ immutable ->
    (execution : E.execution) @ immutable -> (after : Copy_spec.node Pref.heap) @ immutable ->
    (final_pool : P.pool) @ immutable -> (trace : A.trace) @ immutable ->
    {u : unit | E.ran heap depth pool env execution after final_pool
      && S.records trace execution && not (E.result execution === None)} ->
    {u : unit | matches (E.source execution) trace} @ ghost =
  fun heap depth pool env execution after final_pool trace premise -> ghost_ (
    E.ran_def heap depth pool env execution after final_pool;
    E.result_def execution; E.source_def execution; S.records_def trace execution;
    matches_def (E.source execution) trace;
    match trace, execution with
    | A.Abstraction (_, _, child), E.RLam (argument, body, middle, body_pool, _) ->
      successful (H.put heap argument (Copy_spec.cell Copy_spec.Var depth)) depth (P.Entry (argument, pool))
        (Env.Bind (argument, env)) body middle body_pool child ()
    | A.Application (_, left, right), E.RApp (l, r, h1, p1, h2, p2, _, _, _, _)
    | A.List_constructor (_, left, right), E.RCons (l, r, h1, p1, h2, p2, _, _, _) ->
      successful heap depth pool env l h1 p1 left ();
      successful h1 depth p1 env r h2 p2 right ()
    | A.Recursion (_, _, _, child), E.RRec (argument, result, self, body, middle, body_pool, _) ->
      let h1 = H.put heap argument (Copy_spec.cell Copy_spec.Var depth) in
      let h2 = H.put h1 result (Copy_spec.cell Copy_spec.Var depth) in
      let h3 = H.put h2 self (Copy_spec.cell (Copy_spec.Arrow (argument, result)) depth) in
      successful h3 depth (P.Entry (self, P.Entry (result, P.Entry (argument, pool))))
        (Env.Bind (argument, Env.Bind (self, env))) body middle body_pool child ()
    | A.Let_binding (rhs, child), E.RLet (r, body, middle, child_pool) ->
      successful heap (depth + 1) P.Empty env r middle child_pool rhs ();
      (match E.result r with
      | None -> unreachable_ ()
      | Some p ->
        let closed = R.close_heap middle depth child_pool in
        successful closed depth (R.transfer_rep closed child_pool pool)
          (Env.Bind (p, env)) body after final_pool child ())
    | A.Conditional child, E.RIf (_, _, _, body)
    | A.List_case child, E.RCaseList (_, _, _, body) ->
      successful heap depth pool env body after final_pool child ()
    | A.Primitive (_, _, child), E.RPrimitive (_, _, _, body, middle, body_pool, _) ->
      successful heap depth pool env body middle body_pool child ()
    | _ -> ())
