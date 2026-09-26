module A = Hm_annotation_trace
module E = Hm_effective_execution_spec

let[@def] rec (records @ total) (trace : A.trace @ immutable)
    (execution : E.execution @ immutable) = ghost_ (
  match trace, execution with
  | A.Failed, _ -> E.result execution === None
  | A.Variable_use p, E.RShared (_, q, _)
  | A.Variable_use p, E.RVar (_, q, _, _, _) -> p === q
  | A.Boolean_literal p, E.RBool q -> p === q
  | A.False_literal p, E.RFalse q -> p === q
  | A.Empty_list_literal p, E.RNil (_, q) -> p === q
  | A.Word_literal (word, p), E.RWord (other, q) -> word === other && p === q
  | A.Abstraction (p, a, body), E.RLam (b, run, _, _, out) ->
    out === Some p && a === b && records body run
  | A.Application (p, left, right), E.RApp (l, r, _, _, _, _, _, _, _, _) ->
    p === E.result execution && records left l && records right r
  | A.List_constructor (p, left, right), E.RCons (l, r, _, _, _, _, _, _, _) ->
    p === E.result execution && records left l && records right r
  | A.List_case child, E.RCaseList (_, _, _, body)
  | A.Conditional child, E.RIf (_, _, _, body) -> records child body
  | A.Primitive (op, p, child), E.RPrimitive (other, _, _, body, _, _, out) ->
    op === other && p === out && records child body
  | A.Recursion (p, a, b, body), E.RRec (x, y, _, run, _, _, _) ->
    p === E.result execution && a === x && b === y && records body run
  | A.Let_binding (rhs, body), E.RLet (r, b, _, _) ->
    records rhs r && records body b
  | _ -> false)

let rec (root_agrees @ total) :
    (trace : A.trace) @ immutable -> (execution : E.execution) @ immutable ->
    {u : unit | records trace execution} ->
    {u : unit | A.root trace === E.result execution} @ ghost =
  fun trace execution premise -> ghost_ (
    records_def trace execution; A.root_def trace; E.result_def execution;
    match trace, execution with
    | A.List_case body, E.RCaseList (_, _, _, run)
    | A.Conditional body, E.RIf (_, _, _, run)
    | A.Let_binding (_, body), E.RLet (_, run, _, _) ->
      root_agrees body run ()
    | _ -> ())

module H = Pref.Heap

let[@def] (option_owned @ total) (heap : Copy_spec.node Pref.heap @ immutable)
    (root : Copy_spec.node Pref.t option @ immutable) = ghost_ (
  match root with None -> true | Some p -> H.mem heap p)

let[@def] rec (owned @ total) (heap : Copy_spec.node Pref.heap @ immutable)
    (trace : A.trace @ immutable) = ghost_ (
  match trace with
  | A.Failed -> true
  | A.Variable_use p | A.Boolean_literal p | A.False_literal p | A.Empty_list_literal p | A.Word_literal (_, p) -> H.mem heap p
  | A.Abstraction (p, a, body) ->
    H.mem heap p && H.mem heap a && owned heap body
  | A.Application (p, left, right) | A.List_constructor (p, left, right) ->
    option_owned heap p && owned heap left && owned heap right
  | A.Recursion (p, a, b, body) ->
    option_owned heap p && H.mem heap a && H.mem heap b && owned heap body
  | A.Conditional body | A.List_case body -> owned heap body
  | A.Primitive (_, p, body) -> option_owned heap p && owned heap body
  | A.Let_binding (rhs, body) -> owned heap rhs && owned heap body)

let (option_owned_after @ total) :
    (before : Copy_spec.node Pref.heap) @ immutable -> (after : Copy_spec.node Pref.heap) @ immutable ->
    (root : Copy_spec.node Pref.t option) @ immutable ->
    (preserve : ((p : Copy_spec.node Pref.t) @ immutable ->
      {u : unit | not (H.mem before p) || H.mem after p})) @ total ->
    {u : unit | option_owned before root} ->
    {u : unit | option_owned after root} @ ghost =
  fun before after root preserve premise -> ghost_ (
    option_owned_def before root; option_owned_def after root;
    match root with None -> () | Some p -> preserve p)

let rec (owned_after @ total) :
    (before : Copy_spec.node Pref.heap) @ immutable -> (after : Copy_spec.node Pref.heap) @ immutable ->
    (trace : A.trace) @ immutable ->
    (preserve : ((p : Copy_spec.node Pref.t) @ immutable ->
      {u : unit | not (H.mem before p) || H.mem after p})) @ total ->
    {u : unit | owned before trace} ->
    {u : unit | owned after trace} @ ghost =
  fun before after trace preserve premise -> ghost_ (
    owned_def before trace; owned_def after trace;
    match trace with
    | A.Failed -> ()
    | A.Variable_use p | A.Boolean_literal p | A.False_literal p | A.Empty_list_literal p | A.Word_literal (_, p) -> preserve p
    | A.Abstraction (p, a, body) ->
      preserve p; preserve a; owned_after before after body preserve ()
    | A.Application (p, left, right) | A.List_constructor (p, left, right) ->
      option_owned_after before after p preserve ();
      owned_after before after left preserve ();
      owned_after before after right preserve ()
    | A.Recursion (p, a, b, body) ->
      option_owned_after before after p preserve ();
      preserve a; preserve b; owned_after before after body preserve ()
    | A.Conditional body | A.List_case body -> owned_after before after body preserve ()
    | A.Primitive (_, p, body) ->
      option_owned_after before after p preserve ();
      owned_after before after body preserve ()
    | A.Let_binding (rhs, body) ->
      owned_after before after rhs preserve ();
      owned_after before after body preserve ())
