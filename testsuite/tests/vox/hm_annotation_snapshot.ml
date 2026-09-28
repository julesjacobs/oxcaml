module A = Hm_annotation_trace
module S = Hm_annotation_trace_spec
module F = Level_finite_spec
module U = Level_unifier_spec
module H = Pref.Heap

type snapshot =
  | Empty
  | Entry of Copy_spec.node Pref.t * Copy_spec.ty * snapshot
[@@inductive]

let[@def] rec (lookup @ total) (p : Copy_spec.node Pref.t @ immutable)
    (snapshot : snapshot @ immutable) =
  match snapshot with
  | Empty -> None
  | Entry (q, ty, rest) ->
    if Pref.equal p q then Some ty else lookup p rest

let[@def] rec (record @ total) :
    (h : Copy_spec.node Pref.heap) @ immutable ghost ->
    (trees : ((p : Copy_spec.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem h p then F.finite h t else U.observe h p === None)}
      @ immutable)) @ total ghost ->
    (trace : A.trace) @ immutable -> (snapshot : snapshot) @ immutable -> snapshot @ immutable ghost =
  fun h trees trace snapshot -> ghost_ (
    match trace with
    | A.Failed -> snapshot
    | A.Variable_use p | A.Boolean_literal p | A.False_literal p | A.Empty_list_literal p | A.Word_literal (_, p) ->
      Entry (p, F.readback (trees p), snapshot)
    | A.Abstraction (p, a, body) ->
      let snapshot = Entry (p, F.readback (trees p), snapshot) in
      let snapshot = Entry (a, F.readback (trees a), snapshot) in
      record h trees body snapshot
    | A.Application (p, left, right) | A.List_constructor (p, left, right) ->
      let snapshot = match p with
        | None -> snapshot | Some p -> Entry (p, F.readback (trees p), snapshot) in
      let snapshot = record h trees left snapshot in
      record h trees right snapshot
    | A.Recursion (p, a, b, body) ->
      let snapshot = match p with
        | None -> snapshot | Some p -> Entry (p, F.readback (trees p), snapshot) in
      let snapshot = Entry (a, F.readback (trees a), snapshot) in
      let snapshot = Entry (b, F.readback (trees b), snapshot) in
      record h trees body snapshot
    | A.Conditional body | A.List_case body -> record h trees body snapshot
    | A.Primitive (_, p, body) ->
      let snapshot = match p with None -> snapshot | Some p -> Entry (p, F.readback (trees p), snapshot) in
      record h trees body snapshot
    | A.Let_binding (rhs, body) ->
      let snapshot = record h trees rhs snapshot in
      record h trees body snapshot)

let capture :
    (h : Copy_spec.node Pref.heap) @ immutable ghost ->
    (trees : ((p : Copy_spec.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem h p then F.finite h t else U.observe h p === None)}
      @ immutable)) @ total ghost ->
    (p : {p : Copy_spec.node Pref.t | H.mem h p}) @ immutable ->
    (state : {s : Copy_spec.node Pref.token | Pref.own s === h}) @ local read ->
    (snapshot : snapshot) @ immutable ->
    {out : snapshot | out === Entry (p, F.readback (trees p), snapshot)} @ immutable =
  fun h trees p state snapshot ->
    let tree = ghost_ (trees p) in
    let ty = Hm_readback_runtime.read tree p state in
    Entry (p, ty, snapshot)

let rec collect :
    (h : Copy_spec.node Pref.heap) @ immutable ghost ->
    (trees : ((p : Copy_spec.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem h p then F.finite h t else U.observe h p === None)}
      @ immutable)) @ total ghost ->
    (trace : {t : A.trace | S.owned h t}) @ immutable ->
    (state : {s : Copy_spec.node Pref.token | Pref.own s === h}) @ local read ->
    (snapshot : snapshot) @ immutable ->
    {out : snapshot | out === record h trees trace snapshot} @ immutable =
  fun h trees trace state snapshot ->
    ghost_ (S.owned_def h trace; record_def h trees trace snapshot);
    match trace with
    | A.Failed -> snapshot
    | A.Variable_use p | A.Boolean_literal p | A.False_literal p | A.Empty_list_literal p | A.Word_literal (_, p) ->
      capture h trees p state snapshot
    | A.Abstraction (p, a, body) ->
      let snapshot = capture h trees p state snapshot in
      let snapshot = capture h trees a state snapshot in
      collect h trees body state snapshot
    | A.Application (p, left, right) | A.List_constructor (p, left, right) ->
      ghost_ (S.option_owned_def h p);
      let snapshot = match p with
        | None -> snapshot | Some p -> capture h trees p state snapshot in
      let snapshot = collect h trees left state snapshot in
      collect h trees right state snapshot
    | A.Recursion (p, a, b, body) ->
      ghost_ (S.option_owned_def h p);
      let snapshot = match p with
        | None -> snapshot | Some p -> capture h trees p state snapshot in
      let snapshot = capture h trees a state snapshot in
      let snapshot = capture h trees b state snapshot in
      collect h trees body state snapshot
    | A.Conditional body | A.List_case body -> collect h trees body state snapshot
    | A.Primitive (_, p, body) ->
      ghost_ (S.option_owned_def h p);
      let snapshot = match p with None -> snapshot | Some p -> capture h trees p state snapshot in
      collect h trees body state snapshot
    | A.Let_binding (rhs, body) ->
      let snapshot = collect h trees rhs state snapshot in
      collect h trees body state snapshot

let (entry_lookup @ total) : (p : Copy_spec.node Pref.t) @ immutable -> (q : Copy_spec.node Pref.t) @ immutable ->
    (ty : Copy_spec.ty) @ immutable -> (snapshot : snapshot) @ immutable ->
    {u : unit | lookup p (Entry (q, ty, snapshot)) === (if p === q then Some ty else lookup p snapshot)} @ ghost =
  fun p q ty snapshot -> ghost_ (
    lookup_def p (Entry (q, ty, snapshot)); let same = Pref.equal p q in if same then () else ())

let rec (record_lookup @ total) :
    (h : Copy_spec.node Pref.heap) @ immutable ghost ->
    (trees : ((p : Copy_spec.node Pref.t) @ immutable ->
      {t : F.tree | F.tree_root t === p &&
        (if H.mem h p then F.finite h t else U.observe h p === None)}
      @ immutable)) @ total ghost ->
    (p : Copy_spec.node Pref.t) @ immutable -> (trace : A.trace) @ immutable -> (snapshot : snapshot) @ immutable ->
    {u : unit | not (A.contains p trace || lookup p snapshot === Some (F.readback (trees p)))
      || lookup p (record h trees trace snapshot) === Some (F.readback (trees p))} @ ghost =
  fun h trees p trace snapshot -> ghost_ (
    A.contains_def p trace; record_def h trees trace snapshot;
    match trace with
    | A.Failed -> ()
    | A.Variable_use q | A.Boolean_literal q | A.False_literal q | A.Empty_list_literal q | A.Word_literal (_, q) ->
      entry_lookup p q (F.readback (trees q)) snapshot
    | A.Abstraction (q, a, body) ->
      entry_lookup p q (F.readback (trees q)) snapshot;
      let snapshot = Entry (q, F.readback (trees q), snapshot) in
      entry_lookup p a (F.readback (trees a)) snapshot;
      record_lookup h trees p body (Entry (a, F.readback (trees a), snapshot))
    | A.Application (q, left, right) | A.List_constructor (q, left, right) ->
      A.option_contains_def p q;
      let snapshot = match q with None -> snapshot | Some q ->
        entry_lookup p q (F.readback (trees q)) snapshot; Entry (q, F.readback (trees q), snapshot) in
      record_lookup h trees p left snapshot;
      record_lookup h trees p right (record h trees left snapshot)
    | A.Recursion (q, a, b, body) ->
      A.option_contains_def p q;
      let snapshot = match q with None -> snapshot | Some q ->
        entry_lookup p q (F.readback (trees q)) snapshot; Entry (q, F.readback (trees q), snapshot) in
      entry_lookup p a (F.readback (trees a)) snapshot;
      let snapshot = Entry (a, F.readback (trees a), snapshot) in
      entry_lookup p b (F.readback (trees b)) snapshot;
      record_lookup h trees p body (Entry (b, F.readback (trees b), snapshot))
    | A.Conditional body | A.List_case body -> record_lookup h trees p body snapshot
    | A.Primitive (_, q, body) ->
      A.option_contains_def p q;
      let snapshot = match q with None -> snapshot | Some q ->
        entry_lookup p q (F.readback (trees q)) snapshot; Entry (q, F.readback (trees q), snapshot) in
      record_lookup h trees p body snapshot
    | A.Let_binding (rhs, body) ->
      record_lookup h trees p rhs snapshot;
      record_lookup h trees p body (record h trees rhs snapshot))

