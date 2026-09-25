module I = Vox_iarray
module F = Vox_egraph_origin_frame
module L = Vox_egraph_language_spec
module LP = Vox_egraph_language_proof
module N = Vox_egraph_rule_node
module G = Vox_egraph_rule_union
module S = Vox_egraph_rule_semantics
module E = Vox_egraph_derivation_spec
module EP = Vox_egraph_derivation
module M = Vox_egraph_union_spec

type t = {
  semantic : G.t;
  nodes : N.t option iarray;
  sorts : L.sort iarray;
}

let[@def] (node_ok_data @ total)
    (nodes : N.t option iarray @ immutable)
    (sorts : L.sort iarray @ immutable)
    (origins : L.expr iarray @ immutable) (id : int) = ghost_ (
  match I.at nodes id with
  | Some (Some node) ->
    N.children_below id node &&
    N.well_typed sorts id node &&
    I.at sorts id === Some (N.sort node) &&
    S.origin origins id === N.origin origins node &&
    L.sort (S.origin origins id) ===
      Some (N.sort node)
  | _ -> false)

let[@def] (node_ok @ total) (state : t @ immutable) (id : int) = ghost_ (
  node_ok_data state.nodes state.sorts state.semantic.origins id)

let[@def] rec (nodes_valid_data @ total)
    (nodes : N.t option iarray @ immutable)
    (sorts : L.sort iarray @ immutable)
    (origins : L.expr iarray @ immutable) (count : int) = ghost_ (
  if count <= 0 then true
  else node_ok_data nodes sorts origins (count - 1) &&
    nodes_valid_data nodes sorts origins (count - 1))
  [@@decreases if count > 0 then count else 0]

let[@def] (nodes_valid @ total) (state : t @ immutable)
    (count : int) = ghost_ (
  nodes_valid_data state.nodes state.sorts state.semantic.origins count)

let[@def] valid (state : t @ immutable) = ghost_ (
  G.valid state.semantic &&
  Iarray.length state.nodes = 512 &&
  Iarray.length state.sorts = 512 &&
  nodes_valid state state.semantic.union.count)

let rec (node_at @ total) :
    (state : t) @ immutable -> (count : int) -> (id : int) ->
    {u : unit | nodes_valid state count && 0 <= id && id < count} ->
    {u : unit | node_ok state id} @ ghost =
  fun state count id premise -> ghost_ (
    nodes_valid_def state count;
    nodes_valid_data_def state.nodes state.sorts
      state.semantic.origins count;
    if id < count - 1 then (
      nodes_valid_def state (count - 1);
      node_at state (count - 1) id ());
    node_ok_def state id;
    ())
  [@@decreases if count > 0 then count else 0]

let rec (node_at_data @ total) :
    (nodes : N.t option iarray) @ immutable ->
    (sorts : L.sort iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (count : int) -> (id : int) ->
    {u : unit | nodes_valid_data nodes sorts origins count &&
      0 <= id && id < count} ->
    {u : unit | node_ok_data nodes sorts origins id} @ ghost =
  fun nodes sorts origins count id premise -> ghost_ (
    nodes_valid_data_def nodes sorts origins count;
    if id < count - 1 then
      node_at_data nodes sorts origins (count - 1) id ();
    ())
  [@@decreases if count > 0 then count else 0]

let (child_origin_sort @ total) :
    (nodes : N.t option iarray) @ immutable ->
    (sorts : L.sort iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (count : int) -> (id : int) -> (expected : L.sort) ->
    {u : unit | nodes_valid_data nodes sorts origins count &&
      N.child_has_sort sorts count id expected} ->
    {u : unit | L.sort (S.origin origins id) === Some expected}
    @ ghost = fun nodes sorts origins count id expected premise -> ghost_ (
  N.child_has_sort_def sorts count id expected;
  node_at_data nodes sorts origins count id ();
  node_ok_data_def nodes sorts origins id;
  match I.at nodes id with
  | Some (Some node) ->
    N.sort_def node;
    ()
  | _ -> ())

let (node_origin_typed @ total) :
    (nodes : N.t option iarray) @ immutable ->
    (sorts : L.sort iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (count : int) -> (node : N.t) @ immutable ->
    {u : unit | nodes_valid_data nodes sorts origins count &&
      N.well_typed sorts count node} ->
    {u : unit | L.sort (N.origin origins node) === Some (N.sort node)}
    @ ghost = fun nodes sorts origins count node premise -> ghost_ (
  N.well_typed_def sorts count node;
  N.child_origins_typed_def origins node;
  (match node with
   | N.Int_lit _ | N.Bool_lit _ | N.Int_input | N.Bool_input -> ()
   | N.Add (a, b) | N.Eq_int (a, b) ->
     child_origin_sort nodes sorts origins count a L.Integer ();
     child_origin_sort nodes sorts origins count b L.Integer ()
   | N.Int_if (c, a, b) ->
     child_origin_sort nodes sorts origins count c L.Boolean ();
     child_origin_sort nodes sorts origins count a L.Integer ();
     child_origin_sort nodes sorts origins count b L.Integer ()
   | N.Bool_if (c, a, b) ->
     child_origin_sort nodes sorts origins count c L.Boolean ();
     child_origin_sort nodes sorts origins count a L.Boolean ();
     child_origin_sort nodes sorts origins count b L.Boolean ());
  N.origin_typed origins node ();
  ())

let (node_ok_frame @ total) :
    (nodes : N.t option iarray) @ immutable ->
    (sorts : L.sort iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (index : int) -> (node : N.t) @ immutable ->
    (expr : L.expr) @ immutable -> (id : int) ->
    {u : unit | 0 <= id && id < index &&
      node_ok_data nodes sorts origins id} ->
    {u : unit | node_ok_data
      (I.updated nodes index (Some node))
      (I.updated sorts index (N.sort node))
      (I.updated origins index expr) id} @ ghost =
  fun nodes sorts origins index node expr id premise -> ghost_ (
    let changed_nodes = I.updated nodes index (Some node) in
    let changed_sorts = I.updated sorts index (N.sort node) in
    let changed_origins = I.updated origins index expr in
    node_ok_data_def nodes sorts origins id;
    node_ok_data_def changed_nodes changed_sorts changed_origins id;
    I.updated_read nodes index (Some node) id;
    I.updated_read sorts index (N.sort node) id;
    I.updated_read origins index expr id;
    S.origin_def origins id;
    S.origin_def changed_origins id;
    (match I.at nodes id with
     | Some (Some old_node) ->
       N.children_below_def id old_node;
       N.children_below_def index old_node;
       N.origin_frame origins index expr old_node ();
       N.well_typed_frame sorts index (N.sort node) id old_node ();
       ()
     | _ -> ());
    ())

let rec (nodes_valid_frame @ total) :
    (nodes : N.t option iarray) @ immutable ->
    (sorts : L.sort iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (count : int) -> (index : int) ->
    (node : N.t) @ immutable -> (expr : L.expr) @ immutable ->
    {u : unit | 0 <= count && count <= index &&
      nodes_valid_data nodes sorts origins count} ->
    {u : unit | nodes_valid_data
      (I.updated nodes index (Some node))
      (I.updated sorts index (N.sort node))
      (I.updated origins index expr) count} @ ghost =
  fun nodes sorts origins count index node expr premise -> ghost_ (
    let changed_nodes = I.updated nodes index (Some node) in
    let changed_sorts = I.updated sorts index (N.sort node) in
    let changed_origins = I.updated origins index expr in
    nodes_valid_data_def nodes sorts origins count;
    nodes_valid_data_def changed_nodes changed_sorts changed_origins count;
    if count > 0 then (
      node_ok_frame nodes sorts origins index node expr (count - 1) ();
      nodes_valid_frame nodes sorts origins (count - 1) index node expr ());
    ())
  [@@decreases if count > 0 then count else 0]

let (node_ok_appended @ total) :
    (nodes : N.t option iarray) @ immutable ->
    (sorts : L.sort iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (count : int) -> (node : N.t) @ immutable ->
    {u : unit | 0 <= count && count < Iarray.length nodes &&
      count < Iarray.length sorts && count < Iarray.length origins &&
      nodes_valid_data nodes sorts origins count &&
      N.well_typed sorts count node} ->
    {u : unit | node_ok_data
      (I.updated nodes count (Some node))
      (I.updated sorts count (N.sort node))
      (I.updated origins count (N.origin origins node)) count} @ ghost =
  fun nodes sorts origins count node premise -> ghost_ (
    let expr = N.origin origins node in
    let changed_nodes = I.updated nodes count (Some node) in
    let changed_sorts = I.updated sorts count (N.sort node) in
    let changed_origins = I.updated origins count expr in
    N.well_typed_children sorts count node ();
    node_origin_typed nodes sorts origins count node ();
    N.origin_frame origins count expr node ();
    N.well_typed_frame sorts count (N.sort node) count node ();
    I.updated_read nodes count (Some node) count;
    I.updated_read sorts count (N.sort node) count;
    I.updated_read origins count expr count;
    S.origin_def changed_origins count;
    node_ok_data_def changed_nodes changed_sorts changed_origins count;
    ())

let (nodes_valid_appended @ total) :
    (nodes : N.t option iarray) @ immutable ->
    (sorts : L.sort iarray) @ immutable ->
    (origins : L.expr iarray) @ immutable ->
    (count : int) -> (node : N.t) @ immutable ->
    {u : unit | 0 <= count && count < Iarray.length nodes &&
      count < Iarray.length sorts && count < Iarray.length origins &&
      nodes_valid_data nodes sorts origins count &&
      N.well_typed sorts count node} ->
    {u : unit | nodes_valid_data
      (I.updated nodes count (Some node))
      (I.updated sorts count (N.sort node))
      (I.updated origins count (N.origin origins node)) (count + 1)}
    @ ghost = fun nodes sorts origins count node premise -> ghost_ (
  let expr = N.origin origins node in
  let changed_nodes = I.updated nodes count (Some node) in
  let changed_sorts = I.updated sorts count (N.sort node) in
  let changed_origins = I.updated origins count expr in
  node_ok_appended nodes sorts origins count node ();
  nodes_valid_frame nodes sorts origins count count node expr ();
  nodes_valid_data_def changed_nodes changed_sorts changed_origins (count + 1);
  ())

type add_result = #{value : int option; state : t}

let create : (rules : Vox_egraph_rule_spec.t) @ immutable ghost ->
    {state : t | valid state && state.semantic.union.count = 0 &&
      state.semantic.rules === rules}
    @ immutable = fun rules ->
  let semantic = G.create rules in
  let nodes = Iarray.init 512 (fun _ -> (None : N.t option)) in
  let sorts = Iarray.init 512 (fun _ -> L.Integer) in
  let state = {semantic; nodes; sorts} in
  ghost_ (
    nodes_valid_data_def nodes sorts semantic.origins 0;
    nodes_valid_def state 0;
    valid_def state);
  state

let add : (state : {s : t | valid s}) @ immutable ->
    (node : N.t) @ immutable ->
    {r : add_result | valid r.#state && r.#state.semantic.rules === state.semantic.rules &&
      F.preserved state.semantic.origins r.#state.semantic.origins
        state.semantic.union.count &&
      (match r.#value with
       | None -> r.#state === state &&
         (not (N.well_typed state.sorts state.semantic.union.count node) ||
           state.semantic.union.count = 512)
       | Some id -> id = state.semantic.union.count &&
         r.#state.semantic.union.count = id + 1 &&
         r.#state.semantic.origins === I.updated
           state.semantic.origins id (N.origin state.semantic.origins node) &&
         r.#state.nodes === I.updated state.nodes id (Some node) &&
         r.#state.sorts === I.updated state.sorts id (N.sort node) &&
         I.at r.#state.nodes id === Some (Some node) &&
         I.at r.#state.sorts id === Some (N.sort node) &&
         S.origin r.#state.semantic.origins id ===
           N.origin state.semantic.origins node)}
    @ immutable = fun state node ->
  ghost_ (valid_def state;
    F.identity state.semantic.origins state.semantic.union.count);
  let count = state.semantic.union.count in
  if not (N.well_typed state.sorts count node) then
    #{value = None; state}
  else
    let expr = ghost_ (N.origin state.semantic.origins node) in
    let #{G.value; state = semantic} = G.add state.semantic expr in
    match value with
    | None -> #{value = None; state}
    | Some id ->
      let nodes = I.updated state.nodes id (Some node) in
      let sorts = I.updated state.sorts id (N.sort node) in
      ghost_ (
        G.valid_def state.semantic;
        Vox_egraph_union.valid_def state.semantic.union;
        nodes_valid_def state count;
        nodes_valid_appended state.nodes state.sorts
          state.semantic.origins count node ();
        I.updated_length state.nodes id (Some node);
        I.updated_length state.sorts id (N.sort node);
        I.updated_read state.nodes id (Some node) id;
        I.updated_read state.sorts id (N.sort node) id;
        S.origin_def semantic.origins id;
        F.append state.semantic.origins count id expr ());
      let next = {semantic; nodes; sorts} in
      ghost_ (nodes_valid_def next semantic.union.count; valid_def next);
      #{value = Some id; state = next}

let rec (extract_origin @ total) :
    (state : {s : t | valid s}) @ immutable ->
    (id : {i : int | 0 <= i && i < state.semantic.union.count}) ->
    {expr : L.expr | expr === S.origin state.semantic.origins id}
      @ immutable = fun state id ->
  let count = state.semantic.union.count in
  ghost_ (
    valid_def state;
    nodes_valid_def state count;
    node_at_data state.nodes state.sorts state.semantic.origins
      count id ();
    node_ok_data_def state.nodes state.sorts state.semantic.origins id);
  match I.at state.nodes id with
  | Some (Some node) ->
    ghost_ (N.children_below_def id node);
    let result = match node with
      | N.Int_lit value -> L.Int_lit value
      | N.Bool_lit value -> L.Bool_lit value
      | N.Int_input -> L.Int_input
      | N.Bool_input -> L.Bool_input
      | N.Add (a, b) ->
        L.Add (extract_origin state a, extract_origin state b)
      | N.Eq_int (a, b) ->
        L.Eq_int (extract_origin state a, extract_origin state b)
      | N.Int_if (c, a, b) ->
        L.Int_if (extract_origin state c, extract_origin state a,
          extract_origin state b)
      | N.Bool_if (c, a, b) ->
        L.Bool_if (extract_origin state c, extract_origin state a,
          extract_origin state b) in
    ghost_ (N.origin_def state.semantic.origins node);
    result
  | _ -> L.Int_input
  [@@decreases if id > 0 then id else 0]

let (add_congruence @ total) :
    (state : t) @ immutable ->
    (left : int) -> (right : int) ->
    (a : int) -> (b : int) -> (c : int) -> (d : int) ->
    {u : unit | valid state &&
      0 <= left && left < state.semantic.union.count &&
      0 <= right && right < state.semantic.union.count &&
      I.at state.nodes left === Some (Some (N.Add (a, b))) &&
      I.at state.nodes right === Some (Some (N.Add (c, d))) &&
      M.root state.semantic.union.parents a =
        M.root state.semantic.union.parents c &&
      M.root state.semantic.union.parents b =
        M.root state.semantic.union.parents d} ->
    {proof : E.evidence | E.valid state.semantic.rules proof &&
      E.left proof === S.origin state.semantic.origins left &&
      E.right proof === S.origin state.semantic.origins right}
      @ ghost = fun state left right a b c d premise -> ghost_ (
  let count = state.semantic.union.count in
  let parents = state.semantic.union.parents in
  let origins = state.semantic.origins in
  valid_def state;
  G.valid_def state.semantic;
  Vox_egraph_union.valid_def state.semantic.union;
  nodes_valid_def state count;
  node_at_data state.nodes state.sorts origins count left ();
  node_at_data state.nodes state.sorts origins count right ();
  node_ok_data_def state.nodes state.sorts origins left;
  node_ok_data_def state.nodes state.sorts origins right;
  N.well_typed_def state.sorts left (N.Add (a, b));
  N.well_typed_def state.sorts right (N.Add (c, d));
  N.children_below_def left (N.Add (a, b));
  N.children_below_def right (N.Add (c, d));
  N.child_has_sort_def state.sorts left a L.Integer;
  N.child_has_sort_def state.sorts left b L.Integer;
  N.child_has_sort_def state.sorts right c L.Integer;
  N.child_has_sort_def state.sorts right d L.Integer;
  N.child_has_sort_def state.sorts count a L.Integer;
  N.child_has_sort_def state.sorts count b L.Integer;
  N.child_has_sort_def state.sorts count c L.Integer;
  N.child_has_sort_def state.sorts count d L.Integer;
  child_origin_sort state.nodes state.sorts origins count a L.Integer ();
  child_origin_sort state.nodes state.sorts origins count b L.Integer ();
  child_origin_sort state.nodes state.sorts origins count c L.Integer ();
  child_origin_sort state.nodes state.sorts origins count d L.Integer ();
  let first = S.same_class_evidence state.semantic.rules parents origins state.semantic.edges
    count a c () in
  let second = S.same_class_evidence state.semantic.rules parents origins state.semantic.edges
    count b d () in
  let proof = EP.add_congruence state.semantic.rules first second () in
  N.origin_def origins (N.Add (a, b));
  N.origin_def origins (N.Add (c, d));
  proof)

let (eq_congruence @ total) :
    (state : t) @ immutable ->
    (left : int) -> (right : int) ->
    (a : int) -> (b : int) -> (c : int) -> (d : int) ->
    {u : unit | valid state &&
      0 <= left && left < state.semantic.union.count &&
      0 <= right && right < state.semantic.union.count &&
      I.at state.nodes left === Some (Some (N.Eq_int (a, b))) &&
      I.at state.nodes right === Some (Some (N.Eq_int (c, d))) &&
      M.root state.semantic.union.parents a =
        M.root state.semantic.union.parents c &&
      M.root state.semantic.union.parents b =
        M.root state.semantic.union.parents d} ->
    {proof : E.evidence | E.valid state.semantic.rules proof &&
      E.left proof === S.origin state.semantic.origins left &&
      E.right proof === S.origin state.semantic.origins right}
      @ ghost = fun state left right a b c d premise -> ghost_ (
  let count = state.semantic.union.count in
  let parents = state.semantic.union.parents in
  let origins = state.semantic.origins in
  valid_def state;
  G.valid_def state.semantic;
  Vox_egraph_union.valid_def state.semantic.union;
  nodes_valid_def state count;
  node_at_data state.nodes state.sorts origins count left ();
  node_at_data state.nodes state.sorts origins count right ();
  node_ok_data_def state.nodes state.sorts origins left;
  node_ok_data_def state.nodes state.sorts origins right;
  N.well_typed_def state.sorts left (N.Eq_int (a, b));
  N.well_typed_def state.sorts right (N.Eq_int (c, d));
  N.children_below_def left (N.Eq_int (a, b));
  N.children_below_def right (N.Eq_int (c, d));
  N.child_has_sort_def state.sorts left a L.Integer;
  N.child_has_sort_def state.sorts left b L.Integer;
  N.child_has_sort_def state.sorts right c L.Integer;
  N.child_has_sort_def state.sorts right d L.Integer;
  N.child_has_sort_def state.sorts count a L.Integer;
  N.child_has_sort_def state.sorts count b L.Integer;
  N.child_has_sort_def state.sorts count c L.Integer;
  N.child_has_sort_def state.sorts count d L.Integer;
  child_origin_sort state.nodes state.sorts origins count a L.Integer ();
  child_origin_sort state.nodes state.sorts origins count b L.Integer ();
  child_origin_sort state.nodes state.sorts origins count c L.Integer ();
  child_origin_sort state.nodes state.sorts origins count d L.Integer ();
  let first = S.same_class_evidence state.semantic.rules parents origins state.semantic.edges
    count a c () in
  let second = S.same_class_evidence state.semantic.rules parents origins state.semantic.edges
    count b d () in
  let proof = EP.eq_congruence state.semantic.rules first second () in
  N.origin_def origins (N.Eq_int (a, b));
  N.origin_def origins (N.Eq_int (c, d));
  proof)

let (int_if_congruence @ total) :
    (state : t) @ immutable ->
    (left : int) -> (right : int) ->
    (c1 : int) -> (y1 : int) -> (n1 : int) ->
    (c2 : int) -> (y2 : int) -> (n2 : int) ->
    {u : unit | valid state &&
      0 <= left && left < state.semantic.union.count &&
      0 <= right && right < state.semantic.union.count &&
      I.at state.nodes left === Some (Some (N.Int_if (c1, y1, n1))) &&
      I.at state.nodes right === Some (Some (N.Int_if (c2, y2, n2))) &&
      M.root state.semantic.union.parents c1 =
        M.root state.semantic.union.parents c2 &&
      M.root state.semantic.union.parents y1 =
        M.root state.semantic.union.parents y2 &&
      M.root state.semantic.union.parents n1 =
        M.root state.semantic.union.parents n2} ->
    {proof : E.evidence | E.valid state.semantic.rules proof &&
      E.left proof === S.origin state.semantic.origins left &&
      E.right proof === S.origin state.semantic.origins right}
      @ ghost = fun state left right c1 y1 n1 c2 y2 n2 premise -> ghost_ (
  let count = state.semantic.union.count in
  let parents = state.semantic.union.parents in
  let origins = state.semantic.origins in
  valid_def state;
  G.valid_def state.semantic;
  Vox_egraph_union.valid_def state.semantic.union;
  nodes_valid_def state count;
  node_at_data state.nodes state.sorts origins count left ();
  node_at_data state.nodes state.sorts origins count right ();
  node_ok_data_def state.nodes state.sorts origins left;
  node_ok_data_def state.nodes state.sorts origins right;
  N.well_typed_def state.sorts left (N.Int_if (c1, y1, n1));
  N.well_typed_def state.sorts right (N.Int_if (c2, y2, n2));
  N.children_below_def left (N.Int_if (c1, y1, n1));
  N.children_below_def right (N.Int_if (c2, y2, n2));
  N.child_has_sort_def state.sorts left c1 L.Boolean;
  N.child_has_sort_def state.sorts left y1 L.Integer;
  N.child_has_sort_def state.sorts left n1 L.Integer;
  N.child_has_sort_def state.sorts right c2 L.Boolean;
  N.child_has_sort_def state.sorts right y2 L.Integer;
  N.child_has_sort_def state.sorts right n2 L.Integer;
  N.child_has_sort_def state.sorts count c1 L.Boolean;
  N.child_has_sort_def state.sorts count y1 L.Integer;
  N.child_has_sort_def state.sorts count n1 L.Integer;
  N.child_has_sort_def state.sorts count c2 L.Boolean;
  N.child_has_sort_def state.sorts count y2 L.Integer;
  N.child_has_sort_def state.sorts count n2 L.Integer;
  child_origin_sort state.nodes state.sorts origins count c1 L.Boolean ();
  child_origin_sort state.nodes state.sorts origins count y1 L.Integer ();
  child_origin_sort state.nodes state.sorts origins count n1 L.Integer ();
  child_origin_sort state.nodes state.sorts origins count c2 L.Boolean ();
  child_origin_sort state.nodes state.sorts origins count y2 L.Integer ();
  child_origin_sort state.nodes state.sorts origins count n2 L.Integer ();
  let condition = S.same_class_evidence state.semantic.rules parents origins state.semantic.edges
    count c1 c2 () in
  let yes = S.same_class_evidence state.semantic.rules parents origins state.semantic.edges
    count y1 y2 () in
  let no = S.same_class_evidence state.semantic.rules parents origins state.semantic.edges
    count n1 n2 () in
  let proof = EP.int_if_congruence state.semantic.rules condition yes no () in
  N.origin_def origins (N.Int_if (c1, y1, n1));
  N.origin_def origins (N.Int_if (c2, y2, n2));
  proof)

let (bool_if_congruence @ total) :
    (state : t) @ immutable ->
    (left : int) -> (right : int) ->
    (c1 : int) -> (y1 : int) -> (n1 : int) ->
    (c2 : int) -> (y2 : int) -> (n2 : int) ->
    {u : unit | valid state &&
      0 <= left && left < state.semantic.union.count &&
      0 <= right && right < state.semantic.union.count &&
      I.at state.nodes left === Some (Some (N.Bool_if (c1, y1, n1))) &&
      I.at state.nodes right === Some (Some (N.Bool_if (c2, y2, n2))) &&
      M.root state.semantic.union.parents c1 =
        M.root state.semantic.union.parents c2 &&
      M.root state.semantic.union.parents y1 =
        M.root state.semantic.union.parents y2 &&
      M.root state.semantic.union.parents n1 =
        M.root state.semantic.union.parents n2} ->
    {proof : E.evidence | E.valid state.semantic.rules proof &&
      E.left proof === S.origin state.semantic.origins left &&
      E.right proof === S.origin state.semantic.origins right}
      @ ghost = fun state left right c1 y1 n1 c2 y2 n2 premise -> ghost_ (
  let count = state.semantic.union.count in
  let parents = state.semantic.union.parents in
  let origins = state.semantic.origins in
  valid_def state;
  G.valid_def state.semantic;
  Vox_egraph_union.valid_def state.semantic.union;
  nodes_valid_def state count;
  node_at_data state.nodes state.sorts origins count left ();
  node_at_data state.nodes state.sorts origins count right ();
  node_ok_data_def state.nodes state.sorts origins left;
  node_ok_data_def state.nodes state.sorts origins right;
  N.well_typed_def state.sorts left (N.Bool_if (c1, y1, n1));
  N.well_typed_def state.sorts right (N.Bool_if (c2, y2, n2));
  N.children_below_def left (N.Bool_if (c1, y1, n1));
  N.children_below_def right (N.Bool_if (c2, y2, n2));
  N.child_has_sort_def state.sorts left c1 L.Boolean;
  N.child_has_sort_def state.sorts left y1 L.Boolean;
  N.child_has_sort_def state.sorts left n1 L.Boolean;
  N.child_has_sort_def state.sorts right c2 L.Boolean;
  N.child_has_sort_def state.sorts right y2 L.Boolean;
  N.child_has_sort_def state.sorts right n2 L.Boolean;
  N.child_has_sort_def state.sorts count c1 L.Boolean;
  N.child_has_sort_def state.sorts count y1 L.Boolean;
  N.child_has_sort_def state.sorts count n1 L.Boolean;
  N.child_has_sort_def state.sorts count c2 L.Boolean;
  N.child_has_sort_def state.sorts count y2 L.Boolean;
  N.child_has_sort_def state.sorts count n2 L.Boolean;
  child_origin_sort state.nodes state.sorts origins count c1 L.Boolean ();
  child_origin_sort state.nodes state.sorts origins count y1 L.Boolean ();
  child_origin_sort state.nodes state.sorts origins count n1 L.Boolean ();
  child_origin_sort state.nodes state.sorts origins count c2 L.Boolean ();
  child_origin_sort state.nodes state.sorts origins count y2 L.Boolean ();
  child_origin_sort state.nodes state.sorts origins count n2 L.Boolean ();
  let condition = S.same_class_evidence state.semantic.rules parents origins state.semantic.edges
    count c1 c2 () in
  let yes = S.same_class_evidence state.semantic.rules parents origins state.semantic.edges
    count y1 y2 () in
  let no = S.same_class_evidence state.semantic.rules parents origins state.semantic.edges
    count n1 n2 () in
  let proof = EP.bool_if_congruence state.semantic.rules condition yes no () in
  N.origin_def origins (N.Bool_if (c1, y1, n1));
  N.origin_def origins (N.Bool_if (c2, y2, n2));
  proof)

let[@def] (collision @ total) (state : t @ immutable)
    (left : int) (right : int) = ghost_ (
  match I.at state.nodes left, I.at state.nodes right with
  | Some (Some first), Some (Some second) ->
    N.signature state.semantic.union.parents first ===
      N.signature state.semantic.union.parents second
  | _ -> false)

let[@def] (pair_closed @ total) (state : t @ immutable)
    (left : int) (right : int) = ghost_ (
  not (collision state left right) ||
  M.root state.semantic.union.parents left =
    M.root state.semantic.union.parents right)

let[@def] rec (closed_fuel @ total) (state : t @ immutable)
    (left : int) (right : int) (fuel : int) = ghost_ (
  if left = state.semantic.union.count then true
  else if fuel <= 0 then false
  else if right = state.semantic.union.count then
    closed_fuel state (left + 1) 0 (fuel - 1)
  else pair_closed state left right &&
    closed_fuel state left (right + 1) (fuel - 1))
  [@@decreases if fuel > 0 then fuel else 0]

let (same_origin_evidence @ total) :
    (state : t) @ immutable -> (left : int) -> (right : int) ->
    {u : unit | valid state &&
      0 <= left && left < state.semantic.union.count &&
      S.origin state.semantic.origins left ===
        S.origin state.semantic.origins right} ->
    {proof : E.evidence | E.valid state.semantic.rules proof &&
      E.left proof === S.origin state.semantic.origins left &&
      E.right proof === S.origin state.semantic.origins right}
      @ ghost = fun state left right premise -> ghost_ (
  let count = state.semantic.union.count in
  valid_def state;
  nodes_valid_def state count;
  node_at_data state.nodes state.sorts state.semantic.origins
    count left ();
  node_ok_data_def state.nodes state.sorts state.semantic.origins left;
  let proof = E.Refl (S.origin state.semantic.origins left) in
  E.valid_def state.semantic.rules proof;
  E.left_def proof;
  E.right_def proof;
  E.endpoints_def proof;
  proof)

let (collision_evidence @ total) :
    (state : t) @ immutable -> (left : int) -> (right : int) ->
    {u : unit | valid state &&
      0 <= left && left < state.semantic.union.count &&
      0 <= right && right < state.semantic.union.count &&
      collision state left right} ->
    {proof : E.evidence | E.valid state.semantic.rules proof &&
      E.left proof === S.origin state.semantic.origins left &&
      E.right proof === S.origin state.semantic.origins right}
      @ ghost = fun state left right premise -> ghost_ (
  let parents = state.semantic.union.parents in
  let origins = state.semantic.origins in
  collision_def state left right;
  valid_def state;
  nodes_valid_def state state.semantic.union.count;
  node_at_data state.nodes state.sorts origins
    state.semantic.union.count left ();
  node_at_data state.nodes state.sorts origins
    state.semantic.union.count right ();
  node_ok_data_def state.nodes state.sorts origins left;
  node_ok_data_def state.nodes state.sorts origins right;
  match I.at state.nodes left, I.at state.nodes right with
  | Some (Some first), Some (Some second) ->
    N.signature_def parents first;
    N.signature_def parents second;
    N.key_exact (N.canonical parents first)
      (N.canonical parents second);
    N.canonical_def parents first;
    N.canonical_def parents second;
    (match first, second with
     | N.Add (a, b), N.Add (c, d) ->
       add_congruence state left right a b c d ()
     | N.Eq_int (a, b), N.Eq_int (c, d) ->
       eq_congruence state left right a b c d ()
     | N.Int_if (c1, y1, n1), N.Int_if (c2, y2, n2) ->
       int_if_congruence state left right c1 y1 n1 c2 y2 n2 ()
     | N.Bool_if (c1, y1, n1), N.Bool_if (c2, y2, n2) ->
       bool_if_congruence state left right c1 y1 n1 c2 y2 n2 ()
     | _ ->
       N.origin_def origins first;
       N.origin_def origins second;
       same_origin_evidence state left right ())
  | _ -> (E.Refl (S.origin origins left)))


let (child_typed @ total) :
    (state : t) @ immutable -> (id : int) -> (expected : L.sort) ->
    {u : unit | valid state && 0 <= id &&
      id < state.semantic.union.count &&
      L.sort (S.origin state.semantic.origins id) === Some expected} ->
    {u : unit | N.child_has_sort state.sorts
      state.semantic.union.count id expected} @ ghost =
  fun state id expected premise -> ghost_ (
    valid_def state;
    nodes_valid_def state state.semantic.union.count;
    node_at_data state.nodes state.sorts state.semantic.origins
      state.semantic.union.count id ();
    node_ok_data_def state.nodes state.sorts state.semantic.origins id;
    N.child_has_sort_def state.sorts state.semantic.union.count id expected;
    ())
