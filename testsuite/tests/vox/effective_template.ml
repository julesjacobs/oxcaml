open Copy_spec
module E = Effective_level
module U = Level_unifier_spec

let[@def] (finite @ total) (h : node Pref.heap @ immutable)
    (heads : E.heads @ total) (p : node Pref.t @ immutable) = ghost_ (
  H.mem h p && match E.level h heads p with Generic -> false | Finite _ -> true)

let[@def] (generic @ total) (h : node Pref.heap @ immutable)
    (heads : E.heads @ total) (p : node Pref.t @ immutable) = ghost_ (
  H.mem h p && E.level h heads p === Generic)

let[@def] rec (valid_template @ total) (h : node Pref.heap @ immutable)
    (heads : E.heads @ total) (t : template @ immutable) = ghost_ (
  H.mem h (root t) && match t with
  | Boundary p -> finite h heads p
  | Parameter p -> generic h heads p && U.observe h p === Some Var
  | Constant p -> generic h heads p && U.observe h p === Some Bool
  | Word_constant p -> generic h heads p && U.observe h p === Some Word
  | List_template (p, child) -> generic h heads p && U.observe h p === Some (List (root child))
    && valid_template h heads child
  | Product (p, a, b) -> generic h heads p && U.observe h p === Some (Arrow (root a, root b))
    && valid_template h heads a && valid_template h heads b
  | Indirect (p, child) -> generic h heads p && U.observe h p === Some (Link (root child))
    && valid_template h heads child)

let (head @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (t : template) @ immutable ->
    {u : unit | valid_template h heads t} ->
    {u : unit | if head_generic t then generic h heads (root t)
      && U.observe h (root t) === Some (head_desc t)
      else finite h heads (root t)} @ ghost = fun h heads t premise -> ghost_ (
    valid_template_def h heads t;
    root_def t; head_desc_def t; head_generic_def t;
    ())

let rec (unique @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (a : template) @ immutable -> (b : template) @ immutable ->
    {u : unit | valid_template h heads a && valid_template h heads b && root a === root b} ->
    {u : unit | interpret rho choices a === interpret rho choices b} @ ghost =
  fun h heads rho choices a b premise -> ghost_ (
    head h heads a (); head h heads b ();
    valid_template_def h heads a; valid_template_def h heads b;
    let pa = root a in let pb = root b in
    finite_def h heads pa; finite_def h heads pb;
    generic_def h heads pa; generic_def h heads pb;
    root_def a; root_def b; head_desc_def a; head_desc_def b;
    head_generic_def a; head_generic_def b;
    interpret_def rho choices a; interpret_def rho choices b;
    match a with
    | Product (_, a1, a2) -> (match b with Product (_, b1, b2) ->
      unique h heads rho choices a1 b1 ();
      unique h heads rho choices a2 b2 (); () | _ -> ())
    | List_template (_, child) -> (match b with List_template (_, other) ->
      unique h heads rho choices child other (); () | _ -> ())
    | Indirect (_, child) -> (match b with Indirect (_, other) ->
      unique h heads rho choices child other (); () | _ -> ())
    | _ -> ())

let[@def] rec (boundary_bound @ total) (h : node Pref.heap @ immutable)
    (heads : E.heads @ total) (depth : int) (t : template @ immutable) = ghost_ (
  match t with
  | Boundary p -> E.effective_below h heads p depth
  | Parameter _ | Constant _ | Word_constant _ -> true
  | Product (_, a, b) -> boundary_bound h heads depth a && boundary_bound h heads depth b
  | Indirect (_, child) | List_template (_, child) -> boundary_bound h heads depth child)

let[@def] (protected @ total) (h : node Pref.heap @ immutable)
    (heads : E.heads @ total) (after : node Pref.heap @ immutable)
    (next : E.heads @ total) (depth : int) (p : node Pref.t @ immutable) = ghost_ (
  (not (H.mem h p) || H.mem after p)
  && (not (E.effective_below h heads p depth) || E.effective_below after next p depth)
  && (not (generic h heads p) || generic after next p && U.observe h p === U.observe after p))

let rec (transport @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (after : node Pref.heap) @ immutable ->
    (next : E.heads) @ total -> (depth : int) ->
    (frame : ((p : node Pref.t) @ immutable ->
      {u : unit | protected h heads after next depth p})) @ total ->
    (t : template) @ immutable ->
    {u : unit | valid_template h heads t && boundary_bound h heads depth t} ->
    {u : unit | valid_template after next t && boundary_bound after next depth t} @ ghost =
  fun h heads after next depth frame t premise -> ghost_ (
    valid_template_def h heads t;
    valid_template_def after next t; boundary_bound_def h heads depth t;
    boundary_bound_def after next depth t; root_def t;
    let p = root t in frame p; protected_def h heads after next depth p;
    finite_def after next p; E.effective_below_def after next p depth;
    match t with
    | Boundary _ | Parameter _ | Constant _ | Word_constant _ -> ()
    | Product (_, a, b) -> transport h heads after next depth frame a ();
      transport h heads after next depth frame b (); ()
    | Indirect (_, child) | List_template (_, child) -> transport h heads after next depth frame child (); ())

let (protected_trans @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (mid : node Pref.heap) @ immutable ->
    (middle : E.heads) @ total -> (after : node Pref.heap) @ immutable ->
    (next : E.heads) @ total -> (depth : int) -> (p : node Pref.t) @ immutable ->
    {u : unit | protected h heads mid middle depth p && protected mid middle after next depth p} ->
    {u : unit | protected h heads after next depth p} @ ghost =
  fun h heads mid middle after next depth p premise -> ghost_ (
    protected_def h heads mid middle depth p;
    protected_def mid middle after next depth p; protected_def h heads after next depth p;
    ())

let (close_protected @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (cut : int) ->
    (pool : Generalize_spec.pool) @ immutable -> (depth : int) ->
    (p : node Pref.t) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped h pool && E.valid_head h heads p && depth <= cut} ->
    {u : unit | protected h heads (Representative_pool_spec.close_heap h cut pool) heads depth p} @ ghost =
  fun h heads cut pool depth p premise -> ghost_ (
    let after = Representative_pool_spec.close_heap h cut pool in
    Representative_pool_spec.close_heap_def h cut pool;
    let filtered = Representative_level.representatives h pool in Representative_level.representatives_scoped h pool ();
    Generalize_proofs.closed_observe h cut filtered p ();
    Generalize_spec.closed_at_def h after cut filtered p;
    U.observe_def h p; U.observe_def after p;
    protected_def h heads after heads depth p;
    if E.effective_below h heads p depth then
      (E.closed_boundary h heads cut pool p depth (); ()) else ();
    generic_def h heads p; generic_def after heads p;
    if generic h heads p then (
      E.level_def h heads p;
      let r = heads p in
      Representative_level.representative_covered_def h cut pool r.root;
      Generalize_spec.covered_def h cut pool r.root;
      E.closed_level h heads cut pool p ();
      let g = Generic in Generalize_spec.close_level_def cut g; ());
    ())

let[@def] rec (scheme @ total) (h : node Pref.heap @ immutable)
    (heads : E.heads @ total) (cut : int) (tree : Level_finite_spec.tree @ immutable) = ghost_ (
  let p = Level_finite_spec.tree_root tree in
  if not (Generalize_spec.close_level cut (E.level h heads p) === Generic) then Boundary p else
  match tree with
  | Level_finite_spec.Free p -> Parameter p
  | Level_finite_spec.Constant_tree p -> Constant p
  | Level_finite_spec.Word_tree p -> Word_constant p
  | Level_finite_spec.List_tree (p, child) -> List_template (p, scheme h heads cut child)
  | Level_finite_spec.Branch (p, a, b) -> Product (p, scheme h heads cut a, scheme h heads cut b)
  | Level_finite_spec.Alias_tree (p, child) -> Indirect (p, scheme h heads cut child))

let rec (scheme_root @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (cut : int) -> (tree : Level_finite_spec.tree) @ immutable ->
    {u : unit | root (scheme h heads cut tree) === Level_finite_spec.tree_root tree} @ ghost =
  fun h heads cut tree -> ghost_ (
    scheme_def h heads cut tree; Level_finite_spec.tree_root_def tree;
    let result = scheme h heads cut tree in root_def result;
    ())

let rec (scheme_valid @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x})) @ total ->
    (cut : int) -> (pool : Generalize_spec.pool) @ immutable ->
    (coverage : ((x : node Pref.t) @ immutable ->
      {u : unit | Representative_level.representative_covered h cut pool x})) @ total ->
    (tree : Level_finite_spec.tree) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped h pool && Level_finite_spec.finite h tree} ->
    {u : unit | valid_template (Representative_pool_spec.close_heap h cut pool) heads
      (scheme h heads cut tree)} @ ghost =
  fun h heads witness cut pool coverage tree premise -> ghost_ (
    Level_finite_spec.finite_def h tree; Level_finite_spec.tree_root_def tree;
    let p = Level_finite_spec.tree_root tree in
    let after = Representative_pool_spec.close_heap h cut pool in
    witness p; let r = heads p in coverage r.root;
    E.closed_level h heads cut pool p ();
    Representative_pool_spec.close_heap_def h cut pool;
    let filtered = Representative_level.representatives h pool in
    Representative_level.representatives_scoped h pool ();
    Generalize_proofs.closed_observe h cut filtered p ();
    Generalize_spec.closed_at_def h after cut filtered p;
    U.observe_def h p; U.observe_def after p;
    scheme_def h heads cut tree; scheme_root h heads cut tree;
    let output = scheme h heads cut tree in valid_template_def after heads output;
    finite_def after heads p; generic_def after heads p;
    if not (Generalize_spec.close_level cut (E.level h heads p) === Generic) then ()
    else (match tree with
    | Level_finite_spec.Free _ | Level_finite_spec.Constant_tree _ | Level_finite_spec.Word_tree _ -> ()
    | Level_finite_spec.Alias_tree (_, child) | Level_finite_spec.List_tree (_, child) ->
      scheme_root h heads cut child;
      scheme_valid h heads witness cut pool coverage child (); ()
    | Level_finite_spec.Branch (_, a, b) ->
      scheme_root h heads cut a; scheme_root h heads cut b;
      scheme_valid h heads witness cut pool coverage a ();
      scheme_valid h heads witness cut pool coverage b (); ()))

let rec (scheme_boundary @ total) : (h : node Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (cut : int) ->
    (pool : Generalize_spec.pool) @ immutable ->
    (witness : ((x : node Pref.t) @ immutable -> {u : unit | E.valid_head h heads x})) @ total ->
    (levels : ((x : node Pref.t) @ immutable ->
      {u : unit | match E.level h heads x with Generic -> true | Finite n -> n >= 0})) @ total ->
    (tree : Level_finite_spec.tree) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped h pool && Level_finite_spec.finite h tree} ->
    {u : unit | boundary_bound (Representative_pool_spec.close_heap h cut pool) heads cut
      (scheme h heads cut tree)} @ ghost =
  fun h heads cut pool witness levels tree premise -> ghost_ (
    let p = Level_finite_spec.tree_root tree in let output = scheme h heads cut tree in
    let after = Representative_pool_spec.close_heap h cut pool in
    Level_finite_spec.finite_def h tree; Level_finite_spec.tree_root_def tree;
    scheme_def h heads cut tree; boundary_bound_def after heads cut output;
    let old = E.level h heads p in Generalize_spec.close_level_def cut old;
    if not (Generalize_spec.close_level cut old === Generic) then (
      levels p; witness p; E.effective_below_def h heads p cut;
      E.closed_boundary h heads cut pool p cut (); ())
    else (match tree with
    | Level_finite_spec.Free _ | Level_finite_spec.Constant_tree _ | Level_finite_spec.Word_tree _ -> ()
    | Level_finite_spec.Alias_tree (_, child) | Level_finite_spec.List_tree (_, child) ->
      scheme_boundary h heads cut pool witness levels child (); ()
    | Level_finite_spec.Branch (_, a, b) ->
      scheme_boundary h heads cut pool witness levels a ();
      scheme_boundary h heads cut pool witness levels b (); ()))
