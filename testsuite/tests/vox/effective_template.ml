open Copy_spec
module E = Effective_level
module U = Level_unifier_spec

let[@def] (finite @ total) (h : Pref.heap @ immutable)
    (heads : E.heads @ total) (p : node Pref.t @ immutable) = ghost_ (
  H.mem h p && match E.level h heads p with Generic -> false | Finite _ -> true)

let[@def] (generic @ total) (h : Pref.heap @ immutable)
    (heads : E.heads @ total) (p : node Pref.t @ immutable) = ghost_ (
  H.mem h p && E.level h heads p === Generic)

let[@def] rec (valid_template @ total) (h : Pref.heap @ immutable)
    (heads : E.heads @ total) (t : template @ immutable) = ghost_ (
  H.mem h (root t) && match t with
  | Boundary p -> finite h heads p
  | Parameter p -> generic h heads p && U.observe h p === Some Var
  | Constant p -> generic h heads p && U.observe h p === Some Bool
  | Product (p, a, b) -> generic h heads p && U.observe h p === Some (Arrow (root a, root b))
    && valid_template h heads a && valid_template h heads b
  | Indirect (p, child) -> generic h heads p && U.observe h p === Some (Link (root child))
    && valid_template h heads child)

let (head @ total) : (h : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (t : template) @ immutable ->
    {u : unit | valid_template h heads t} ->
    {u : unit | if head_generic t then generic h heads (root t)
      && U.observe h (root t) === Some (head_desc t)
      else finite h heads (root t)} @ ghost = fun h heads t premise -> ghost_ (
    let refine_ premise = premise in valid_template_def h heads t;
    root_def t; head_desc_def t; head_generic_def t;
    let u = () in refine_ u)

let rec (unique @ total) : (h : Pref.heap) @ immutable ->
    (heads : E.heads) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (choices : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (a : template) @ immutable -> (b : template) @ immutable ->
    {u : unit | valid_template h heads a && valid_template h heads b && root a === root b} ->
    {u : unit | interpret rho choices a === interpret rho choices b} @ ghost =
  fun h heads rho choices a b premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    head h heads a (refine_ u); head h heads b (refine_ u);
    valid_template_def h heads a; valid_template_def h heads b;
    let pa = root a in let pb = root b in
    finite_def h heads pa; finite_def h heads pb;
    generic_def h heads pa; generic_def h heads pb;
    root_def a; root_def b; head_desc_def a; head_desc_def b;
    head_generic_def a; head_generic_def b;
    interpret_def rho choices a; interpret_def rho choices b;
    match a with
    | Product (_, a1, a2) -> (match b with Product (_, b1, b2) ->
      unique h heads rho choices a1 b1 (refine_ u);
      unique h heads rho choices a2 b2 (refine_ u); refine_ u | _ -> refine_ u)
    | Indirect (_, child) -> (match b with Indirect (_, other) ->
      unique h heads rho choices child other (refine_ u); refine_ u | _ -> refine_ u)
    | _ -> refine_ u)

let[@def] rec (boundary_bound @ total) (h : Pref.heap @ immutable)
    (heads : E.heads @ total) (depth : int) (t : template @ immutable) = ghost_ (
  match t with
  | Boundary p -> E.effective_below h heads p depth
  | Parameter _ | Constant _ -> true
  | Product (_, a, b) -> boundary_bound h heads depth a && boundary_bound h heads depth b
  | Indirect (_, child) -> boundary_bound h heads depth child)

let[@def] (protected @ total) (h : Pref.heap @ immutable)
    (heads : E.heads @ total) (after : Pref.heap @ immutable)
    (next : E.heads @ total) (depth : int) (p : node Pref.t @ immutable) = ghost_ (
  (not (H.mem h p) || H.mem after p)
  && (not (E.effective_below h heads p depth) || E.effective_below after next p depth)
  && (not (generic h heads p) || generic after next p && U.observe h p === U.observe after p))

let rec (transport @ total) : (h : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (after : Pref.heap) @ immutable ->
    (next : E.heads) @ total -> (depth : int) ->
    (frame : ((p : node Pref.t) @ immutable ->
      {u : unit | protected h heads after next depth p})) @ total ->
    (t : template) @ immutable ->
    {u : unit | valid_template h heads t && boundary_bound h heads depth t} ->
    {u : unit | valid_template after next t && boundary_bound after next depth t} @ ghost =
  fun h heads after next depth frame t premise -> ghost_ (
    let refine_ premise = premise in valid_template_def h heads t;
    valid_template_def after next t; boundary_bound_def h heads depth t;
    boundary_bound_def after next depth t; root_def t;
    let p = root t in frame p; protected_def h heads after next depth p;
    finite_def after next p; E.effective_below_def after next p depth;
    let u = () in match t with
    | Boundary _ | Parameter _ | Constant _ -> refine_ u
    | Product (_, a, b) -> transport h heads after next depth frame a (refine_ u);
      transport h heads after next depth frame b (refine_ u); refine_ u
    | Indirect (_, child) -> transport h heads after next depth frame child (refine_ u); refine_ u)

let (protected_trans @ total) : (h : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (mid : Pref.heap) @ immutable ->
    (middle : E.heads) @ total -> (after : Pref.heap) @ immutable ->
    (next : E.heads) @ total -> (depth : int) -> (p : node Pref.t) @ immutable ->
    {u : unit | protected h heads mid middle depth p && protected mid middle after next depth p} ->
    {u : unit | protected h heads after next depth p} @ ghost =
  fun h heads mid middle after next depth p premise -> ghost_ (
    let refine_ premise = premise in protected_def h heads mid middle depth p;
    protected_def mid middle after next depth p; protected_def h heads after next depth p;
    let u = () in refine_ u)

let (close_protected @ total) : (h : Pref.heap) @ immutable ->
    (heads : E.heads) @ total -> (cut : int) ->
    (pool : Generalize_spec.pool) @ immutable -> (depth : int) ->
    (p : node Pref.t) @ immutable ->
    {u : unit | Generalize_spec.pool_scoped h pool && E.valid_head h heads p && depth <= cut} ->
    {u : unit | protected h heads (Representative_pool_spec.close_heap h cut pool) heads depth p} @ ghost =
  fun h heads cut pool depth p premise -> ghost_ (
    let refine_ premise = premise in let after = Representative_pool_spec.close_heap h cut pool in
    Representative_pool_spec.close_heap_def h cut pool;
    let filtered = Representative_level.representatives h pool in let u = () in
    Representative_level.representatives_scoped h pool (refine_ u);
    Generalize_proofs.closed_observe h cut filtered p (refine_ u);
    Generalize_spec.closed_at_def h after cut filtered p;
    U.observe_def h p; U.observe_def after p;
    protected_def h heads after heads depth p;
    if E.effective_below h heads p depth then
      (E.closed_boundary h heads cut pool p depth (refine_ u); ()) else ();
    generic_def h heads p; generic_def after heads p;
    if generic h heads p then (
      E.level_def h heads p;
      let r = heads p in
      Representative_level.representative_covered_def h cut pool r.root;
      Generalize_spec.covered_def h cut pool r.root;
      E.closed_level h heads cut pool p (refine_ u);
      let g = Generic in Generalize_spec.close_level_def cut g; ());
    refine_ u)
