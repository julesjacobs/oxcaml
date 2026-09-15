open Copy_spec
open Level_spec
open Level_unifier_spec
open Level_unifier_proofs
open Level_finite_spec

let rec (search_finite @ total) :
    (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> (trace : search) @ immutable ->
    {u : unit | searched h p x false trace} ->
    {t : tree | tree_root t === x && finite (H.put h p (redirect h p q)) t} @ immutable ghost =
  fun h p q x trace premise -> ghost_ (
    let refine_ premise = premise in
    let flag = false in searched_def h p x flag trace;
    let after = H.put h p (redirect h p q) in
    let v = redirect h p q in redirect_desc h p q; observe_write h p v x;
    let u = () in
    match trace with
    | Hit | Left _ -> let t = Free x in refine_ t
    | Leaf ->
      if observe h x === Some Var then (
        let t = Free x in tree_root_def t; finite_def after t; refine_ t)
      else (
        let t = Constant_tree x in tree_root_def t; finite_def after t; refine_ t)
    | Follow (y, rest) ->
      let refine_ child = search_finite h p q y rest (refine_ u) in
      let t = Alias_tree (x, child) in tree_root_def t; finite_def after t; refine_ t
    | Both (a, b, left, right) ->
      let refine_ ta = search_finite h p q a left (refine_ u) in
      let refine_ tb = search_finite h p q b right (refine_ u) in
      let t = Branch (x, ta, tb) in tree_root_def t; finite_def after t; refine_ t)

let rec (replace_free @ total) :
    (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (target : tree) @ immutable -> (old : tree) @ immutable ->
    {u : unit | observe h p === Some Var && finite h old
      && tree_root target === q && finite (H.put h p (redirect h p q)) target} ->
    {t : tree | tree_root t === tree_root old && finite (H.put h p (redirect h p q)) t}
      @ immutable ghost = fun h p q target old premise -> ghost_ (
    let refine_ premise = premise in
    finite_def h old; tree_root_def old;
    let after = H.put h p (redirect h p q) in
    let v = redirect h p q in let x = tree_root old in redirect_desc h p q; observe_write h p v x;
    let u = () in
    match old with
    | Free x ->
      if x === p then (
        let t = Alias_tree (x, target) in tree_root_def t; finite_def after t; refine_ t)
      else (finite_def after old; refine_ old)
    | Constant_tree _ -> finite_def after old; refine_ old
    | Alias_tree (x, child) ->
      let refine_ child = replace_free h p q target child (refine_ u) in
      let t = Alias_tree (x, child) in tree_root_def t; finite_def after t; refine_ t
    | Branch (x, a, b) ->
      let refine_ a = replace_free h p q target a (refine_ u) in
      let refine_ b = replace_free h p q target b (refine_ u) in
      let t = Branch (x, a, b) in tree_root_def t; finite_def after t; refine_ t)

let rec (lower_finite @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (edits : lowering) @ immutable -> (t : tree) @ immutable ->
    {u : unit | lower_valid h bound edits && finite h t} ->
    {u : unit | finite (lower_heap h bound edits) t} @ ghost = fun h bound edits t premise -> ghost_ (
  let refine_ premise = premise in finite_def h t; tree_root_def t;
  let after = lower_heap h bound edits in finite_def after t; let x = tree_root t in
  let u = () in lower_observe h bound edits x (refine_ u); match t with
  | Free _ | Constant_tree _ -> refine_ u
  | Alias_tree (_, c) -> lower_finite h bound edits c (refine_ u); refine_ u
  | Branch (_, a, b) -> lower_finite h bound edits a (refine_ u); lower_finite h bound edits b (refine_ u); refine_ u)

let rec (unified_finite_at @ total) :
    (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : node Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {t : tree | tree_root t === x && (if H.mem after x then finite after t else observe after x === None)}
      @ immutable ghost = fun h trees p q ok after d x premise -> ghost_ (
    let refine_ premise = premise in
    unified_def h p q ok after d;
    let u = () in
    let refine_ old = trees x in
    match d with
    | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> refine_ old
    | Bind_left trace ->
      let v = redirect h p q in observe_write h p v x;
      if H.mem h x then (
        let refine_ target = search_finite h p q q trace (refine_ u) in
        let refine_ t = replace_free h p q target old (refine_ u) in refine_ t)
      else refine_ old
    | Bind_right trace ->
      let v = redirect h q p in observe_write h q v x;
      if H.mem h x then (
        let refine_ target = search_finite h q p p trace (refine_ u) in
        let refine_ t = replace_free h q p target old (refine_ u) in refine_ t)
      else refine_ old
    | Lowering (bound, edits, _, rest) ->
      let mid = lower_heap h bound edits in
      let middle_trees : (x : node Pref.t) @ immutable ->
          {t : tree | tree_root t === x && (if H.mem mid x then finite mid t else observe mid x === None)} @ immutable total =
        fun x -> let refine_ t = trees x in let u = () in lower_observe h bound edits x (refine_ u);
          if H.mem h x then (lower_finite h bound edits t (refine_ u); refine_ t) else refine_ t in
      unified_finite_at mid middle_trees p q ok after rest x (refine_ u)
    | Swap rest -> unified_finite_at h trees q p ok after rest x (refine_ u)
    | Resolve (r, s, _, _, rest) ->
      unified_finite_at h trees r s ok after rest x (refine_ u)
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let middle_trees : (x : node Pref.t) @ immutable ->
          {t : tree | tree_root t === x && (if H.mem middle x then finite middle t else observe middle x === None)}
          @ immutable total = fun x ->
        let u = () in
        let refine_ t = unified_finite_at h trees a c left_ok middle left x (refine_ u) in
        refine_ t in
      if left_ok then
        unified_finite_at middle middle_trees b e ok after right x (refine_ u)
      else let refine_ t = middle_trees x in refine_ t)

let rec (size_positive @ total) : (t : tree) @ immutable ->
    {u : unit | size t > Bigint.zero} @ ghost = fun t -> ghost_ (
  size_def t;
  let u = () in
  match t with
  | Free _ | Constant_tree _ -> refine_ u
  | Alias_tree (_, child) -> size_positive child; refine_ u
  | Branch (_, a, b) -> size_positive a; size_positive b; refine_ u)

let rec (finite_unique @ total) :
    (h : node Pref.heap) @ immutable -> (a : tree) @ immutable -> (b : tree) @ immutable ->
    {u : unit | finite h a && finite h b && tree_root a === tree_root b} ->
    {u : unit | a === b} @ ghost = fun h a b premise -> ghost_ (
  let refine_ premise = premise in
  finite_def h a; finite_def h b; tree_root_def a; tree_root_def b;
  let u = () in
  match a with
  | Alias_tree (_, child) ->
    (match b with
     | Alias_tree (_, other) -> finite_unique h child other (refine_ u); refine_ u
     | _ -> refine_ u)
  | Branch (_, left, right) ->
    (match b with
     | Branch (_, other_left, other_right) ->
       finite_unique h left other_left (refine_ u);
       finite_unique h right other_right (refine_ u); refine_ u
     | _ -> refine_ u)
  | Free _ | Constant_tree _ -> refine_ u)

let (edge_smaller @ total) :
    (h : node Pref.heap) @ immutable -> (a : tree) @ immutable -> (b : tree) @ immutable ->
    {u : unit | finite h a && finite h b && edge h (tree_root a) (tree_root b)} ->
    {u : unit | size b < size a} @ ghost = fun h a b premise -> ghost_ (
  let refine_ premise = premise in
  finite_def h a; tree_root_def a;
  let ra = tree_root a in let rb = tree_root b in edge_def h ra rb; size_def a;
  let u = () in
  match a with
  | Free _ | Constant_tree _ -> refine_ u
  | Alias_tree (_, child) -> finite_unique h child b (refine_ u); refine_ u
  | Branch (_, left, right) ->
    size_positive left; size_positive right;
    if tree_root left === tree_root b then (
      finite_unique h left b (refine_ u); refine_ u)
    else (finite_unique h right b (refine_ u); refine_ u))

let rec (walk_bound @ total) :
    (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (w : walk) @ immutable ->
    {u : unit | H.mem h p && walks h p q w} ->
    {u : unit | let refine_ tq = trees q in let refine_ tp = trees p in
      H.mem h q && size tq <= size tp
      && (w === Stop || size tq < size tp)} @ ghost =
  fun h trees p q w premise -> ghost_ (
    let refine_ premise = premise in
    walks_def h p q w;
    let refine_ tp = trees p in finite_def h tp; tree_root_def tp;
    let u = () in
    match w with
    | Stop -> refine_ u
    | Step (next, rest) ->
      edge_def h p next;
      (match tp with
       | Free _ | Constant_tree _ -> ()
       | Alias_tree (_, child) -> finite_def h child; ()
       | Branch (_, a, b) -> finite_def h a; finite_def h b; ());
      let refine_ tn = trees next in
      edge_smaller h tp tn (refine_ u);
      walk_bound h trees next q rest (refine_ u);
      refine_ u)

let (no_cycle @ total) :
    (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (w : walk) @ immutable ->
    {u : unit | H.mem h p && walks h p p w && not (w === Stop)} ->
    {u : unit | false} @ ghost = fun h trees p w premise -> ghost_ (
  let refine_ premise = premise in
  let u = () in walk_bound h trees p p w (refine_ u); refine_ u)

let (readback_model_at @ total) :
    (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (agrees : ((x : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees x in not (H.mem h x) || rho x === readback t})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | node_equation h rho x} @ ghost = fun h trees rho agrees x -> ghost_ (
  node_equation_def h rho x;
  let refine_ t = trees x in finite_def h t; tree_root_def t;
  agrees x; readback_def t;
  let u = () in
  if H.mem h x then (
    match t with
    | Free _ | Constant_tree _ -> refine_ u
    | Alias_tree (_, child) ->
      finite_def h child;
      let y = tree_root child in let refine_ ty = trees y in
      finite_unique h child ty (refine_ u); agrees y; refine_ u
    | Branch (_, a, b) ->
      finite_def h a; finite_def h b;
      let y = tree_root a in let z = tree_root b in
      let refine_ ta = trees y in let refine_ tb = trees z in
      finite_unique h a ta (refine_ u); finite_unique h b tb (refine_ u);
      agrees y; agrees z; refine_ u)
  else refine_ u)


let rec (allocation_frame @ total) :
    (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (t : tree) @ immutable ->
    {u : unit | not (H.mem h p) && finite h t} ->
    {u : unit | finite (H.put h p v) t} @ ghost = fun h p v t premise -> ghost_ (
  let refine_ premise = premise in finite_def h t; tree_root_def t;
  let after = H.put h p v in let x = tree_root t in observe_write h p v x; finite_def after t;
  let u = () in
  match t with
  | Free _ | Constant_tree _ -> refine_ u
  | Alias_tree (_, child) -> allocation_frame h p v child (refine_ u); refine_ u
  | Branch (_, a, b) ->
    allocation_frame h p v a (refine_ u);
    allocation_frame h p v b (refine_ u); refine_ u)

let (allocation_finite_at @ total) :
    (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x &&
        (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && allocatable h v} ->
    {t : tree | tree_root t === x &&
      (if H.mem (H.put h p v) x then finite (H.put h p v) t
       else observe (H.put h p v) x === None)} @ immutable ghost =
  fun h trees p v x premise -> ghost_ (
    let refine_ premise = premise in allocatable_def h v;
    let after = H.put h p v in observe_write h p v x; let u = () in
    if x === p then (
      match v.desc with
      | Var -> let t = Free p in tree_root_def t; finite_def after t; refine_ t
      | Bool -> let t = Constant_tree p in tree_root_def t; finite_def after t; refine_ t
      | Link q ->
        let refine_ child = trees q in allocation_frame h p v child (refine_ u);
        let t = Alias_tree (p, child) in tree_root_def t; finite_def after t; refine_ t
      | Arrow (a, b) ->
        let refine_ ta = trees a in let refine_ tb = trees b in
        allocation_frame h p v ta (refine_ u); allocation_frame h p v tb (refine_ u);
        let t = Branch (p, ta, tb) in tree_root_def t; finite_def after t; refine_ t)
    else (
      let refine_ t = trees x in
      if H.mem h x then (allocation_frame h p v t (refine_ u); refine_ t)
      else refine_ t))

let (finite_scope_at @ total) :
    (h : node Pref.heap) @ immutable -> (t : tree) @ immutable ->
    {u : unit | finite h t} ->
    {u : unit | scoped h (tree_root t)} @ ghost = fun h t premise -> ghost_ (
  let refine_ premise = premise in finite_def h t; tree_root_def t;
  let x = tree_root t in scoped_def h x;
  let u = () in
  match t with
  | Free _ | Constant_tree _ -> refine_ u
  | Alias_tree (_, child) -> finite_def h child; refine_ u
  | Branch (_, a, b) -> finite_def h a; finite_def h b; refine_ u)

let (with_finite_model @ total) :
    (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x &&
        (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (claim : bool) ->
    (use : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable ->
        {u : unit | node_equation h rho x})) @ total ->
      {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun h trees claim use -> ghost_ (
  let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
    let refine_ t = trees x in readback t in
  let agrees : (x : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees x in not (H.mem h x) || rho x === readback t}
      @ total = fun x -> rho_def x; let u = () in refine_ u in
  let model : (x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x}
      @ total = fun x ->
    let refine_ u = readback_model_at h trees rho agrees x in refine_ u in
  let refine_ u = use rho model in refine_ u)
