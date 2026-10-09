open Marked_occurs_proofs
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
    let flag = false in searched_def h p x flag trace;
    let after = H.put h p (redirect h p q) in
    let v = redirect h p q in redirect_desc h p q; observe_write h p v x;
    match trace with
    | Hit | Left _ -> let t = Free x in t
    | Leaf ->
      if observe h x === Some Var then (
        let t = Free x in tree_root_def t; finite_def after t; t)
      else (
        let t = Constant_tree x in tree_root_def t; finite_def after t; t)
    | Follow (y, rest) ->
      let child = search_finite h p q y rest () in
      let t = Alias_tree (x, child) in tree_root_def t; finite_def after t; t
    | Both (a, b, left, right) ->
      let ta = search_finite h p q a left () in
      let tb = search_finite h p q b right () in
      let t = Branch (x, ta, tb) in tree_root_def t; finite_def after t; t)

let rec (replace_free @ total) :
    (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (target : tree) @ immutable -> (old : tree) @ immutable ->
    {u : unit | observe h p === Some Var && finite h old
      && tree_root target === q && finite (H.put h p (redirect h p q)) target} ->
    {t : tree | tree_root t === tree_root old && finite (H.put h p (redirect h p q)) t}
      @ immutable ghost = fun h p q target old premise -> ghost_ (
    finite_def h old; tree_root_def old;
    let after = H.put h p (redirect h p q) in
    let v = redirect h p q in let x = tree_root old in redirect_desc h p q; observe_write h p v x;
    match old with
    | Free x ->
      if x === p then (
        let t = Alias_tree (x, target) in tree_root_def t; finite_def after t; t)
      else (finite_def after old; old)
    | Constant_tree _ -> finite_def after old; old
    | Alias_tree (x, child) ->
      let child = replace_free h p q target child () in
      let t = Alias_tree (x, child) in tree_root_def t; finite_def after t; t
    | Branch (x, a, b) ->
      let a = replace_free h p q target a () in
      let b = replace_free h p q target b () in
      let t = Branch (x, a, b) in tree_root_def t; finite_def after t; t)

let rec (lower_finite @ total) : (h : node Pref.heap) @ immutable -> (bound : int) ->
    (edits : lowering) @ immutable -> (t : tree) @ immutable ->
    {u : unit | lower_valid h bound edits && finite h t} ->
    {u : unit | finite (lower_heap h bound edits) t} @ ghost = fun h bound edits t premise -> ghost_ (
  finite_def h t; tree_root_def t;
  let after = lower_heap h bound edits in finite_def after t; let x = tree_root t in
  lower_observe h bound edits x (); match t with
  | Free _ | Constant_tree _ -> ()
  | Alias_tree (_, c) -> lower_finite h bound edits c (); ()
  | Branch (_, a, b) -> lower_finite h bound edits a (); lower_finite h bound edits b (); ())

let rec (scan_finite @ total) : (h : node Pref.heap) @ immutable ->
    (needle : node Pref.t) @ immutable -> (d : marks) @ immutable ->
    (t : tree) @ immutable -> {u : unit | marks_valid h needle d && finite h t} ->
    {u : unit | finite (scan_heap h d) t} @ ghost = fun h needle d t premise -> ghost_ (
  finite_def h t; tree_root_def t;
  let after = scan_heap h d in finite_def after t; let x = tree_root t in
  scan_observe h needle d x (); match t with
  | Free _ | Constant_tree _ -> ()
  | Alias_tree (_, c) -> scan_finite h needle d c (); ()
  | Branch (_, a, b) -> scan_finite h needle d a ();
    scan_finite h needle d b (); ())

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
    unified_def h p q ok after d;
    let old = trees x in
    match d with
    | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> old
    | Bind_left trace ->
      let v = redirect h p q in observe_write h p v x;
      if H.mem h x then (
        let target = search_finite h p q q trace () in
        let t = replace_free h p q target old () in t)
      else old
    | Bind_right trace ->
      let v = redirect h q p in observe_write h q v x;
      if H.mem h x then (
        let target = search_finite h q p p trace () in
        let t = replace_free h q p target old () in t)
      else old
    | Scanned (needle, marks, rest) ->
      let mid = scan_heap h marks in
      let middle_trees : (x : node Pref.t) @ immutable ->
          {t : tree | tree_root t === x && (if H.mem mid x then finite mid t else observe mid x === None)} @ immutable total =
        fun x -> let t = trees x in scan_observe h needle marks x ();
          if H.mem h x then (scan_finite h needle marks t (); t) else t in
      unified_finite_at mid middle_trees p q ok after rest x ()
    | Lowering (bound, edits, _, rest) ->
      let mid = lower_heap h bound edits in
      let middle_trees : (x : node Pref.t) @ immutable ->
          {t : tree | tree_root t === x && (if H.mem mid x then finite mid t else observe mid x === None)} @ immutable total =
        fun x -> let t = trees x in lower_observe h bound edits x ();
          if H.mem h x then (lower_finite h bound edits t (); t) else t in
      unified_finite_at mid middle_trees p q ok after rest x ()
    | Swap rest -> unified_finite_at h trees q p ok after rest x ()
    | Resolve (r, s, _, _, rest) ->
      unified_finite_at h trees r s ok after rest x ()
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let middle_trees : (x : node Pref.t) @ immutable ->
          {t : tree | tree_root t === x && (if H.mem middle x then finite middle t else observe middle x === None)}
          @ immutable total = fun x ->
        let t = unified_finite_at h trees a c left_ok middle left x () in
        t in
      if left_ok then
        unified_finite_at middle middle_trees b e ok after right x ()
      else let t = middle_trees x in t)

let rec (size_positive @ total) : (t : tree) @ immutable ->
    {u : unit | size t > Bigint.zero} @ ghost = fun t -> ghost_ (
  size_def t;
  match t with
  | Free _ | Constant_tree _ -> ()
  | Alias_tree (_, child) -> size_positive child; ()
  | Branch (_, a, b) -> size_positive a; size_positive b; ())

let rec (finite_unique @ total) :
    (h : node Pref.heap) @ immutable -> (a : tree) @ immutable -> (b : tree) @ immutable ->
    {u : unit | finite h a && finite h b && tree_root a === tree_root b} ->
    {u : unit | a === b} @ ghost = fun h a b premise -> ghost_ (
  finite_def h a; finite_def h b; tree_root_def a; tree_root_def b;
  match a, b with
  | Alias_tree (_, child), Alias_tree (_, other) ->
    finite_unique h child other (); ()
  | Branch (_, left, right), Branch (_, other_left, other_right) ->
    finite_unique h left other_left ();
    finite_unique h right other_right (); ()
  | _ -> ())

let (edge_smaller @ total) :
    (h : node Pref.heap) @ immutable -> (a : tree) @ immutable -> (b : tree) @ immutable ->
    {u : unit | finite h a && finite h b && edge h (tree_root a) (tree_root b)} ->
    {u : unit | size b < size a} @ ghost = fun h a b premise -> ghost_ (
  finite_def h a; tree_root_def a;
  let ra = tree_root a in let rb = tree_root b in edge_def h ra rb; size_def a;
  match a with
  | Free _ | Constant_tree _ -> ()
  | Alias_tree (_, child) -> finite_unique h child b (); ()
  | Branch (_, left, right) ->
    size_positive left; size_positive right;
    if tree_root left === tree_root b then (
      finite_unique h left b (); ())
    else (finite_unique h right b (); ()))

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
    walks_def h p q w;
    let tp = trees p in finite_def h tp; tree_root_def tp;
    match w with
    | Stop -> ()
    | Step (next, rest) ->
      edge_def h p next;
      (match tp with
       | Free _ | Constant_tree _ -> ()
       | Alias_tree (_, child) -> finite_def h child; ()
       | Branch (_, a, b) -> finite_def h a; finite_def h b; ());
      let tn = trees next in
      edge_smaller h tp tn ();
      walk_bound h trees next q rest ();
      ())

let (no_cycle @ total) :
    (h : node Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | tree_root t === x && (if H.mem h x then finite h t else observe h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (w : walk) @ immutable ->
    {u : unit | H.mem h p && walks h p p w && not (w === Stop)} ->
    {u : unit | false} @ ghost = fun h trees p w premise -> ghost_ (
  walk_bound h trees p p w (); ())

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
  let t = trees x in finite_def h t; tree_root_def t;
  agrees x; readback_def t;
  if H.mem h x then (
    match t with
    | Free _ | Constant_tree _ -> ()
    | Alias_tree (_, child) ->
      finite_def h child;
      let y = tree_root child in let ty = trees y in
      finite_unique h child ty (); agrees y; ()
    | Branch (_, a, b) ->
      finite_def h a; finite_def h b;
      let y = tree_root a in let z = tree_root b in
      let ta = trees y in let tb = trees z in
      finite_unique h a ta (); finite_unique h b tb ();
      agrees y; agrees z; ())
  else ())


let rec (allocation_frame @ total) :
    (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (t : tree) @ immutable ->
    {u : unit | not (H.mem h p) && finite h t} ->
    {u : unit | finite (H.put h p v) t} @ ghost = fun h p v t premise -> ghost_ (
  finite_def h t; tree_root_def t;
  let after = H.put h p v in let x = tree_root t in observe_write h p v x; finite_def after t;
  match t with
  | Free _ | Constant_tree _ -> ()
  | Alias_tree (_, child) -> allocation_frame h p v child (); ()
  | Branch (_, a, b) ->
    allocation_frame h p v a ();
    allocation_frame h p v b (); ())

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
    allocatable_def h v;
    let after = H.put h p v in observe_write h p v x; if x === p then (
      match v.desc with
      | Var -> let t = Free p in tree_root_def t; finite_def after t; t
      | Bool -> let t = Constant_tree p in tree_root_def t; finite_def after t; t
      | Link q ->
        let child = trees q in allocation_frame h p v child ();
        let t = Alias_tree (p, child) in tree_root_def t; finite_def after t; t
      | Arrow (a, b) ->
        let ta = trees a in let tb = trees b in
        allocation_frame h p v ta (); allocation_frame h p v tb ();
        let t = Branch (p, ta, tb) in tree_root_def t; finite_def after t; t)
    else (
      let t = trees x in
      if H.mem h x then (allocation_frame h p v t (); t)
      else t))

let (finite_scope_at @ total) :
    (h : node Pref.heap) @ immutable -> (t : tree) @ immutable ->
    {u : unit | finite h t} ->
    {u : unit | scoped h (tree_root t)} @ ghost = fun h t premise -> ghost_ (
  finite_def h t; tree_root_def t;
  let x = tree_root t in scoped_def h x;
  match t with
  | Free _ | Constant_tree _ -> ()
  | Alias_tree (_, child) -> finite_def h child; ()
  | Branch (_, a, b) -> finite_def h a; finite_def h b; ())

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
    let t = trees x in readback t in
  let agrees : (x : node Pref.t) @ immutable ->
      {u : unit | let refine_ t = trees x in not (H.mem h x) || rho x === readback t}
      @ total = fun x -> rho_def x; () in
  let model : (x : node Pref.t) @ immutable -> {u : unit | node_equation h rho x}
      @ total = fun x ->
    let () = readback_model_at h trees rho agrees x in () in
  let () = use rho model in ())
