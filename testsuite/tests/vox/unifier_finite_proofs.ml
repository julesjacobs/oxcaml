open Unifier_spec
open Unifier_finite_spec

let rec (search_finite @ total) :
    (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (x : node Pref.t) @ immutable -> (trace : search) @ immutable ->
    {u : unit | searched h p x false trace} ->
    {t : tree | root t === x && finite (H.put h p (Link q)) t} @ immutable ghost =
  fun h p q x trace premise -> ghost_ (
    let flag = false in searched_def h p x flag trace;
    let after = H.put h p (Link q) in
    let u = () in
    match trace with
    | Hit | Left _ -> let t = Free x in t
    | Leaf ->
      if H.at h x === Some Var then (
        let t = Free x in root_def t; finite_def after t; t)
      else (
        let t = Boolean x in root_def t; finite_def after t; t)
    | Follow (y, rest) ->
      let child = search_finite h p q y rest (u) in
      let t = Alias (x, child) in root_def t; finite_def after t; t
    | Both (a, b, left, right) ->
      let ta = search_finite h p q a left (u) in
      let tb = search_finite h p q b right (u) in
      let t = Branch (x, ta, tb) in root_def t; finite_def after t; t)

let rec (replace_free @ total) :
    (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (target : tree) @ immutable -> (old : tree) @ immutable ->
    {u : unit | H.at h p === Some Var && finite h old
      && root target === q && finite (H.put h p (Link q)) target} ->
    {t : tree | root t === root old && finite (H.put h p (Link q)) t}
      @ immutable ghost = fun h p q target old premise -> ghost_ (
    finite_def h old; root_def old;
    let after = H.put h p (Link q) in
    let u = () in
    match old with
    | Free x ->
      if x === p then (
        let t = Alias (x, target) in root_def t; finite_def after t; t)
      else (finite_def after old; old)
    | Boolean _ -> finite_def after old; old
    | Alias (x, child) ->
      let child = replace_free h p q target child (u) in
      let t = Alias (x, child) in root_def t; finite_def after t; t
    | Branch (x, a, b) ->
      let a = replace_free h p q target a (u) in
      let b = replace_free h p q target b (u) in
      let t = Branch (x, a, b) in root_def t; finite_def after t; t)

let rec (unified_finite_at @ total) :
    (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x && (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (ok : bool) -> (after : Pref.heap) @ immutable ->
    (d : derivation) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | unified h p q ok after d} ->
    {t : tree | root t === x && (if H.mem after x then finite after t else H.at after x === None)}
      @ immutable ghost = fun h trees p q ok after d x premise -> ghost_ (
    unified_def h p q ok after d;
    let u = () in
    let old = trees x in
    match d with
    | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> old
    | Bind_left trace ->
      if H.mem h x then (
        let target = search_finite h p q q trace (u) in
        let t = replace_free h p q target old (u) in t)
      else old
    | Bind_right trace ->
      if H.mem h x then (
        let target = search_finite h q p p trace (u) in
        let t = replace_free h q p target old (u) in t)
      else old
    | Swap rest -> unified_finite_at h trees q p ok after rest x (u)
    | Resolve (r, s, _, _, rest) ->
      unified_finite_at h trees r s ok after rest x (u)
    | Children (a, b, c, e, middle, left_ok, left, right) ->
      let middle_trees : (x : node Pref.t) @ immutable ->
          {t : tree | root t === x && (if H.mem middle x then finite middle t else H.at middle x === None)}
          @ immutable total = fun x ->
        let u = () in
        let t = unified_finite_at h trees a c left_ok middle left x (u) in
        t in
      if left_ok then
        unified_finite_at middle middle_trees b e ok after right x (u)
      else let t = middle_trees x in t)

let rec (size_positive @ total) : (t : tree) @ immutable ->
    {u : unit | size t > Bigint.zero} @ ghost = fun t -> ghost_ (
  size_def t;
  let u = () in
  match t with
  | Free _ | Boolean _ -> u
  | Alias (_, child) -> size_positive child; u
  | Branch (_, a, b) -> size_positive a; size_positive b; u)

let rec (finite_unique @ total) :
    (h : Pref.heap) @ immutable -> (a : tree) @ immutable -> (b : tree) @ immutable ->
    {u : unit | finite h a && finite h b && root a === root b} ->
    {u : unit | a === b} @ ghost = fun h a b premise -> ghost_ (
  finite_def h a; finite_def h b; root_def a; root_def b;
  let u = () in
  match a with
  | Alias (_, child) ->
    (match b with
     | Alias (_, other) -> finite_unique h child other (u); u
     | _ -> u)
  | Branch (_, left, right) ->
    (match b with
     | Branch (_, other_left, other_right) ->
       finite_unique h left other_left (u);
       finite_unique h right other_right (u); u
     | _ -> u)
  | Free _ | Boolean _ -> u)

let (edge_smaller @ total) :
    (h : Pref.heap) @ immutable -> (a : tree) @ immutable -> (b : tree) @ immutable ->
    {u : unit | finite h a && finite h b && edge h (root a) (root b)} ->
    {u : unit | size b < size a} @ ghost = fun h a b premise -> ghost_ (
  finite_def h a; root_def a;
  let ra = root a in let rb = root b in edge_def h ra rb; size_def a;
  let u = () in
  match a with
  | Free _ | Boolean _ -> u
  | Alias (_, child) -> finite_unique h child b (u); u
  | Branch (_, left, right) ->
    size_positive left; size_positive right;
    if root left === root b then (
      finite_unique h left b (u); u)
    else (finite_unique h right b (u); u))

let rec (walk_bound @ total) :
    (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x && (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (w : walk) @ immutable ->
    {u : unit | H.mem h p && walks h p q w} ->
    {u : unit | let tq = trees q in let tp = trees p in
      H.mem h q && size tq <= size tp
      && (w === Stop || size tq < size tp)} @ ghost =
  fun h trees p q w premise -> ghost_ (
    walks_def h p q w;
    let tp = trees p in finite_def h tp; root_def tp;
    let u = () in
    match w with
    | Stop -> u
    | Step (next, rest) ->
      edge_def h p next;
      (match tp with
       | Free _ | Boolean _ -> ()
       | Alias (_, child) -> finite_def h child; ()
       | Branch (_, a, b) -> finite_def h a; finite_def h b; ());
      let tn = trees next in
      edge_smaller h tp tn (u);
      walk_bound h trees next q rest (u);
      u)

let (no_cycle @ total) :
    (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x && (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (w : walk) @ immutable ->
    {u : unit | H.mem h p && walks h p p w && not (w === Stop)} ->
    {u : unit | false} @ ghost = fun h trees p w premise -> ghost_ (
  let u = () in walk_bound h trees p p w (u); u)

let (readback_model_at @ total) :
    (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x && (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (agrees : ((x : node Pref.t) @ immutable ->
      {u : unit | let t = trees x in not (H.mem h x) || rho x === readback t})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | equation h rho x} @ ghost = fun h trees rho agrees x -> ghost_ (
  equation_def h rho x;
  let t = trees x in finite_def h t; root_def t;
  agrees x; readback_def t;
  let u = () in
  if H.mem h x then (
    match t with
    | Free _ | Boolean _ -> u
    | Alias (_, child) ->
      finite_def h child;
      let y = root child in let ty = trees y in
      finite_unique h child ty (u); agrees y; u
    | Branch (_, a, b) ->
      finite_def h a; finite_def h b;
      let y = root a in let z = root b in
      let ta = trees y in let tb = trees z in
      finite_unique h a ta (u); finite_unique h b tb (u);
      agrees y; agrees z; u)
  else u)


let rec (allocation_frame @ total) :
    (h : Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (t : tree) @ immutable ->
    {u : unit | not (H.mem h p) && finite h t} ->
    {u : unit | finite (H.put h p v) t} @ ghost = fun h p v t premise -> ghost_ (
  finite_def h t; root_def t;
  let after = H.put h p v in finite_def after t;
  let u = () in
  match t with
  | Free _ | Boolean _ -> u
  | Alias (_, child) -> allocation_frame h p v child (u); u
  | Branch (_, a, b) ->
    allocation_frame h p v a (u);
    allocation_frame h p v b (u); u)

let (allocation_finite_at @ total) :
    (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && allocatable h v} ->
    {t : tree | root t === x &&
      (if H.mem (H.put h p v) x then finite (H.put h p v) t
       else H.at (H.put h p v) x === None)} @ immutable ghost =
  fun h trees p v x premise -> ghost_ (
    allocatable_def h v;
    let after = H.put h p v in let u = () in
    if x === p then (
      match v with
      | Var -> let t = Free p in root_def t; finite_def after t; t
      | Bool -> let t = Boolean p in root_def t; finite_def after t; t
      | Link q ->
        let child = trees q in allocation_frame h p v child (u);
        let t = Alias (p, child) in root_def t; finite_def after t; t
      | Arrow (a, b) ->
        let ta = trees a in let tb = trees b in
        allocation_frame h p v ta (u); allocation_frame h p v tb (u);
        let t = Branch (p, ta, tb) in root_def t; finite_def after t; t)
    else (
      let t = trees x in
      if H.mem h x then (allocation_frame h p v t (u); t)
      else t))

let (finite_scope_at @ total) :
    (h : Pref.heap) @ immutable -> (t : tree) @ immutable ->
    {u : unit | finite h t} ->
    {u : unit | scoped h (root t)} @ ghost = fun h t premise -> ghost_ (
  finite_def h t; root_def t;
  let x = root t in scoped_def h x;
  let u = () in
  match t with
  | Free _ | Boolean _ -> u
  | Alias (_, child) -> finite_def h child; u
  | Branch (_, a, b) -> finite_def h a; finite_def h b; u)

let (with_finite_model @ total) :
    (h : Pref.heap) @ immutable ->
    (trees : ((x : node Pref.t) @ immutable ->
      {t : tree | root t === x &&
        (if H.mem h x then finite h t else H.at h x === None)} @ immutable)) @ total ->
    (claim : bool) ->
    (use : ((rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (model : ((x : node Pref.t) @ immutable ->
        {u : unit | equation h rho x})) @ total ->
      {u : unit | claim})) @ total ->
    {u : unit | claim} @ ghost = fun h trees claim use -> ghost_ (
  let[@def] rho : node Pref.t @ immutable total -> ty @ immutable total = fun x ->
    let t = trees x in readback t in
  let agrees : (x : node Pref.t) @ immutable ->
      {u : unit | let t = trees x in not (H.mem h x) || rho x === readback t}
      @ total = fun x -> rho_def x; let u = () in u in
  let model : (x : node Pref.t) @ immutable -> {u : unit | equation h rho x}
      @ total = fun x ->
    let u = readback_model_at h trees rho agrees x in u in
  let u = use rho model in u)
