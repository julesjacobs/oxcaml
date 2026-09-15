open Copy_spec
open Copy_heap_proofs

let (put_scope @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | payload_scoped h v} ->
    {u : unit | if H.mem (H.put h p v) x then source_ok (H.put h p v) x else H.at (H.put h p v) x === None}
    @ ghost = fun h scope p v x premise -> ghost_ (
  let refine_ premise = premise in scope x; payload_scoped_def h v;
  let after = H.put h p v in source_ok_def after x; source_ok_def h x;
  put_frame h p v x; let u = () in
  if x === p then (
    (match v.desc with Var | Bool -> () | Link q -> put_frame h p v q; ()
      | Arrow (a, b) -> put_frame h p v a; put_frame h p v b; ());
    (match v.memo with Empty_memo -> () | Memo (stamp, _) -> put_frame h p v stamp; ()); refine_ u)
  else (match H.at h x with None -> refine_ u | Some old ->
    (match old.desc with Var | Bool -> () | Link q -> put_frame h p v q; ()
      | Arrow (a, b) -> put_frame h p v a; put_frame h p v b; ());
    (match old.memo with Empty_memo -> () | Memo (stamp, _) -> put_frame h p v stamp; ()); refine_ u))

let rec (epoch_allocated @ total) : (saved : node Pref.heap) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | H.mem (heap saved epoch depth d) epoch} @ ghost = fun saved epoch depth d premise -> ghost_ (
  let refine_ premise = premise in valid_def saved epoch depth d; heap_def saved epoch depth d;
  let u = () in match d with
  | Start -> let v = cell Bool depth in put_frame saved epoch v epoch; refine_ u
  | Fresh (rest, p, q, old, desc) ->
    epoch_allocated saved epoch depth rest (refine_ u);
    let h = heap saved epoch depth rest in let v = cell desc depth in put_frame h q v epoch;
    let h1 = H.put h q v in let w = mark old epoch q in put_frame h1 p w epoch; refine_ u
  | Alias (rest, p, q, old) -> epoch_allocated saved epoch depth rest (refine_ u);
    let h = heap saved epoch depth rest in let w = mark old epoch q in put_frame h p w epoch; refine_ u)

let (ready_scoped @ total) : (saved : node Pref.heap) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> (source : desc) @ immutable -> (dest : desc) @ immutable ->
    {u : unit | valid saved epoch depth d && ready saved d source dest} ->
    {u : unit | payload_scoped (heap saved epoch depth d) (cell dest depth)} @ ghost =
  fun saved epoch depth d source dest premise -> ghost_ (
    let refine_ premise = premise in ready_def saved d source dest;
    let h = heap saved epoch depth d in let v = cell dest depth in cell_def dest depth; payload_scoped_def h v;
    let u = () in match source, dest with
    | Arrow (a, b), Arrow (x, y) -> target_allocated saved epoch depth d a x (refine_ u);
      target_allocated saved epoch depth d b y (refine_ u); refine_ u
    | _ -> refine_ u)

let rec (history_scope @ total) : (saved : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | if H.mem (heap saved epoch depth d) x then source_ok (heap saved epoch depth d) x
      else H.at (heap saved epoch depth d) x === None} @ ghost = fun saved scope epoch depth d x premise -> ghost_ (
  let refine_ premise = premise in valid_def saved epoch depth d; heap_def saved epoch depth d;
  let u = () in match d with
  | Start -> let desc = Bool in let v = cell desc depth in cell_def desc depth; payload_scoped_def saved v;
    let refine_ u = put_scope saved scope epoch v x (refine_ u) in refine_ u
  | Fresh (rest, p, q, old, desc) ->
    let h = heap saved epoch depth rest in
    let prior : (x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}
        @ total = fun x -> let u = () in let refine_ u = history_scope saved scope epoch depth rest x (refine_ u) in refine_ u in
    ready_scoped saved epoch depth rest old.desc desc (refine_ u);
    let v = cell desc depth in let h1 = H.put h q v in
    let next : (x : node Pref.t) @ immutable -> {u : unit | if H.mem h1 x then source_ok h1 x else H.at h1 x === None}
        @ total = fun x -> let u = () in let refine_ u = put_scope h prior q v x (refine_ u) in refine_ u in
    history_grows saved epoch depth rest p (refine_ u); prior p; source_ok_def h p;
    epoch_allocated saved epoch depth rest (refine_ u);
    let w = mark old epoch q in mark_def old epoch q; payload_scoped_def h1 w;
    let refine_ u = put_scope h1 next p w x (refine_ u) in refine_ u
  | Alias (rest, p, q, old) ->
    let h = heap saved epoch depth rest in
    let prior : (x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}
        @ total = fun x -> let u = () in let refine_ u = history_scope saved scope epoch depth rest x (refine_ u) in refine_ u in
    history_grows saved epoch depth rest p (refine_ u); prior p; source_ok_def h p;
    epoch_allocated saved epoch depth rest (refine_ u);
    let w = mark old epoch q in mark_def old epoch q; payload_scoped_def h w;
    let refine_ u = put_scope h prior p w x (refine_ u) in refine_ u)

let[@def] (describes @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (desc : desc @ immutable) (value : ty @ immutable) = ghost_ (match desc with
  | Var -> true | Bool -> value === Boolean | Arrow (a, b) -> value === Function (rho a, rho b)
  | Link q -> value === rho q)
let (allocation_model @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable -> (value : ty) @ immutable ->
    (tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (update : ((x : node Pref.t) @ immutable -> {u : unit | tau x === (if x === p then value else rho x)})) @ total ->
    (x : node Pref.t) @ immutable ->
    {u : unit | not (H.mem h p) && payload_scoped h v && describes rho v.desc value} ->
    {u : unit | equation (H.put h p v) tau x} @ ghost = fun h scope rho model p v value tau update x premise -> ghost_ (
  let refine_ premise = premise in payload_scoped_def h v; describes_def rho v.desc value;
  let after = H.put h p v in equation_def after tau x; update x;
  let u = () in if x === p then (
    match v.desc with Var | Bool -> refine_ u | Link q -> update q; refine_ u
    | Arrow (a, b) -> update a; update b; refine_ u)
  else (scope x; source_ok_def h x; model x; equation_def h rho x;
    match H.at h x with None -> refine_ u | Some old -> match old.desc with
    | Var | Bool -> refine_ u | Link q -> update q; refine_ u | Arrow (a, b) -> update a; update b; refine_ u))
let (with_allocation_model @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable -> (value : ty) @ immutable ->
    {u : unit | not (H.mem h p) && payload_scoped h v && describes rho v.desc value} -> (claim : bool) ->
    (use : ((tau : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
      (next : ((x : node Pref.t) @ immutable -> {u : unit | equation (H.put h p v) tau x})) @ total ->
      (equal : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x})) @ total ->
      {u : unit | tau p === value} -> {u : unit | claim})) @ total -> {u : unit | claim} @ ghost =
  fun h scope rho model p v value premise claim use -> ghost_ (
    let refine_ premise = premise in
    let[@def] tau : node Pref.t @ immutable total -> ty @ immutable total = fun x -> if x === p then value else rho x in
    let update : (x : node Pref.t) @ immutable -> {u : unit | tau x === (if x === p then value else rho x)}
        @ total = fun x -> tau_def x; let u = () in refine_ u in
    let next : (x : node Pref.t) @ immutable -> {u : unit | equation (H.put h p v) tau x}
        @ total = fun x -> let u = () in let refine_ u = allocation_model h scope rho model p v value tau update x (refine_ u) in refine_ u in
    let equal : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}
        @ total = fun x -> tau_def x; let u = () in refine_ u in
    tau_def p; let u = () in let refine_ u = use tau next equal (refine_ u) in refine_ u)
let (mark_model @ total) : (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (old : node) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.at h p === Some old} -> {u : unit | equation (H.put h p (mark old epoch q)) rho x}
    @ ghost = fun h rho model p old epoch q x premise -> ghost_ (
  let refine_ premise = premise in let after = H.put h p (mark old epoch q) in
  mark_def old epoch q; model x; equation_def h rho x; equation_def after rho x; let u = () in refine_ u)

let (restrict_model @ total) : (saved : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | equation saved rho x} @ ghost = fun saved scope epoch depth d rho model x premise -> ghost_ (
  let refine_ premise = premise in let u = () in history_at saved epoch depth d x (refine_ u); scope x;
  let after = heap saved epoch depth d in model x; equation_def after rho x; equation_def saved rho x; refine_ u)
