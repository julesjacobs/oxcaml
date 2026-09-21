open Copy_spec
open Copy_heap_proofs

let (put_scope @ total) : (h : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem h x then source_ok h x else H.at h x === None})) @ total ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | payload_scoped h v} ->
    {u : unit | if H.mem (H.put h p v) x then source_ok (H.put h p v) x else H.at (H.put h p v) x === None}
    @ ghost = fun h scope p v x premise -> ghost_ (
  scope x; payload_scoped_def h v;
  let after = H.put h p v in source_ok_def after x; source_ok_def h x;
  if x === p then (
    (match v.desc with Var | Bool -> () | Link q -> ()
      | Arrow (a, b) -> ());
    (match v.memo with Empty_memo | Forward _ -> () | Memo (stamp, _) -> ()); ())
  else (match H.at h x with None -> () | Some old ->
    (match old.desc with Var | Bool -> () | Link q -> ()
      | Arrow (a, b) -> ());
    (match old.memo with Empty_memo | Forward _ -> () | Memo (stamp, _) -> ()); ()))

let rec (epoch_allocated @ total) : (saved : node Pref.heap) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | H.mem (heap saved epoch depth d) epoch} @ ghost = fun saved epoch depth d premise -> ghost_ (
  valid_def saved epoch depth d; heap_def saved epoch depth d;
  match d with
  | Clean -> ()
  | Start -> let v = cell Bool depth in put_frame saved epoch v epoch; ()
  | Fresh (rest, p, q, old, desc) ->
    epoch_allocated saved epoch depth rest ();
    let h = heap saved epoch depth rest in let v = cell desc depth in let h1 = H.put h q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h1 p w epoch; ()
  | Alias (rest, p, q, old) -> epoch_allocated saved epoch depth rest ();
    let h = heap saved epoch depth rest in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h p w epoch; ())

let (ready_scoped @ total) : (saved : node Pref.heap) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> (source : desc) @ immutable -> (dest : desc) @ immutable ->
    {u : unit | valid saved epoch depth d && ready saved d source dest} ->
    {u : unit | payload_scoped (heap saved epoch depth d) (cell dest depth)} @ ghost =
  fun saved epoch depth d source dest premise -> ghost_ (
    ready_def saved d source dest;
    let h = heap saved epoch depth d in let v = cell dest depth in cell_def dest depth; payload_scoped_def h v;
    match source, dest with
    | Arrow (a, b), Arrow (x, y) -> target_allocated saved epoch depth d a x ();
      target_allocated saved epoch depth d b y (); ()
    | _ -> ())

let rec (history_scope @ total) : (saved : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit |
      if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | if H.mem (heap saved epoch depth d) x then source_ok (heap saved epoch depth d) x
      else H.at (heap saved epoch depth d) x === None} @ ghost = fun saved scope epoch depth d x premise -> ghost_ (
  valid_def saved epoch depth d; heap_def saved epoch depth d;
  match d with
  | Clean -> scope x; ()
  | Start -> let desc = Bool in let v = cell desc depth in cell_def desc depth; payload_scoped_def saved v;
    let () = put_scope saved scope epoch v x () in ()
  | Fresh (rest, p, q, old, desc) ->
    let h = heap saved epoch depth rest in
    let prior : (x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}
        @ total = fun x -> let () = history_scope saved scope epoch depth rest x () in () in
    ready_scoped saved epoch depth rest old.desc desc ();
    let v = cell desc depth in let h1 = H.put h q v in
    let next : (x : node Pref.t) @ immutable -> {u : unit | if H.mem h1 x then source_ok h1 x else H.at h1 x === None}
        @ total = fun x -> let () = put_scope h prior q v x () in () in
    history_grows saved epoch depth rest p (); prior p; source_ok_def h p;
    epoch_allocated saved epoch depth rest ();
    let w = session_mark rest old epoch q in session_mark_def rest old epoch q; mark_def old epoch q; payload_scoped_def h1 w;
    let () = put_scope h1 next p w x () in ()
  | Alias (rest, p, q, old) ->
    let h = heap saved epoch depth rest in
    let prior : (x : node Pref.t) @ immutable -> {u : unit | if H.mem h x then source_ok h x else H.at h x === None}
        @ total = fun x -> let () = history_scope saved scope epoch depth rest x () in () in
    history_grows saved epoch depth rest p (); prior p; source_ok_def h p;
    epoch_allocated saved epoch depth rest ();
    let w = session_mark rest old epoch q in session_mark_def rest old epoch q; mark_def old epoch q; payload_scoped_def h w;
    let () = put_scope h prior p w x () in ())

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
  payload_scoped_def h v; describes_def rho v.desc value;
  let after = H.put h p v in equation_def after tau x; update x;
  if x === p then (
    match v.desc with Var | Bool -> () | Link q -> update q; ()
    | Arrow (a, b) -> update a; update b; ())
  else (scope x; source_ok_def h x; model x; equation_def h rho x;
    match H.at h x with None -> () | Some old -> match old.desc with
    | Var | Bool -> () | Link q -> update q; () | Arrow (a, b) -> update a; update b; ()))
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
    let[@def] tau : node Pref.t @ immutable total -> ty @ immutable total = fun x -> if x === p then value else rho x in
    let update : (x : node Pref.t) @ immutable -> {u : unit | tau x === (if x === p then value else rho x)}
        @ total = fun x -> tau_def x; () in
    let next : (x : node Pref.t) @ immutable -> {u : unit | equation (H.put h p v) tau x}
        @ total = fun x -> let () = allocation_model h scope rho model p v value tau update x () in () in
    let equal : (x : node Pref.t) @ immutable -> {u : unit | not (H.mem h x) || tau x === rho x}
        @ total = fun x -> tau_def x; () in
    tau_def p; let () = use tau next equal () in ())
let (mark_model @ total) : (d : history) @ immutable -> (h : node Pref.heap) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation h rho x})) @ total ->
    (p : node Pref.t) @ immutable -> (old : node) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.at h p === Some old} -> {u : unit | equation (H.put h p (session_mark d old epoch q)) rho x}
    @ ghost = fun d h rho model p old epoch q x premise -> ghost_ (
  let after = H.put h p (session_mark d old epoch q) in
  session_mark_def d old epoch q; mark_def old epoch q; model x; equation_def h rho x; equation_def after rho x; ())

let (restrict_model @ total) : (saved : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (rho : (node Pref.t @ immutable total -> ty @ immutable total)) @ total ->
    (model : ((x : node Pref.t) @ immutable -> {u : unit | equation (heap saved epoch depth d) rho x})) @ total ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | equation saved rho x} @ ghost = fun saved scope epoch depth d rho model x premise -> ghost_ (
  history_at saved epoch depth d x (); scope x;
  let after = heap saved epoch depth d in model x; equation_def after rho x; equation_def saved rho x; ())
