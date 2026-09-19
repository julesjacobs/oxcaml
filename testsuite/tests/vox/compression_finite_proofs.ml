open Copy_spec
open Level_unifier_spec
open Level_finite_spec

let[@def] rec (head @ total) (tree : tree @ immutable) = match tree with
  | Alias_tree (_, child) -> head child | _ -> tree
let[@def] rec (compress @ total) (p : node Pref.t @ immutable) (tree : tree @ immutable) = ghost_ (match tree with
  | Free _ | Constant_tree _ -> tree
  | Alias_tree (q, child) -> let next = compress p child in
    Alias_tree (q, if q === p then head next else next)
  | Branch (q, a, b) -> Branch (q, compress p a, compress p b))

let (compress_root @ total) : (p : node Pref.t) @ immutable -> (tree : tree) @ immutable ->
    {u : unit | tree_root (compress p tree) === tree_root tree} @ ghost = fun p tree -> ghost_ (
    compress_def p tree; tree_root_def tree; let next = compress p tree in tree_root_def next; ())

let rec (head_idempotent @ total) : (tree : tree) @ immutable ->
    {u : unit | head (head tree) === head tree} @ ghost = fun tree -> ghost_ (
    head_def tree; let out = head tree in head_def out;
    (match tree with Alias_tree (_, child) -> head_idempotent child; () | _ -> ()); ())

let rec (head_finite @ total) : (h : Pref.heap) @ immutable -> (tree : tree) @ immutable ->
    {u : unit | finite h tree} ->
    {u : unit | finite h (head tree) && terminal h (tree_root (head tree))} @ ghost = fun h tree premise -> ghost_ (
    finite_def h tree; head_def tree; tree_root_def tree;
    let out = head tree in let root = tree_root out in terminal_def h root;
    match tree with Alias_tree (_, child) -> head_finite h child (); () | _ -> ())

let rec (head_compress @ total) : (p : node Pref.t) @ immutable -> (tree : tree) @ immutable ->
    {u : unit | tree_root (head (compress p tree)) === tree_root (head tree)} @ ghost = fun p tree -> ghost_ (
    compress_def p tree; head_def tree; let next = compress p tree in head_def next;
    match tree with
    | Alias_tree (q, child) -> let changed = compress p child in
      head_compress p child; if q === p then (head_idempotent changed; ()) else ()
    | _ -> tree_root_def tree; tree_root_def next; ())

let rec (resolution_head @ total) : (h : Pref.heap) @ immutable -> (tree : tree) @ immutable ->
    (r : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | finite h tree && resolves h (tree_root tree) r path} ->
    {u : unit | tree_root (head tree) === r} @ ghost = fun h tree r path premise -> ghost_ (
    finite_def h tree; tree_root_def tree; head_def tree;
    let p = tree_root tree in resolves_def h p r path; terminal_def h p;
    match path with Here -> () | Via (_, rest) ->
      match tree with Alias_tree (_, child) -> resolution_head h child r rest (); () | _ -> ())

let rec (finite_compress @ total) : (h : Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable ->
    (path : resolution) @ immutable -> (tree : tree) @ immutable ->
    {u : unit | observe h p === Some (Link q) && resolves h p r path && finite h tree} ->
    {u : unit | finite (H.put h p (redirect h p r)) (compress p tree)} @ ghost = fun h p q r path tree premise -> ghost_ (
    finite_def h tree; tree_root_def tree; compress_def p tree;
    let v = redirect h p r in let after = H.put h p v in let next = compress p tree in
    finite_def after next; tree_root_def next; let root = tree_root tree in
    Level_unifier_proofs.observe_write h p v root; Level_unifier_proofs.redirect_desc h p r;
    match tree with
    | Free _ | Constant_tree _ -> ()
    | Alias_tree (s, child) ->
      finite_compress h p q r path child (); compress_root p child;
      let changed = compress p child in head_finite after changed ();
      if s === p then (resolution_head h tree r path ();
        head_def tree; head_compress p child; ()) else ()
    | Branch (_, a, b) -> finite_compress h p q r path a ();
      finite_compress h p q r path b (); compress_root p a; compress_root p b; ())

let rec (head_readback @ total) : (tree : tree) @ immutable ->
    {u : unit | readback (head tree) === readback tree} @ ghost = fun tree -> ghost_ (
    head_def tree; readback_def tree;
    (match tree with Alias_tree (_, child) -> head_readback child; () | _ -> ()); ())

let rec (compress_readback @ total) : (p : node Pref.t) @ immutable -> (tree : tree) @ immutable ->
    {u : unit | readback (compress p tree) === readback tree} @ ghost = fun p tree -> ghost_ (
    compress_def p tree; readback_def tree; let next = compress p tree in readback_def next;
    match tree with
    | Free _ | Constant_tree _ -> ()
    | Alias_tree (q, child) -> compress_readback p child; let changed = compress p child in
      if q === p then (head_readback changed; ()) else ()
    | Branch (_, a, b) -> compress_readback p a; compress_readback p b; ())
