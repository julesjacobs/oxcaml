open Copy_spec
open Level_spec
open Level_unifier_spec

let rec (resolution_terminal @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | resolves h p r path} -> {u : unit | H.mem h r && terminal h r} @ ghost = fun h p r path premise -> ghost_ (
    let refine_ premise = premise in resolves_def h p r path; let u = () in match path with
    | Here -> refine_ u | Via (q, rest) -> resolution_terminal h q r rest (refine_ u); refine_ u)

let rec (redirect_resolution @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | H.mem h p && not (p === r) && resolves h q r path} ->
    {d : resolution | resolves (H.put h p (redirect h p r)) q r d} @ immutable ghost = fun h p r q path premise -> ghost_ (
    let refine_ premise = premise in let u = () in resolution_terminal h q r path (refine_ u);
    let v = redirect h p r in let after = H.put h p v in
    Level_unifier_proofs.observe_write h p v q; Level_unifier_proofs.redirect_desc h p r;
    if q === p then (
      Level_unifier_proofs.observe_write h p v r; terminal_def h r; terminal_def after r;
      let here = Here in let d = Via (r, here) in resolves_def after r r here; resolves_def after q r d; refine_ d)
    else (resolves_def h q r path; match path with
    | Here -> terminal_def h q; terminal_def after q; let d = Here in resolves_def after q r d; refine_ d
    | Via (next, rest) ->
      let refine_ tail = redirect_resolution h p r next rest (refine_ u) in
      let d = Via (next, tail) in resolves_def after q r d; refine_ d))

let[@def] (tail @ total) (path : resolution @ immutable) = match path with Here -> Here | Via (_, rest) -> rest
let (tail_resolves @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    (r : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | observe h p === Some (Link q) && resolves h p r path} ->
    {u : unit | resolves h q r (tail path)} @ ghost = fun h p q r path premise -> ghost_ (
    let refine_ premise = premise in tail_def path; resolves_def h p r path; terminal_def h p; let u = () in refine_ u)

let rec (unique_root @ total) : (h : node Pref.heap) @ immutable ->
    (p : node Pref.t) @ immutable -> (r : node Pref.t) @ immutable ->
    (s : node Pref.t) @ immutable -> (a : resolution) @ immutable -> (b : resolution) @ immutable ->
    {u : unit | resolves h p r a && resolves h p s b} -> {u : unit | r === s} @ ghost =
  fun h p r s a b premise -> ghost_ (
    let refine_ premise = premise in resolves_def h p r a; resolves_def h p s b;
    terminal_def h p; let u = () in match a with
    | Here -> refine_ u
    | Via (q, rest) -> (match b with
      | Here -> refine_ u
      | Via (_, tail) -> unique_root h q r s rest tail (refine_ u); refine_ u))

let rec (shortcut_resolution @ total) : (h : node Pref.heap) @ immutable ->
    (source : node Pref.t) @ immutable -> (target : node Pref.t) @ immutable ->
    (original : resolution) @ immutable -> (p : node Pref.t) @ immutable ->
    (root : node Pref.t) @ immutable -> (path : resolution) @ immutable ->
    {u : unit | H.mem h source && not (source === target)
      && resolves h source target original && resolves h p root path} ->
    {d : resolution | resolves (H.put h source (redirect h source target)) p root d} @ immutable ghost =
  fun h source target original p root path premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    let v = redirect h source target in let after = H.put h source v in
    Level_unifier_proofs.observe_write h source v p;
    if p === source then (
      unique_root h p root target path original (refine_ u);
      let refine_ d = redirect_resolution h source target p path (refine_ u) in refine_ d)
    else (
      resolves_def h p root path;
      match path with
      | Here -> terminal_def h p; terminal_def after p;
        let d = Here in resolves_def after p root d; refine_ d
      | Via (q, rest) ->
        let refine_ tail = shortcut_resolution h source target original q root rest (refine_ u) in
        let d = Via (q, tail) in resolves_def after p root d; refine_ d))
