open Copy_spec
open Copy_heap_proofs
open Copy_model_proofs
open Level_spec

let rec (copy_depth @ total) : (saved : node Pref.heap) @ immutable -> (epoch : node Pref.t) @ immutable ->
    (depth : int) -> (d : history) @ immutable -> {u : unit | valid saved epoch depth d} -> {u : unit | depth >= 0} @ ghost =
  fun saved epoch depth d premise -> ghost_ (
    let refine_ premise = premise in valid_def saved epoch depth d; let u = () in match d with
    | Start | Clean -> refine_ u | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) ->
      copy_depth saved epoch depth rest (refine_ u); refine_ u)

let rec (target_active_at @ total) : (saved : node Pref.heap) @ immutable ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || ordered saved x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (final : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d && valid saved epoch depth final && extends d final && target_for saved d p q} ->
    {u : unit | active (heap saved epoch depth final) q} @ ghost = fun saved order epoch depth d final p q premise -> ghost_ (
  let refine_ premise = premise in target_for_def saved d p q; valid_def saved epoch depth d; mapping_def d p;
  let after = heap saved epoch depth final in let u = () in
  copy_depth saved epoch depth d (refine_ u); order p; ordered_def saved p; history_at saved epoch depth final p (refine_ u);
  active_def after q; at_level_def after q;
  match H.at saved p with None -> refine_ u | Some v ->
  match v.level with Finite _ -> refine_ u | Generic ->
    match d with Start | Clean -> refine_ u
    | Fresh (rest, x, _, old, desc) ->
      if p === x then (
        history_at saved epoch depth rest p (refine_ u); ready_def saved rest old.desc desc;
        copied_fresh saved epoch depth d final p q (refine_ u); refine_ u)
      else (
        extends_def rest d; extends_def rest rest; extension_trans rest d final (refine_ u);
        target_for_def saved rest p q;
        target_active_at saved order epoch depth rest final p q (refine_ u); refine_ u)
    | Alias (rest, x, _, old) ->
      extends_def rest d; extends_def rest rest; extension_trans rest d final (refine_ u);
      if p === x then (match old.desc with Link child ->
        target_active_at saved order epoch depth rest final child q (refine_ u); refine_ u | _ -> refine_ u)
      else (target_for_def saved rest p q;
        target_active_at saved order epoch depth rest final p q (refine_ u); refine_ u))

let rec (copy_finite_scope @ total) : (saved : node Pref.heap) @ immutable ->
    (scope : ((x : node Pref.t) @ immutable -> {u : unit | if H.mem saved x then source_ok saved x else H.at saved x === None})) @ total ->
    (order : ((x : node Pref.t) @ immutable -> {u : unit | not (H.mem saved x) || ordered saved x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d} ->
    {u : unit | not (H.mem (heap saved epoch depth d) x) || finite_scope (heap saved epoch depth d) x} @ ghost =
  fun saved scope order epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in valid_def saved epoch depth d; heap_def saved epoch depth d;
    let after = heap saved epoch depth d in let u = () in history_scope saved scope epoch depth d x (refine_ u);
    source_ok_def after x; finite_scope_def after x; active_def after x; at_level_def after x;
    scope x;
    if H.mem saved x then (
      scope x; source_ok_def saved x; history_at saved epoch depth d x (refine_ u); order x; ordered_def saved x;
      match H.at saved x with None -> refine_ u | Some v -> match v.level with Generic -> refine_ u | Finite n ->
        children_below_def saved v.desc n;
        let available : ((y : node Pref.t) @ immutable -> {u : unit | not (below saved y n) || active after y}) @ total = fun y ->
          below_def saved y n; active_def after y; at_level_def saved y; at_level_def after y;
          let u = () in history_at saved epoch depth d y (refine_ u); refine_ u in
        (match v.desc with Var | Bool | Word -> () | Link q | List q -> available q; () | Arrow (a, b) -> available a; available b; ()); refine_ u)
    else match d with
    | Clean -> refine_ u
    | Start -> let desc = Bool in cell_def desc depth; let v = cell desc depth in put_frame saved epoch v x; refine_ u
    | Fresh (rest, p, q, old, desc) ->
      let mid = heap saved epoch depth rest in
      copy_finite_scope saved scope order epoch depth rest x (refine_ u);
      if x === q then (
        let v = cell desc depth in cell_def desc depth; let w = session_mark rest old epoch q in session_mark_def rest old epoch q; mark_def old epoch q;
        put_frame mid q v x; let h1 = H.put mid q v in put_frame h1 p w x;
        ready_def saved rest old.desc desc; extends_def rest d; extends_def rest rest;
        (match old.desc, desc with
        | List a, List c -> target_active_at saved order epoch depth rest d a c (refine_ u); ()
        | Arrow (a, b), Arrow (c, e) ->
          target_active_at saved order epoch depth rest d a c (refine_ u);
          target_active_at saved order epoch depth rest d b e (refine_ u); () | _ -> ()); refine_ u)
      else (
        finite_scope_def mid x; active_def mid x; at_level_def mid x;
        let v = cell desc depth in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
        put_frame mid q v x; let h1 = H.put mid q v in put_frame h1 p w x;
        (* Earlier copied children remain finite after later memo writes. *)
        let preserve : ((y : node Pref.t) @ immutable -> {u : unit | not (active mid y) || active after y}) @ total = fun y ->
          active_def mid y; active_def after y; at_level_def mid y; at_level_def after y;
          let u = () in history_at saved epoch depth rest p (refine_ u); mark_def old epoch q; cell_def desc depth;
          put_frame mid q v y; put_frame h1 p w y; refine_ u in
        (match H.at mid x with None -> () | Some v -> match v.desc with Var | Bool | Word -> ()
          | Link a | List a -> preserve a; () | Arrow (a, b) -> preserve a; preserve b; ()); refine_ u)
    | Alias (rest, p, q, old) ->
      let mid = heap saved epoch depth rest in copy_finite_scope saved scope order epoch depth rest x (refine_ u);
      let w = session_mark rest old epoch q in session_mark_def rest old epoch q; mark_def old epoch q; put_frame mid p w x;
      finite_scope_def mid x; active_def mid x; at_level_def mid x;
      let preserve : ((y : node Pref.t) @ immutable -> {u : unit | not (active mid y) || active after y}) @ total = fun y ->
        active_def mid y; active_def after y; at_level_def mid y; at_level_def after y;
        let u = () in history_at saved epoch depth rest p (refine_ u); put_frame mid p w y; refine_ u in
      (match H.at mid x with None -> () | Some v -> match v.desc with Var | Bool | Word -> ()
        | Link a | List a -> preserve a; () | Arrow (a, b) -> preserve a; preserve b; ()); refine_ u)
