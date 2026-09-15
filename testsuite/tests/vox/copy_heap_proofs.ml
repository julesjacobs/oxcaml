open Copy_spec

let (put_frame @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem (H.put h p v) p && (not (H.mem h x) || H.mem (H.put h p v) x)
      && (x === p || H.at (H.put h p v) x === H.at h x)} @ ghost =
  fun h p v x -> ghost_ (let u = () in refine_ u)

let rec (history_at @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | not (H.mem saved x) || (H.mem (heap saved epoch depth d) x &&
      match H.at saved x, H.at (heap saved epoch depth d) x with
      | Some old, Some now -> old.desc === now.desc && old.level === now.level && old.visited === now.visited &&
        (match mapping d x with None -> now.memo === old.memo
        | Some q -> now.memo === Memo (epoch, q))
      | None, None -> true | _ -> false)} @ ghost = fun saved epoch depth d x premise -> ghost_ (
  let refine_ premise = premise in valid_def saved epoch depth d;
  heap_def saved epoch depth d; mapping_def d x;
  let u = () in match d with
  | Start -> let v = cell Bool depth in put_frame saved epoch v x; refine_ u
  | Fresh (rest, p, q, old, desc) ->
    history_at saved epoch depth rest x (refine_ u);
    let h = heap saved epoch depth rest in let v = cell desc depth in
    put_frame h q v x; let h1 = H.put h q v in let w = mark old epoch q in
    put_frame h1 p w x; mark_def old epoch q; cell_def desc depth; refine_ u
  | Alias (rest, p, q, old) ->
    history_at saved epoch depth rest x (refine_ u);
    let h = heap saved epoch depth rest in let w = mark old epoch q in put_frame h p w x;
    mark_def old epoch q; refine_ u)

let (history_grows @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | not (H.mem saved x) || H.mem (heap saved epoch depth d) x} @ ghost =
  fun saved epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    history_at saved epoch depth d x (refine_ u); refine_ u)

let rec (target_allocated @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d && target_for saved d p q} ->
    {u : unit | H.mem (heap saved epoch depth d) q} @ ghost =
  fun saved epoch depth d p q premise -> ghost_ (
    let refine_ premise = premise in target_for_def saved d p q;
    valid_def saved epoch depth d; heap_def saved epoch depth d; mapping_def d p;
    let u = () in history_grows saved epoch depth d p (refine_ u);
    match d with Start -> refine_ u
    | Fresh (rest, x, y, old, desc) ->
      let h = heap saved epoch depth rest in let v = cell desc depth in
      put_frame h y v q; let h1 = H.put h y v in let w = mark old epoch y in
      put_frame h1 x w q;
      if p === x then refine_ u else (
        target_for_def saved rest p q;
        target_allocated saved epoch depth rest p q (refine_ u); refine_ u)
    | Alias (rest, x, y, old) ->
      let h = heap saved epoch depth rest in let w = mark old epoch y in put_frame h x w q;
      if p === x then (
        match old.desc with Link child ->
          target_allocated saved epoch depth rest child y (refine_ u); refine_ u
        | _ -> refine_ u)
      else (target_for_def saved rest p q;
        target_allocated saved epoch depth rest p q (refine_ u); refine_ u))

let[@def] rec (extends @ total) (before : history @ immutable) (after : history @ immutable) = ghost_ (
  before === after || match after with Start -> false
  | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) -> extends before rest)
let rec (extension_trans @ total) : (a : history) @ immutable -> (b : history) @ immutable ->
    (c : history) @ immutable -> {u : unit | extends a b && extends b c} ->
    {u : unit | extends a c} @ ghost = fun a b c premise -> ghost_ (
  let refine_ premise = premise in extends_def b c; extends_def a c;
  let u = () in if b === c then refine_ u else match c with Start -> refine_ u
  | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) ->
    extension_trans a b rest (refine_ u); refine_ u)
let rec (mapping_preserved @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (before : history) @ immutable ->
    (after : history) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth after && extends before after && mapping before p === Some q} ->
    {u : unit | mapping after p === Some q} @ ghost = fun saved epoch depth before after p q premise -> ghost_ (
  let refine_ premise = premise in extends_def before after;
  valid_def saved epoch depth after; mapping_def after p;
  let u = () in if before === after then refine_ u else match after with Start -> refine_ u
  | Fresh (rest, x, _, _, _) | Alias (rest, x, _, _) ->
    mapping_preserved saved epoch depth before rest p q (refine_ u); refine_ u)
let (target_preserved @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (before : history) @ immutable ->
    (after : history) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth after && extends before after && target_for saved before p q} ->
    {u : unit | target_for saved after p q} @ ghost = fun saved epoch depth before after p q premise -> ghost_ (
  let refine_ premise = premise in target_for_def saved before p q; target_for_def saved after p q;
  let u = () in match H.at saved p with None -> refine_ u | Some v -> match v.level with
  | Finite _ -> refine_ u | Generic -> mapping_preserved saved epoch depth before after p q (refine_ u); refine_ u)

let rec (fresh_epoch @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    {u : unit | valid saved epoch depth d} -> {u : unit | not (H.mem saved epoch)} @ ghost =
  fun saved epoch depth d premise -> ghost_ (
    let refine_ premise = premise in valid_def saved epoch depth d;
    let u = () in match d with Start -> refine_ u
    | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) -> fresh_epoch saved epoch depth rest (refine_ u))

let (memo_lookup @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    {u : unit | valid saved epoch depth d && H.mem saved p && source_ok saved p
      && H.at (heap saved epoch depth d) p === Some v} ->
    {u : unit | (match v.memo with Empty_memo -> mapping d p === None
      | Memo (stamp, q) -> if stamp === epoch then mapping d p === Some q else mapping d p === None)}
    @ ghost = fun saved epoch depth d p v premise -> ghost_ (
  let refine_ premise = premise in source_ok_def saved p;
  let u = () in history_at saved epoch depth d p (refine_ u);
  let refine_ u = fresh_epoch saved epoch depth d (refine_ u) in refine_ u)

let rec (extension_valid @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (before : history) @ immutable ->
    (after : history) @ immutable -> {u : unit | valid saved epoch depth after && extends before after} ->
    {u : unit | valid saved epoch depth before} @ ghost = fun saved epoch depth before after premise -> ghost_ (
  let refine_ premise = premise in extends_def before after; valid_def saved epoch depth after;
  let u = () in if before === after then refine_ u else match after with Start -> refine_ u
  | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) ->
    extension_valid saved epoch depth before rest (refine_ u); refine_ u)
let rec (fresh_frame @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (before : history) @ immutable ->
    (after : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth after && extends before after
      && not (H.mem saved x) && H.mem (heap saved epoch depth before) x} ->
    {u : unit | H.mem (heap saved epoch depth after) x
      && H.at (heap saved epoch depth after) x === H.at (heap saved epoch depth before) x} @ ghost =
  fun saved epoch depth before after x premise -> ghost_ (
    let refine_ premise = premise in extends_def before after;
    valid_def saved epoch depth after; heap_def saved epoch depth after;
    let u = () in if before === after then refine_ u else match after with
    | Start -> refine_ u
    | Fresh (rest, p, q, old, desc) ->
      fresh_frame saved epoch depth before rest x (refine_ u);
      let h = heap saved epoch depth rest in let v = cell desc depth in put_frame h q v x;
      let h1 = H.put h q v in let w = mark old epoch q in put_frame h1 p w x; refine_ u
    | Alias (rest, p, q, old) -> fresh_frame saved epoch depth before rest x (refine_ u);
      let h = heap saved epoch depth rest in let w = mark old epoch q in put_frame h p w x; refine_ u)

let (template_head @ total) : (saved : node Pref.heap) @ immutable -> (t : template) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | template saved t && root t === p} ->
    {u : unit | match H.at saved p with None -> false | Some v ->
      if head_generic t then v.level === Generic && v.desc === head_desc t
      else not (v.level === Generic)} @ ghost = fun saved t p premise -> ghost_ (
  let refine_ premise = premise in template_def saved t; root_def t; head_desc_def t; head_generic_def t;
  let desc = head_desc t in
  let u = () in match t with Boundary _ -> finite_node_def saved p; refine_ u
  | _ -> generic_desc_def saved p desc; refine_ u)

let rec (copied_fresh @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (final : history) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d && valid saved epoch depth final && extends d final
      && mapping d p === Some q && match H.at saved p with None -> false | Some v ->
        match v.desc with Link _ -> false | _ -> true} ->
    {u : unit | not (H.mem saved q) && H.mem (heap saved epoch depth final) q
      && match H.at (heap saved epoch depth final) q with None -> false
        | Some v -> v.level === Finite depth && v.memo === Empty_memo} @ ghost =
  fun saved epoch depth d final p q premise -> ghost_ (
    let refine_ premise = premise in valid_def saved epoch depth d; mapping_def d p;
    let u = () in match d with Start -> refine_ u
    | Fresh (rest, x, y, old, desc) ->
      if p === x then (
        history_at saved epoch depth rest p (refine_ u); history_grows saved epoch depth rest q (refine_ u);
        heap_def saved epoch depth d;
        let h = heap saved epoch depth rest in let v = cell desc depth in cell_def desc depth;
        put_frame h q v q; let h1 = H.put h q v in let w = mark old epoch q in put_frame h1 p w q;
        fresh_frame saved epoch depth d final q (refine_ u); refine_ u)
      else (extends_def rest d; extends_def rest rest; extension_trans rest d final (refine_ u);
        let refine_ u = copied_fresh saved epoch depth rest final p q (refine_ u) in refine_ u)
    | Alias (rest, x, _, old) ->
      if p === x then (history_at saved epoch depth rest p (refine_ u); refine_ u)
      else (extends_def rest d; extends_def rest rest; extension_trans rest d final (refine_ u);
        let refine_ u = copied_fresh saved epoch depth rest final p q (refine_ u) in refine_ u))

let rec (history_unmarked @ total) : (saved : node Pref.heap) @ immutable ->
    (unmarked : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved x with None -> true | Some v -> not v.visited})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | match H.at (heap saved epoch depth d) x with
      None -> true | Some v -> not v.visited} @ ghost =
  fun saved unmarked epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in valid_def saved epoch depth d;
    heap_def saved epoch depth d; let u = () in
    match d with
    | Start -> unmarked x; let desc : desc = Bool in cell_def desc depth;
      let v = cell desc depth in put_frame saved epoch v x; refine_ u
    | Fresh (rest, p, q, old, desc) ->
      history_unmarked saved unmarked epoch depth rest x (refine_ u);
      history_unmarked saved unmarked epoch depth rest p (refine_ u);
      let h = heap saved epoch depth rest in let v = cell desc depth in
      put_frame h q v x; let h1 = H.put h q v in let w = mark old epoch q in
      put_frame h1 p w x; cell_def desc depth; mark_def old epoch q; refine_ u
    | Alias (rest, p, q, old) ->
      history_unmarked saved unmarked epoch depth rest x (refine_ u);
      history_unmarked saved unmarked epoch depth rest p (refine_ u);
      let h = heap saved epoch depth rest in let w = mark old epoch q in
      put_frame h p w x; mark_def old epoch q; refine_ u)
