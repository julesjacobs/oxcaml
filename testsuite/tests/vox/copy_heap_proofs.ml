open Copy_spec

let (put_frame @ total) : (h : node Pref.heap) @ immutable -> (p : node Pref.t) @ immutable ->
    (v : node) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | H.mem (H.put h p v) p && (not (H.mem h x) || H.mem (H.put h p v) x)
      && (x === p || H.at (H.put h p v) x === H.at h x)} @ ghost =
  fun h p v x -> ghost_ ()

let rec (history_at @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | not (H.mem saved x) || (H.mem (heap saved epoch depth d) x &&
      match H.at saved x, H.at (heap saved epoch depth d) x with
      | Some old, Some now -> old.desc === now.desc && old.level === now.level && old.visited === now.visited &&
        (match mapping d x with None -> now.memo === old.memo
        | Some q -> now.memo === (if clean_session d then Forward q else Memo (epoch, q)))
      | None, None -> true | _ -> false)} @ ghost = fun saved epoch depth d x premise -> ghost_ (
  valid_def saved epoch depth d;
  heap_def saved epoch depth d; mapping_def d x; clean_session_def d;
  match d with
  | Start -> let v = cell Bool depth in put_frame saved epoch v x; ()
  | Clean -> ()
  | Fresh (rest, p, q, old, desc) ->
    history_at saved epoch depth rest x ();
    let h = heap saved epoch depth rest in let v = cell desc depth in
    let h1 = H.put h q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
    put_frame h1 p w x; mark_def old epoch q; cell_def desc depth; ()
  | Alias (rest, p, q, old) ->
    history_at saved epoch depth rest x ();
    let h = heap saved epoch depth rest in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h p w x;
    mark_def old epoch q; ())

let (history_grows @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | not (H.mem saved x) || H.mem (heap saved epoch depth d) x} @ ghost =
  fun saved epoch depth d x premise -> ghost_ (
    history_at saved epoch depth d x (); ())

let rec (target_allocated @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth d && target_for saved d p q} ->
    {u : unit | H.mem (heap saved epoch depth d) q} @ ghost =
  fun saved epoch depth d p q premise -> ghost_ (
    target_for_def saved d p q;
    valid_def saved epoch depth d; heap_def saved epoch depth d; mapping_def d p;
    history_grows saved epoch depth d p ();
    match d with Start | Clean -> ()
    | Fresh (rest, x, y, old, desc) ->
      let h = heap saved epoch depth rest in let v = cell desc depth in
      let h1 = H.put h y v in let w = session_mark rest old epoch y in session_mark_def rest old epoch y;
      put_frame h1 x w q;
      if p === x then () else (
        target_for_def saved rest p q;
        target_allocated saved epoch depth rest p q (); ())
    | Alias (rest, x, y, old) ->
      let h = heap saved epoch depth rest in let w = session_mark rest old epoch y in session_mark_def rest old epoch y; put_frame h x w q;
      if p === x then (
        match old.desc with Link child ->
          target_allocated saved epoch depth rest child y (); ()
        | _ -> ())
      else (target_for_def saved rest p q;
        target_allocated saved epoch depth rest p q (); ()))

let rec (extension_trans @ total) : (a : history) @ immutable -> (b : history) @ immutable ->
    (c : history) @ immutable -> {u : unit | extends a b && extends b c} ->
    {u : unit | extends a c} @ ghost = fun a b c premise -> ghost_ (
  extends_def b c; extends_def a c;
  if b === c then () else match c with Start | Clean -> ()
  | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) ->
    extension_trans a b rest (); ())
let rec (mapping_preserved @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (before : history) @ immutable ->
    (after : history) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth after && extends before after && mapping before p === Some q} ->
    {u : unit | mapping after p === Some q} @ ghost = fun saved epoch depth before after p q premise -> ghost_ (
  extends_def before after;
  valid_def saved epoch depth after; mapping_def after p;
  if before === after then () else match after with Start | Clean -> ()
  | Fresh (rest, x, _, _, _) | Alias (rest, x, _, _) ->
    mapping_preserved saved epoch depth before rest p q (); ())
let (target_preserved @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (before : history) @ immutable ->
    (after : history) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth after && extends before after && target_for saved before p q} ->
    {u : unit | target_for saved after p q} @ ghost = fun saved epoch depth before after p q premise -> ghost_ (
  target_for_def saved before p q; target_for_def saved after p q;
  match H.at saved p with None -> () | Some v -> match v.level with
  | Finite _ -> () | Generic -> mapping_preserved saved epoch depth before after p q (); ())

let rec (fresh_epoch @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    {u : unit | valid saved epoch depth d} -> {u : unit | clean_session d || not (H.mem saved epoch)} @ ghost =
  fun saved epoch depth d premise -> ghost_ (
    valid_def saved epoch depth d; clean_session_def d;
    match d with Start | Clean -> ()
    | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) -> fresh_epoch saved epoch depth rest (); ())

let (memo_lookup @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    {u : unit | valid saved epoch depth d && not (clean_session d) && H.mem saved p && source_ok saved p
      && H.at (heap saved epoch depth d) p === Some v} ->
    {u : unit | (match v.memo with Empty_memo | Forward _ -> mapping d p === None
      | Memo (stamp, q) -> if stamp === epoch then mapping d p === Some q else mapping d p === None)}
    @ ghost = fun saved epoch depth d p v premise -> ghost_ (
  source_ok_def saved p;
  history_at saved epoch depth d p ();
  let () = fresh_epoch saved epoch depth d () in ())

let rec (extension_valid @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (before : history) @ immutable ->
    (after : history) @ immutable -> {u : unit | valid saved epoch depth after && extends before after} ->
    {u : unit | valid saved epoch depth before} @ ghost = fun saved epoch depth before after premise -> ghost_ (
  extends_def before after; valid_def saved epoch depth after;
  if before === after then () else match after with Start | Clean -> ()
  | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) ->
    extension_valid saved epoch depth before rest (); ())
let rec (fresh_frame @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (before : history) @ immutable ->
    (after : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | valid saved epoch depth after && extends before after
      && not (H.mem saved x) && H.mem (heap saved epoch depth before) x} ->
    {u : unit | H.mem (heap saved epoch depth after) x
      && H.at (heap saved epoch depth after) x === H.at (heap saved epoch depth before) x} @ ghost =
  fun saved epoch depth before after x premise -> ghost_ (
    extends_def before after;
    valid_def saved epoch depth after; heap_def saved epoch depth after;
    if before === after then () else match after with
    | Start | Clean -> ()
    | Fresh (rest, p, q, old, desc) ->
      fresh_frame saved epoch depth before rest x ();
      let h = heap saved epoch depth rest in let v = cell desc depth in let h1 = H.put h q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h1 p w x; ()
    | Alias (rest, p, q, old) -> fresh_frame saved epoch depth before rest x ();
      let h = heap saved epoch depth rest in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h p w x; ())

let (template_head @ total) : (saved : node Pref.heap) @ immutable -> (t : template) @ immutable -> (p : node Pref.t) @ immutable ->
    {u : unit | template saved t && root t === p} ->
    {u : unit | match H.at saved p with None -> false | Some v ->
      if head_generic t then v.level === Generic && v.desc === head_desc t
      else not (v.level === Generic)} @ ghost = fun saved t p premise -> ghost_ (
  template_def saved t; root_def t; head_desc_def t; head_generic_def t;
  let desc = head_desc t in
  match t with Boundary _ -> finite_node_def saved p; ()
  | _ -> generic_desc_def saved p desc; ())

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
    valid_def saved epoch depth d; mapping_def d p;
    match d with Start | Clean -> ()
    | Fresh (rest, x, y, old, desc) ->
      if p === x then (
        history_at saved epoch depth rest p (); history_grows saved epoch depth rest q ();
        heap_def saved epoch depth d;
        let h = heap saved epoch depth rest in let v = cell desc depth in cell_def desc depth;
        let h1 = H.put h q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h1 p w q;
        fresh_frame saved epoch depth d final q (); ())
      else (extends_def rest d; extends_def rest rest; extension_trans rest d final ();
        let () = copied_fresh saved epoch depth rest final p q () in ())
    | Alias (rest, x, _, old) ->
      if p === x then (history_at saved epoch depth rest p (); ())
      else (extends_def rest d; extends_def rest rest; extension_trans rest d final ();
        let () = copied_fresh saved epoch depth rest final p q () in ()))

let rec (history_unmarked @ total) : (saved : node Pref.heap) @ immutable ->
    (unmarked : ((x : node Pref.t) @ immutable ->
      {u : unit | match H.at saved x with None -> true | Some v -> not v.visited})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | valid saved epoch depth d} ->
    {u : unit | match H.at (heap saved epoch depth d) x with
      None -> true | Some v -> not v.visited} @ ghost =
  fun saved unmarked epoch depth d x premise -> ghost_ (
    valid_def saved epoch depth d;
    heap_def saved epoch depth d; match d with
    | Clean -> unmarked x; ()
    | Start -> unmarked x; let desc : desc = Bool in cell_def desc depth;
      let v = cell desc depth in put_frame saved epoch v x; ()
    | Fresh (rest, p, q, old, desc) ->
      history_unmarked saved unmarked epoch depth rest x ();
      history_unmarked saved unmarked epoch depth rest p ();
      let h = heap saved epoch depth rest in let v = cell desc depth in
      let h1 = H.put h q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      put_frame h1 p w x; cell_def desc depth; mark_def old epoch q; ()
    | Alias (rest, p, q, old) ->
      history_unmarked saved unmarked epoch depth rest x ();
      history_unmarked saved unmarked epoch depth rest p ();
      let h = heap saved epoch depth rest in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      put_frame h p w x; mark_def old epoch q; ())

let (clean_memo_lookup @ total) : (saved : node Pref.heap) @ immutable ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    {u : unit | valid saved epoch depth d && clean_session d && H.mem saved p
      && H.at (heap saved epoch depth d) p === Some v
      && match H.at saved p with None -> true | Some old -> old.memo === Empty_memo} ->
    {u : unit | match v.memo with Empty_memo -> mapping d p === None
      | Forward q -> mapping d p === Some q | Memo _ -> false}
    @ ghost = fun saved epoch depth d p v premise -> ghost_ (
  history_at saved epoch depth d p (); ())
