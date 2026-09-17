open Copy_spec
open Copy_heap_proofs
open Effective_copy_spec

let rec (history_at @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_valid saved heads epoch depth d} ->
    {u : unit | not (H.mem saved x) || (H.mem (heap saved epoch depth d) x &&
      match H.at saved x, H.at (heap saved epoch depth d) x with
      | Some old, Some now -> old.desc === now.desc && old.level === now.level && old.visited === now.visited &&
        (match mapping d x with None -> now.memo === old.memo
        | Some q -> now.memo === (if clean_session d then Forward q else Memo (epoch, q)))
      | None, None -> true | _ -> false)} @ ghost = fun saved heads epoch depth d x premise -> ghost_ (
  let refine_ premise = premise in effective_valid_def saved heads epoch depth d;
  heap_def saved epoch depth d; mapping_def d x; clean_session_def d;
  let u = () in match d with
  | Start -> let v = cell Bool depth in put_frame saved epoch v x; refine_ u
  | Clean -> refine_ u
  | Fresh (rest, p, q, old, desc) ->
    history_at saved heads epoch depth rest x (refine_ u);
    let h = heap saved epoch depth rest in let v = cell desc depth in
    put_frame h q v x; let h1 = H.put h q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
    put_frame h1 p w x; mark_def old epoch q; cell_def desc depth; refine_ u
  | Alias (rest, p, q, old) ->
    history_at saved heads epoch depth rest x (refine_ u);
    let h = heap saved epoch depth rest in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h p w x;
    mark_def old epoch q; refine_ u)

let (history_grows @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (x : node Pref.t) @ immutable -> {u : unit | effective_valid saved heads epoch depth d} ->
    {u : unit | not (H.mem saved x) || H.mem (heap saved epoch depth d) x} @ ghost =
  fun saved heads epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in let u = () in
    history_at saved heads epoch depth d x (refine_ u); refine_ u)

let rec (target_allocated @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && effective_target_for saved heads d p q} ->
    {u : unit | H.mem (heap saved epoch depth d) q} @ ghost =
  fun saved heads epoch depth d p q premise -> ghost_ (
    let refine_ premise = premise in effective_target_for_def saved heads d p q;
    effective_valid_def saved heads epoch depth d; heap_def saved epoch depth d; mapping_def d p;
    let u = () in history_grows saved heads epoch depth d p (refine_ u);
    match d with Start | Clean -> refine_ u
    | Fresh (rest, x, y, old, desc) ->
      let h = heap saved epoch depth rest in let v = cell desc depth in
      put_frame h y v q; let h1 = H.put h y v in let w = session_mark rest old epoch y in session_mark_def rest old epoch y;
      put_frame h1 x w q;
      if p === x then refine_ u else (
        effective_target_for_def saved heads rest p q;
        target_allocated saved heads epoch depth rest p q (refine_ u); refine_ u)
    | Alias (rest, x, y, old) ->
      let h = heap saved epoch depth rest in let w = session_mark rest old epoch y in session_mark_def rest old epoch y; put_frame h x w q;
      if p === x then (
        match old.desc with Link child ->
          target_allocated saved heads epoch depth rest child y (refine_ u); refine_ u
        | _ -> refine_ u)
      else (effective_target_for_def saved heads rest p q;
        target_allocated saved heads epoch depth rest p q (refine_ u); refine_ u))


let rec (mapped_fresh @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable ->
      {u : unit | Effective_level.valid_head saved heads x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && mapping d p === Some q} ->
    {u : unit | not (H.mem saved q)} @ ghost =
  fun saved heads witness epoch depth d p q premise -> ghost_ (
    let refine_ premise = premise in effective_valid_def saved heads epoch depth d;
    mapping_def d p; let u = () in match d with
    | Start | Clean -> refine_ u
    | Fresh (rest, x, _, _, _) ->
      if p === x then (history_grows saved heads epoch depth rest q (refine_ u); refine_ u)
      else (mapped_fresh saved heads witness epoch depth rest p q (refine_ u); refine_ u)
    | Alias (rest, x, _, old) ->
      if p === x then (
        history_at saved heads epoch depth rest x (refine_ u);
        match old.desc with
        | Link child ->
          effective_target_for_def saved heads rest child q;
          witness x; witness child; Level_unifier_spec.observe_def saved x;
          Effective_level.link_level saved heads x child (refine_ u);
          mapped_fresh saved heads witness epoch depth rest child q (refine_ u); refine_ u
        | _ -> refine_ u)
      else (mapped_fresh saved heads witness epoch depth rest p q (refine_ u); refine_ u))

let (copy_link_decision @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (witness : ((x : node Pref.t) @ immutable ->
      {u : unit | Effective_level.valid_head saved heads x})) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && effective_target_for saved heads d p q} ->
    {u : unit | (p === q) === (not (Effective_level.level saved heads p === Generic))
      && (not (Effective_level.level saved heads p === Generic) || not (H.mem saved q))} @ ghost =
  fun saved heads witness epoch depth d p q premise -> ghost_ (
    let refine_ premise = premise in effective_target_for_def saved heads d p q;
    let u = () in match Effective_level.level saved heads p with
    | Finite _ -> refine_ u
    | Generic -> mapped_fresh saved heads witness epoch depth d p q (refine_ u); refine_ u)

let rec (mapping_preserved @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (before : history) @ immutable ->
    (after : history) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth after && extends before after && mapping before p === Some q} ->
    {u : unit | mapping after p === Some q} @ ghost = fun saved heads epoch depth before after p q premise -> ghost_ (
  let refine_ premise = premise in extends_def before after;
  effective_valid_def saved heads epoch depth after; mapping_def after p;
  let u = () in if before === after then refine_ u else match after with Start | Clean -> refine_ u
  | Fresh (rest, x, _, _, _) | Alias (rest, x, _, _) ->
    mapping_preserved saved heads epoch depth before rest p q (refine_ u); refine_ u)
let (clean_memo_lookup @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (d : history) @ immutable ->
    (p : node Pref.t) @ immutable -> (v : node) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && clean_session d && H.mem saved p
      && H.at (heap saved epoch depth d) p === Some v
      && match H.at saved p with None -> true | Some old -> old.memo === Empty_memo} ->
    {u : unit | match v.memo with Empty_memo -> mapping d p === None
      | Forward q -> mapping d p === Some q | Memo _ -> false}
    @ ghost = fun saved heads epoch depth d p v premise -> ghost_ (
  let refine_ premise = premise in let u = () in
  history_at saved heads epoch depth d p (refine_ u); refine_ u)

let (target_preserved @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (before : history) @ immutable ->
    (after : history) @ immutable -> (p : node Pref.t) @ immutable -> (q : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth after && extends before after
      && effective_target_for saved heads before p q} ->
    {u : unit | effective_target_for saved heads after p q} @ ghost =
  fun saved heads epoch depth before after p q premise -> ghost_ (
    let refine_ premise = premise in effective_target_for_def saved heads before p q;
    effective_target_for_def saved heads after p q;
    let u = () in match Effective_level.level saved heads p with
    | Finite _ -> refine_ u
    | Generic -> mapping_preserved saved heads epoch depth before after p q (refine_ u); refine_ u)

let rec (mapped_generic @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (p : node Pref.t) @ immutable ->
    (q : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d && mapping d p === Some q} ->
    {u : unit | Effective_level.level saved heads p === Generic} @ ghost =
  fun saved heads epoch depth d p q premise -> ghost_ (
    let refine_ premise = premise in effective_valid_def saved heads epoch depth d;
    mapping_def d p; let u = () in match d with
    | Start | Clean -> refine_ u
    | Fresh (rest, x, _, _, _) | Alias (rest, x, _, _) ->
      if p === x then refine_ u
      else (mapped_generic saved heads epoch depth rest p q (refine_ u); refine_ u))

open Generalize_spec
open Pooled_spec
open Copy_cleanup_spec
open Copy_cleanup_proofs

let rec (touched_saved @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d} ->
    {u : unit | not (listed (touched d) x) || (H.mem saved x
      && match H.at saved x with None -> false | Some _ -> true)} @ ghost =
  fun saved heads epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in effective_valid_def saved heads epoch depth d;
    touched_def d; let trail = touched d in listed_def trail x;
    let u = () in match d with Start | Clean -> refine_ u
    | Fresh (rest, _, _, _, _) | Alias (rest, _, _, _) ->
      touched_saved saved heads epoch depth rest x (refine_ u);
      history_at saved heads epoch depth rest x (refine_ u); refine_ u)

let rec (touched_distinct @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> {u : unit | effective_valid saved heads epoch depth d} ->
    {u : unit | distinct (touched d)} @ ghost =
  fun saved heads epoch depth d premise -> ghost_ (
    let refine_ premise = premise in effective_valid_def saved heads epoch depth d;
    touched_def d; let trail = touched d in distinct_def trail;
    let u = () in match d with Start | Clean -> refine_ u
    | Fresh (rest, p, _, _, _) | Alias (rest, p, _, _) ->
      touched_mapping rest p;
      touched_distinct saved heads epoch depth rest (refine_ u); refine_ u)

let rec (history_clean @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) ->
    (d : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth d &&
      match H.at saved x with None -> true | Some v -> v.memo === Empty_memo} ->
    {u : unit | match H.at (heap saved epoch depth d) x with
      | None -> true | Some v -> v.memo === Empty_memo || listed (touched d) x} @ ghost =
  fun saved heads epoch depth d x premise -> ghost_ (
    let refine_ premise = premise in effective_valid_def saved heads epoch depth d;
    heap_def saved epoch depth d; touched_def d;
    let trail = touched d in listed_def trail x;
    let u = () in match d with
    | Clean -> refine_ u
    | Start ->
      let desc : desc = Bool in let v = cell desc depth in cell_def desc depth;
      put_frame saved epoch v x; refine_ u
    | Fresh (rest, p, q, old, desc) ->
      history_clean saved heads epoch depth rest x (refine_ u);
      let h = heap saved epoch depth rest in let v = cell desc depth in
      cell_def desc depth; put_frame h q v x;
      let mid = H.put h q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      put_frame mid p w x; refine_ u
    | Alias (rest, p, q, old) ->
      history_clean saved heads epoch depth rest x (refine_ u);
      let h = heap saved epoch depth rest in let w = session_mark rest old epoch q in session_mark_def rest old epoch q;
      put_frame h p w x; refine_ u)

let rec (fresh_frame @ total) : (saved : Pref.heap) @ immutable ->
    (heads : Effective_level.heads) @ total ->
    (epoch : node Pref.t) @ immutable -> (depth : int) -> (before : history) @ immutable ->
    (after : history) @ immutable -> (x : node Pref.t) @ immutable ->
    {u : unit | effective_valid saved heads epoch depth after && extends before after
      && not (H.mem saved x) && H.mem (heap saved epoch depth before) x} ->
    {u : unit | H.mem (heap saved epoch depth after) x
      && H.at (heap saved epoch depth after) x === H.at (heap saved epoch depth before) x} @ ghost =
  fun saved heads epoch depth before after x premise -> ghost_ (
    let refine_ premise = premise in extends_def before after;
    effective_valid_def saved heads epoch depth after; heap_def saved epoch depth after;
    let u = () in if before === after then refine_ u else match after with
    | Start | Clean -> refine_ u
    | Fresh (rest, p, q, old, desc) ->
      fresh_frame saved heads epoch depth before rest x (refine_ u);
      let h = heap saved epoch depth rest in let v = cell desc depth in put_frame h q v x;
      let h1 = H.put h q v in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h1 p w x; refine_ u
    | Alias (rest, p, q, old) -> fresh_frame saved heads epoch depth before rest x (refine_ u);
      let h = heap saved epoch depth rest in let w = session_mark rest old epoch q in session_mark_def rest old epoch q; put_frame h p w x; refine_ u)

