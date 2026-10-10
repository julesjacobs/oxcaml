module H = Pref.Heap

let (empty @ total) : ('a : immutable_data).
    (p : 'a Pref.t) @ immutable ->
    {u : unit | not (H.mem (H.empty ()) p)
      && H.at (H.empty ()) p === None} @ ghost =
  fun p -> ghost_ (())

let (put @ total) : ('a : immutable_data).
    (h : 'a Pref.heap) @ immutable -> (p : 'a Pref.t) @ immutable ->
    (value : 'a) @ immutable -> (q : 'a Pref.t) @ immutable ->
    {u : unit | H.mem (H.put h p value) q = (p === q || H.mem h q)
      && H.at (H.put h p value) q ===
        (if p === q then Some value else H.at h q)} @ ghost =
  fun h p value q -> ghost_ (())

let (union @ total) : ('a : immutable_data).
    (left : 'a Pref.heap) @ immutable -> (right : 'a Pref.heap) @ immutable ->
    (p : 'a Pref.t) @ immutable ->
    {u : unit | H.mem (H.union left right) p = (H.mem left p || H.mem right p)
      && H.at (H.union left right) p ===
        (if H.mem left p then H.at left p else H.at right p)} @ ghost =
  fun left right p -> ghost_ (())

let (restrict @ total) : ('a : immutable_data).
    (h : 'a Pref.heap) @ immutable -> (selection : 'a Pref.heap) @ immutable ->
    (p : 'a Pref.t) @ immutable ->
    {u : unit | H.mem (H.restrict h selection) p =
        (H.mem h p && H.mem selection p)
      && H.at (H.restrict h selection) p ===
        (if H.mem selection p then H.at h p else None)} @ ghost =
  fun h selection p -> ghost_ (())

let (exclude @ total) : ('a : immutable_data).
    (h : 'a Pref.heap) @ immutable -> (selection : 'a Pref.heap) @ immutable ->
    (p : 'a Pref.t) @ immutable ->
    {u : unit | H.mem (H.exclude h selection) p =
        (H.mem h p && not (H.mem selection p))
      && H.at (H.exclude h selection) p ===
        (if H.mem selection p then None else H.at h p)} @ ghost =
  fun h selection p -> ghost_ (())
