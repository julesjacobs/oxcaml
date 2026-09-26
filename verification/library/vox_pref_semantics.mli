module H = Pref.Heap

val empty : ('a : immutable_data).
    (p : 'a Pref.t) @ immutable ->
    {u : unit | not (H.mem (H.empty ()) p)
      && H.at (H.empty ()) p === None} @ ghost @@ total

val put : ('a : immutable_data).
    (h : 'a Pref.heap) @ immutable -> (p : 'a Pref.t) @ immutable ->
    (value : 'a) @ immutable -> (q : 'a Pref.t) @ immutable ->
    {u : unit | H.mem (H.put h p value) q = (p === q || H.mem h q)
      && H.at (H.put h p value) q ===
        (if p === q then Some value else H.at h q)} @ ghost @@ total

val union : ('a : immutable_data).
    (left : 'a Pref.heap) @ immutable -> (right : 'a Pref.heap) @ immutable ->
    (p : 'a Pref.t) @ immutable ->
    {u : unit | H.mem (H.union left right) p = (H.mem left p || H.mem right p)
      && H.at (H.union left right) p ===
        (if H.mem left p then H.at left p else H.at right p)} @ ghost @@ total

val restrict : ('a : immutable_data).
    (h : 'a Pref.heap) @ immutable -> (selection : 'a Pref.heap) @ immutable ->
    (p : 'a Pref.t) @ immutable ->
    {u : unit | H.mem (H.restrict h selection) p =
        (H.mem h p && H.mem selection p)
      && H.at (H.restrict h selection) p ===
        (if H.mem selection p then H.at h p else None)} @ ghost @@ total

val exclude : ('a : immutable_data).
    (h : 'a Pref.heap) @ immutable -> (selection : 'a Pref.heap) @ immutable ->
    (p : 'a Pref.t) @ immutable ->
    {u : unit | H.mem (H.exclude h selection) p =
        (H.mem h p && not (H.mem selection p))
      && H.at (H.exclude h selection) p ===
        (if H.mem selection p then None else H.at h p)} @ ghost @@ total
