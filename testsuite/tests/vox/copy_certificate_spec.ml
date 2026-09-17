open Copy_spec
module C = Representative_certificate

let[@def] (certificate_level @ total) (h : Pref.heap @ immutable)
    (c : C.certificate @ immutable) (p : node Pref.t @ immutable) = ghost_ (
  if H.mem h p then Level_spec.at_level h (C.head c p).Representative_level.root else Generic)

let[@def] (certified_target @ total) (saved : Pref.heap @ immutable) (c : C.certificate @ immutable) (d : history @ immutable)
    (p : node Pref.t @ immutable) (q : node Pref.t @ immutable) = ghost_ (
  H.mem saved p && match certificate_level saved c p with
  | Finite _ -> q === p | Generic -> mapping d p === Some q)
let[@def] (certified_ready @ total) (saved : Pref.heap @ immutable) (c : C.certificate @ immutable) (d : history @ immutable)
    (source : desc @ immutable) (dest : desc @ immutable) = ghost_ (match source, dest with
  | Var, Var | Bool, Bool -> true
  | Arrow (a, b), Arrow (x, y) -> certified_target saved c d a x && certified_target saved c d b y
  | _ -> false)
let[@def] rec (certified_valid @ total) (saved : Pref.heap @ immutable) (c : C.certificate @ immutable)
    (epoch : node Pref.t @ immutable) (depth : int) (d : history @ immutable) = ghost_ (
  match d with
  | Start -> not (H.mem saved epoch) && depth >= 0
  | Clean -> H.mem saved epoch && depth >= 0
  | Fresh (rest, p, q, old, desc) -> certified_valid saved c epoch depth rest
    && H.mem saved p && H.at (heap saved epoch depth rest) p === Some old
    && certificate_level saved c p === Generic && mapping rest p === None
    && not (H.mem (heap saved epoch depth rest) q) && certified_ready saved c rest old.desc desc
  | Alias (rest, p, q, old) -> certified_valid saved c epoch depth rest
    && H.mem saved p && H.at (heap saved epoch depth rest) p === Some old
    && certificate_level saved c p === Generic && mapping rest p === None
    && (match old.desc with Link x -> certified_target saved c rest x q | _ -> false))


let[@def] (covered_desc @ total) (c : C.certificate @ immutable) (desc : desc @ immutable) = ghost_ (
  match desc with Var | Bool -> true | Link p -> C.listed c p
  | Arrow (a, b) -> C.listed c a && C.listed c b)

let[@def] rec (covered @ total) (c : C.certificate @ immutable) (d : history @ immutable) = ghost_ (
  match d with Start | Clean -> true
  | Fresh (rest, p, _, old, _) | Alias (rest, p, _, old) ->
    covered c rest && C.listed c p && covered_desc c old.desc)

let[@def] (certifies @ total) (h : Pref.heap @ immutable)
    (c : C.certificate @ immutable) (epoch : node Pref.t @ immutable)
    (depth : int) (d : history @ immutable) (p : node Pref.t @ immutable)
    (q : node Pref.t @ immutable) = ghost_ (
  C.certificate_valid h c && covered c d && C.listed c p
  && certified_valid h c epoch depth d && certified_target h c d p q)

let[@def] (capture_desc @ total) (heads : Effective_level.heads @ total)
    (desc : desc @ immutable) (c : C.certificate @ immutable) = ghost_ (
  match desc with Var | Bool -> c
  | Link p -> C.Entry (p, heads p, c)
  | Arrow (a, b) -> C.Entry (a, heads a, C.Entry (b, heads b, c)))

let[@def] rec (capture @ total) (heads : Effective_level.heads @ total)
    (d : history @ immutable) (c : C.certificate @ immutable) = ghost_ (
  match d with Start | Clean -> c
  | Fresh (rest, p, _, old, _) | Alias (rest, p, _, old) ->
    capture heads rest (C.Entry (p, heads p, capture_desc heads old.desc c)))
