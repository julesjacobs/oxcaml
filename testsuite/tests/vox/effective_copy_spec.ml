open Copy_spec

let[@def] (effective_target_for @ total) (saved : Pref.heap @ immutable) (heads : Effective_level.heads @ total) (d : history @ immutable)
    (p : node Pref.t @ immutable) (q : node Pref.t @ immutable) = ghost_ (
  H.mem saved p && match Effective_level.level saved heads p with
  | Finite _ -> q === p | Generic -> mapping d p === Some q)
let[@def] (effective_ready @ total) (saved : Pref.heap @ immutable) (heads : Effective_level.heads @ total) (d : history @ immutable)
    (source : desc @ immutable) (dest : desc @ immutable) = ghost_ (match source, dest with
  | Var, Var | Bool, Bool -> true
  | Arrow (a, b), Arrow (x, y) -> effective_target_for saved heads d a x && effective_target_for saved heads d b y
  | _ -> false)
let[@def] rec (effective_valid @ total) (saved : Pref.heap @ immutable) (heads : Effective_level.heads @ total)
    (epoch : node Pref.t @ immutable) (depth : int) (d : history @ immutable) = ghost_ (
  match d with
  | Start -> not (H.mem saved epoch) && depth >= 0
  | Clean -> H.mem saved epoch && depth >= 0
  | Fresh (rest, p, q, old, desc) -> effective_valid saved heads epoch depth rest
    && H.mem saved p && H.at (heap saved epoch depth rest) p === Some old
    && Effective_level.level saved heads p === Generic && mapping rest p === None
    && not (H.mem (heap saved epoch depth rest) q) && effective_ready saved heads rest old.desc desc
  | Alias (rest, p, q, old) -> effective_valid saved heads epoch depth rest
    && H.mem saved p && H.at (heap saved epoch depth rest) p === Some old
    && Effective_level.level saved heads p === Generic && mapping rest p === None
    && (match old.desc with Link x -> effective_target_for saved heads rest x q | _ -> false))

let[@def] (effective_instance_at @ total) (saved : Pref.heap @ immutable)
    (heads : Effective_level.heads @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (want : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (p : node Pref.t @ immutable) = ghost_ (match H.at saved p with
  | None -> true | Some v -> match Effective_level.level saved heads p with
  | Finite _ -> want p === rho p
  | Generic -> match v.desc with Var -> true | Bool -> want p === Boolean
    | Arrow (a, b) -> want p === Function (want a, want b) | Link q -> want p === want q)

let[@def] (effective_available @ total) (saved : Pref.heap @ immutable)
    (heads : Effective_level.heads @ total) (d : history @ immutable)
    (p : node Pref.t @ immutable) = ghost_ (H.mem saved p && match Effective_level.level saved heads p with
  | Finite _ -> true | Generic -> match mapping d p with None -> false | Some _ -> true)

let[@def] (effective_image @ total) (saved : Pref.heap @ immutable)
    (heads : Effective_level.heads @ total) (d : history @ immutable)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (p : node Pref.t @ immutable) = ghost_ (match Effective_level.level saved heads p with
  | Generic -> (match mapping d p with Some q -> rho q | None -> rho p)
  | Finite _ -> rho p)

let[@def] (effective_children_available @ total) (saved : Pref.heap @ immutable)
    (heads : Effective_level.heads @ total) (d : history @ immutable)
    (desc : desc @ immutable) = ghost_ (match desc with Var | Bool -> true
  | Link q -> effective_available saved heads d q
  | Arrow (a, b) -> effective_available saved heads d a && effective_available saved heads d b)
