module H = Pref.Heap

type node : immutable_data =
  | Var
  | Bool
  | Arrow of node Pref.t * node Pref.t
  | Link of node Pref.t

type ty = TVar of int | TBool | TArrow of ty * ty [@@inductive]

let[@def] (scoped @ total) (h : Pref.heap @ immutable)
    (p : node Pref.t @ immutable) =
  ghost_ (match H.at h p with
  | None -> false
  | Some (Var | Bool) -> true
  | Some (Link q) -> H.mem h q
  | Some (Arrow (a, b)) -> H.mem h a && H.mem h b)

let[@def] (equation @ total) (h : Pref.heap @ immutable)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (p : node Pref.t @ immutable) =
  ghost_ (match H.at h p with
  | None | Some Var -> true
  | Some Bool -> rho p === TBool
  | Some (Link q) -> rho p === rho q
  | Some (Arrow (a, b)) -> rho p === TArrow (rho a, rho b))

type resolution = Here | Via of node Pref.t * resolution [@@inductive]

let[@def] (terminal @ total) (h : Pref.heap @ immutable)
    (p : node Pref.t @ immutable) =
  ghost_ (match H.at h p with
  | Some (Var | Bool | Arrow _) -> true
  | None | Some (Link _) -> false)

let[@def] rec (resolves @ total) (h : Pref.heap @ immutable)
    (p : node Pref.t @ immutable) (r : node Pref.t @ immutable)
    (path : resolution @ immutable) =
  ghost_ (match path with
  | Here -> p === r && H.mem h p && terminal h p
  | Via (q, rest) -> H.mem h p && H.at h p === Some (Link q)
    && resolves h q r rest)

type resolved = #{
  value : node Pref.t;
  path : resolution Ghost.t;
}

type search =
  | Hit
  | Leaf
  | Follow of node Pref.t * search
  | Left of node Pref.t * node Pref.t * search
  | Both of node Pref.t * node Pref.t * search * search
  [@@inductive]

let[@def] rec (searched @ total) (h : Pref.heap @ immutable)
    (needle : node Pref.t @ immutable) (p : node Pref.t @ immutable)
    (found : bool) (trace : search @ immutable) =
  ghost_ (H.mem h p && match trace with
  | Hit -> p === needle && found
  | Leaf -> not (p === needle) && not found
    && (H.at h p === Some Var || H.at h p === Some Bool)
  | Follow (q, rest) -> not (p === needle) && H.at h p === Some (Link q)
    && searched h needle q found rest
  | Left (a, b, left) -> not (p === needle)
    && H.at h p === Some (Arrow (a, b)) && found
    && searched h needle a true left
  | Both (a, b, left, right) -> not (p === needle)
    && H.at h p === Some (Arrow (a, b))
    && searched h needle a false left && searched h needle b found right)

type searched_result = #{ found : bool; search : search Ghost.t }

type derivation =
  | Same
  | Swap of derivation
  | Constants
  | Bind_left of search
  | Bind_right of search
  | Occurs_left of search
  | Occurs_right of search
  | Clash
  | Resolve of node Pref.t * node Pref.t * resolution * resolution * derivation
  | Children of node Pref.t * node Pref.t * node Pref.t * node Pref.t
      * Pref.heap * bool * derivation * derivation
  [@@inductive]

let[@def] rec (unified @ total) (h : Pref.heap @ immutable)
    (p : node Pref.t @ immutable) (q : node Pref.t @ immutable)
    (ok : bool) (after : Pref.heap @ immutable) (d : derivation @ immutable) =
  ghost_ (H.mem h p && H.mem h q && match d with
  | Swap rest -> unified h q p ok after rest
  | Same -> p === q && ok && after === h
  | Constants -> H.at h p === Some Bool && H.at h q === Some Bool
    && ok && after === h
  | Bind_left search -> H.at h p === Some Var && not (p === q)
    && searched h p q false search && ok && after === H.put h p (Link q)
  | Bind_right search -> H.at h q === Some Var && not (p === q)
    && searched h q p false search && ok && after === H.put h q (Link p)
  | Occurs_left search -> H.at h p === Some Var && not (p === q)
    && terminal h q && searched h p q true search && not ok && after === h
  | Occurs_right search -> H.at h q === Some Var && not (p === q)
    && terminal h p && searched h q p true search && not ok && after === h
  | Clash -> not ok && after === h &&
    (match H.at h p, H.at h q with
     | Some Bool, Some (Arrow _) | Some (Arrow _), Some Bool -> true
     | _ -> false)
  | Resolve (r, s, rp, sq, rest) -> resolves h p r rp && resolves h q s sq
    && unified h r s ok after rest
  | Children (a, b, c, e, middle, left_ok, left, right) ->
    H.at h p === Some (Arrow (a, b)) && H.at h q === Some (Arrow (c, e))
    && unified h a c left_ok middle left
    && (if left_ok then unified middle b e ok after right
        else not ok && after === middle))

type result = #{
  ok : bool;
  state : Pref.token;
  derivation : derivation Ghost.t;
}

type edits = Unchanged | Set of node Pref.t * node Pref.t | Then of edits * edits
  [@@inductive]

let[@def] rec (apply_edits @ total) (h : Pref.heap @ immutable)
    (edits : edits @ immutable) = ghost_ (match edits with
  | Unchanged -> h
  | Set (p, q) -> H.put h p (Link q)
  | Then (left, right) -> apply_edits (apply_edits h left) right)

let[@def] rec (valid_edits @ total) (h : Pref.heap @ immutable)
    (edits : edits @ immutable) = ghost_ (match edits with
  | Unchanged -> true
  | Set (p, _) -> H.at h p === Some Var
  | Then (left, right) -> valid_edits h left
    && valid_edits (apply_edits h left) right)

let[@def] rec (equal_edits @ total)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (edits : edits @ immutable) = ghost_ (match edits with
  | Unchanged -> true
  | Set (p, q) -> rho p === rho q
  | Then (left, right) -> equal_edits rho left && equal_edits rho right)

let[@def] rec (writes @ total) (p : node Pref.t @ immutable)
    (q : node Pref.t @ immutable) (d : derivation @ immutable) =
  match d with
  | Bind_left _ -> Set (p, q)
  | Bind_right _ -> Set (q, p)
  | Swap rest -> writes q p rest
  | Resolve (r, s, _, _, rest) -> writes r s rest
  | Children (a, b, c, e, _, ok, left, right) ->
    if ok then Then (writes a c left, writes b e right) else writes a c left
  | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> Unchanged

let[@def] rec (weight @ total) (t : ty @ immutable) =
  match t with
  | TVar _ | TBool -> Bigint.one
  | TArrow (a, b) -> Bigint.add Bigint.one (Bigint.add (weight a) (weight b))
