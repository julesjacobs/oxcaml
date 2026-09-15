module H = Pref.Heap

open Copy_spec
open Level_spec
open Lower_locality_spec

let[@def] (observe @ total) (h : Pref.heap @ immutable) (p : node Pref.t @ immutable) =
  ghost_ (match H.at h p with None -> None | Some v -> Some v.desc)
let[@def] (redirect @ total) (h : Pref.heap @ immutable) (p : node Pref.t @ immutable)
    (q : node Pref.t @ immutable) = ghost_ (match H.at h p with
  | None -> cell (Link q) 0 | Some v -> {v with desc = Link q})

type ty = Copy_spec.ty = Variable of node Pref.t | Boolean | Function of ty * ty [@@inductive]

let[@def] (scoped @ total) (h : Pref.heap @ immutable)
    (p : node Pref.t @ immutable) =
  ghost_ (match observe h p with
  | None -> false
  | Some (Var | Bool) -> true
  | Some (Link q) -> H.mem h q
  | Some (Arrow (a, b)) -> H.mem h a && H.mem h b)

let[@def] (node_equation @ total) (h : Pref.heap @ immutable)
    (rho : (node Pref.t @ immutable total -> ty @ immutable total) @ total)
    (p : node Pref.t @ immutable) =
  ghost_ (match observe h p with
  | None | Some Var -> true
  | Some Bool -> rho p === Boolean
  | Some (Link q) -> rho p === rho q
  | Some (Arrow (a, b)) -> rho p === Function (rho a, rho b))

type resolution = Here | Via of node Pref.t * resolution [@@inductive]

let[@def] (terminal @ total) (h : Pref.heap @ immutable)
    (p : node Pref.t @ immutable) =
  ghost_ (match observe h p with
  | Some (Var | Bool | Arrow _) -> true
  | None | Some (Link _) -> false)

let[@def] rec (resolves @ total) (h : Pref.heap @ immutable)
    (p : node Pref.t @ immutable) (r : node Pref.t @ immutable)
    (path : resolution @ immutable) =
  ghost_ (match path with
  | Here -> p === r && H.mem h p && terminal h p
  | Via (q, rest) -> H.mem h p && observe h p === Some (Link q)
    && resolves h q r rest)

type resolved = #{
  value : node Pref.t;
  path : resolution @@ ghost;
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
    && (observe h p === Some Var || observe h p === Some Bool)
  | Follow (q, rest) -> not (p === needle) && observe h p === Some (Link q)
    && searched h needle q found rest
  | Left (a, b, left) -> not (p === needle)
    && observe h p === Some (Arrow (a, b)) && found
    && searched h needle a true left
  | Both (a, b, left, right) -> not (p === needle)
    && observe h p === Some (Arrow (a, b))
    && searched h needle a false left && searched h needle b found right)

type searched_result = #{ found : bool; search : search @@ ghost }

type trail = End | Trail of node Pref.t * trail [@@inductive]
type marks = No_marks | Marked of marks * node Pref.t * node * search [@@inductive]
let[@def] (set_visited @ total) (v : node @ immutable) (visited : bool) =
  {v with visited}
let[@def] rec (marked_heap @ total) (h : Pref.heap @ immutable)
    (d : marks @ immutable) = ghost_ (match d with
  | No_marks -> h
  | Marked (rest, p, old, _) ->
    H.put (marked_heap h rest) p (set_visited old true))
let[@def] rec (mark_trail @ total) (d : marks @ immutable) = ghost_ (
  match d with No_marks -> End
  | Marked (rest, p, _, _) -> Trail (p, mark_trail rest))
let[@def] rec (marked @ total) (d : marks @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (match d with
  | No_marks -> false
  | Marked (rest, p, _, _) -> p === x || marked rest x)
let[@def] rec (marks_valid @ total) (h : Pref.heap @ immutable)
    (needle : node Pref.t @ immutable) (d : marks @ immutable) = ghost_ (
  match d with No_marks -> true
  | Marked (rest, p, old, proof) -> marks_valid h needle rest
    && H.mem (marked_heap h rest) p
    && H.at (marked_heap h rest) p === Some old && not old.visited
    && searched h needle p false proof)
let[@def] (marked_at @ total) (h : Pref.heap @ immutable)
    (after : Pref.heap @ immutable) (d : marks @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (
  H.mem h x === H.mem after x && match H.at h x, H.at after x with
  | None, None -> true
  | Some a, Some b -> a.desc === b.desc && a.level === b.level
    && a.memo === b.memo && b.visited === (a.visited || marked d x)
  | _ -> false)
let[@def] rec (cached @ total) (d : marks @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (match d with
  | No_marks -> Leaf
  | Marked (rest, p, _, proof) -> if x === p then proof else cached rest x)
let[@def] rec (on_trail @ total) (trail : trail @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (match trail with
  | End -> false | Trail (p, rest) -> x === p || on_trail rest x)
let[@def] rec (reset_heap @ total) (h : Pref.heap @ immutable)
    (trail : trail @ immutable) = ghost_ (match trail with
  | End -> h
  | Trail (p, rest) -> match H.at h p with
    | None -> reset_heap h rest
    | Some v -> reset_heap (H.put h p (set_visited v false)) rest)
let[@def] (reset_at @ total) (h : Pref.heap @ immutable)
    (after : Pref.heap @ immutable) (trail : trail @ immutable)
    (x : node Pref.t @ immutable) = ghost_ (
  H.mem h x === H.mem after x && match H.at h x, H.at after x with
  | None, None -> true
  | Some a, Some b -> a.desc === b.desc && a.level === b.level
    && a.memo === b.memo
    && b.visited === (if on_trail trail x then false else a.visited)
  | _ -> false)

type scanning = #{state : Pref.token; found : bool; trail : trail @@ aliased;
  marks : marks @@ ghost; search : search @@ ghost}
type checked = #{state : Pref.token; found : bool;
  marks : marks @@ ghost; search : search @@ ghost}

let[@def] (scan_heap @ total) (h : Pref.heap @ immutable) (d : marks @ immutable) =
  ghost_ (reset_heap (marked_heap h d) (mark_trail d))

type derivation =
  | Scanned of node Pref.t * marks * derivation
  | Lowering of int * lowering * bounded * derivation
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
  ghost_ (H.mem h p && H.mem h q && active h p && active h q && match d with
  | Scanned (needle, marks, rest) -> marks_valid h needle marks
    && unified (scan_heap h marks) p q ok after rest
  | Lowering (bound, edits, tree, rest) -> lower_valid h bound edits
    && observe h p === Some Var && at_level h p === Finite bound
    && confined edits tree && bound_root tree === q
    && bounded (lower_heap h bound edits) bound tree
    && (match rest with Bind_left _ -> true | _ -> false)
    && unified (lower_heap h bound edits) p q ok after rest
  | Swap rest -> unified h q p ok after rest
  | Same -> p === q && ok && after === h
  | Constants -> observe h p === Some Bool && observe h q === Some Bool
    && ok && after === h
  | Bind_left search -> observe h p === Some Var && not (p === q)
    && (match at_level h p with Generic -> false | Finite n -> below h q n)
    && searched h p q false search && ok && after === H.put h p (redirect h p q)
  | Bind_right search -> observe h q === Some Var && not (p === q)
    && (match at_level h q with Generic -> false | Finite n -> below h p n)
    && searched h q p false search && ok && after === H.put h q (redirect h q p)
  | Occurs_left search -> observe h p === Some Var && not (p === q)
    && terminal h q && searched h p q true search && not ok && after === h
  | Occurs_right search -> observe h q === Some Var && not (p === q)
    && terminal h p && searched h q p true search && not ok && after === h
  | Clash -> not ok && after === h &&
    (match observe h p, observe h q with
     | Some Bool, Some (Arrow _) | Some (Arrow _), Some Bool -> true
     | _ -> false)
  | Resolve (r, s, rp, sq, rest) -> resolves h p r rp && resolves h q s sq
    && unified h r s ok after rest
  | Children (a, b, c, e, middle, left_ok, left, right) ->
    observe h p === Some (Arrow (a, b)) && observe h q === Some (Arrow (c, e))
    && unified h a c left_ok middle left
    && (if left_ok then unified middle b e ok after right
        else not ok && after === middle))

type result = #{
  ok : bool;
  state : Pref.token;
  derivation : derivation @@ ghost;
}

type edits = Scanned_edit of node Pref.t * marks | Lowered of int * lowering | Unchanged | Set of node Pref.t * node Pref.t | Then of edits * edits
  [@@inductive]

let[@def] rec (apply_edits @ total) (h : Pref.heap @ immutable)
    (edits : edits @ immutable) = ghost_ (match edits with
  | Scanned_edit (_, d) -> scan_heap h d
  | Lowered (bound, d) -> lower_heap h bound d
  | Unchanged -> h
  | Set (p, q) -> H.put h p (redirect h p q)
  | Then (left, right) -> apply_edits (apply_edits h left) right)

let[@def] rec (valid_edits @ total) (h : Pref.heap @ immutable)
    (edits : edits @ immutable) = ghost_ (match edits with
  | Scanned_edit (needle, d) -> marks_valid h needle d
  | Lowered (bound, d) -> lower_valid h bound d
  | Unchanged -> true
  | Set (p, _) -> observe h p === Some Var
  | Then (left, right) -> valid_edits h left
    && valid_edits (apply_edits h left) right)

let[@def] rec (writes @ total) (p : node Pref.t @ immutable)
    (q : node Pref.t @ immutable) (d : derivation @ immutable) =
  match d with
  | Scanned (needle, marks, rest) -> Then (Scanned_edit (needle, marks), writes p q rest)
  | Lowering (bound, edits, _, rest) -> Then (Lowered (bound, edits), writes p q rest)
  | Bind_left _ -> Set (p, q)
  | Bind_right _ -> Set (q, p)
  | Swap rest -> writes q p rest
  | Resolve (r, s, _, _, rest) -> writes r s rest
  | Children (a, b, c, e, _, ok, left, right) ->
    if ok then Then (writes a c left, writes b e right) else writes a c left
  | Same | Constants | Occurs_left _ | Occurs_right _ | Clash -> Unchanged

let[@def] rec (weight @ total) (t : ty @ immutable) =
  match t with
  | Variable _ | Boolean -> Bigint.one
  | Function (a, b) -> Bigint.add Bigint.one (Bigint.add (weight a) (weight b))
