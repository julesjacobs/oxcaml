type t =
  | Leaf
  | Node of t * int * t

external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"

let empty = Leaf

type direction =
  | Same
  | Left
  | Right

let[@vox.def] direction (new_key : int) (key : int) : direction =
  if int_equal new_key key
  then Same
  else if int_less new_key key
  then Left
  else Right

(* Search membership: one comparison per level, descending a single spine.
   Correct only on ordered trees, which is what [invariant] records. *)
let[@vox.def] rec member (query : int) (tree : t @ logical) =
  match tree with
  | Leaf -> false
  | Node (left, key, right) ->
    if int_equal query key
    then true
    else if int_less query key
    then member query left
    else member query right

(* Occurrence anywhere in the tree.  The rotations are stated against this,
   because they are ordering-free rearrangements; [member_occurs] below is
   what carries their conclusions back to the spine. *)
let[@vox.def] rec occurs (query : int) (tree : t @ logical) =
  match tree with
  | Leaf -> false
  | Node (left, key, right) ->
    int_equal query key || occurs query left || occurs query right

let[@vox.def] rec below (tree : t @ logical) (bound : int) =
  match tree with
  | Leaf -> true
  | Node (left, key, right) ->
    int_less key bound && below left bound && below right bound

let[@vox.def] rec above (tree : t @ logical) (bound : int) =
  match tree with
  | Leaf -> true
  | Node (left, key, right) ->
    int_less bound key && above left bound && above right bound

let[@vox.def] rec ordered (tree : t @ logical) =
  match tree with
  | Leaf -> true
  | Node (left, key, right) ->
    ordered left && ordered right && below left key && above right key

let rec below_weaken (hi : int) (lo : int{ _ < hi }) (tree : t @ logical)
    (_bounded : unit{ below tree lo = true })
    : unit{ below tree hi = true } =
  match tree with
  | Leaf ->
    below_def Leaf hi;
    ()
  | Node (left, key, right) ->
    below_def (Node (left, key, right)) lo;
    below_def (Node (left, key, right)) hi;
    below_weaken hi lo left ();
    below_weaken hi lo right ();
    ()

let rec above_weaken (lo : int) (hi : int{ lo < _ }) (tree : t @ logical)
    (_bounded : unit{ above tree hi = true })
    : unit{ above tree lo = true } =
  match tree with
  | Leaf ->
    above_def Leaf lo;
    ()
  | Node (left, key, right) ->
    above_def (Node (left, key, right)) hi;
    above_def (Node (left, key, right)) lo;
    above_weaken lo hi left ();
    above_weaken lo hi right ();
    ()

let rec below_absent (bound : int) (query : int{ bound < _ })
    (tree : t @ logical)
    (_bounded : unit{ below tree bound = true })
    : unit{ occurs query tree = false } =
  match tree with
  | Leaf ->
    occurs_def query Leaf;
    ()
  | Node (left, key, right) ->
    below_def (Node (left, key, right)) bound;
    occurs_def query (Node (left, key, right));
    below_absent bound query left ();
    below_absent bound query right ();
    ()

let rec above_absent (bound : int) (query : int{ _ < bound })
    (tree : t @ logical)
    (_bounded : unit{ above tree bound = true })
    : unit{ occurs query tree = false } =
  match tree with
  | Leaf ->
    occurs_def query Leaf;
    ()
  | Node (left, key, right) ->
    above_def (Node (left, key, right)) bound;
    occurs_def query (Node (left, key, right));
    above_absent bound query left ();
    above_absent bound query right ();
    ()

(* On an ordered tree the single spine finds exactly the keys that occur. *)
let rec member_occurs (query : int) (tree : t @ logical)
    (_ordered : unit{ ordered tree = true })
    : unit{ member query tree = occurs query tree } =
  match tree with
  | Leaf ->
    member_def query Leaf;
    occurs_def query Leaf;
    ()
  | Node (left, key, right) ->
    ordered_def (Node (left, key, right));
    member_def query (Node (left, key, right));
    occurs_def query (Node (left, key, right));
    member_occurs query left ();
    member_occurs query right ();
    let choice = direction query key in
    direction_def query key;
    match choice with
    | Same -> ()
    | Left -> above_absent key query right ()
    | Right -> below_absent key query left ()

let[@vox.def] rotate_right (tree : t @ logical) : t =
  match tree with
  | Leaf -> tree
  | Node (l, y, c) ->
    match l with
    | Leaf -> tree
    | Node (a, x, b) -> Node (a, x, Node (b, y, c))

let[@vox.def] rotate_left (tree : t @ logical) : t =
  match tree with
  | Leaf -> tree
  | Node (a, x, r) ->
    match r with
    | Leaf -> tree
    | Node (b, y, c) -> Node (Node (a, x, b), y, c)

let rotate_right_preserves_occurs (tree : t @ logical) (query : int)
    : unit{ occurs query (rotate_right tree) = occurs query tree }
  =
  let _ = rotate_right_def tree in
  match tree with
  | Leaf -> ()
  | Node (l, y, c) ->
    let _ = occurs_def query (Node (l, y, c)) in
    match l with
    | Leaf -> ()
    | Node (a, x, b) ->
      let _ = occurs_def query (Node (a, x, b)) in
      let _ = occurs_def query (Node (a, x, Node (b, y, c))) in
      let _ = occurs_def query (Node (b, y, c)) in
      ()

let rotate_left_preserves_occurs (tree : t @ logical) (query : int)
    : unit{ occurs query (rotate_left tree) = occurs query tree }
  =
  let _ = rotate_left_def tree in
  match tree with
  | Leaf -> ()
  | Node (a, x, r) ->
    let _ = occurs_def query (Node (a, x, r)) in
    match r with
    | Leaf -> ()
    | Node (b, y, c) ->
      let _ = occurs_def query (Node (b, y, c)) in
      let _ = occurs_def query (Node (Node (a, x, b), y, c)) in
      let _ = occurs_def query (Node (a, x, b)) in
      ()

let rotate_right_below (tree : t @ logical) (bound : int)
    : unit{ below (rotate_right tree) bound = below tree bound } =
  rotate_right_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, y, c) ->
    below_def (Node (l, y, c)) bound;
    match l with
    | Leaf -> ()
    | Node (a, x, b) ->
      below_def (Node (a, x, b)) bound;
      below_def (Node (a, x, Node (b, y, c))) bound;
      below_def (Node (b, y, c)) bound;
      ()

let rotate_right_above (tree : t @ logical) (bound : int)
    : unit{ above (rotate_right tree) bound = above tree bound } =
  rotate_right_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, y, c) ->
    above_def (Node (l, y, c)) bound;
    match l with
    | Leaf -> ()
    | Node (a, x, b) ->
      above_def (Node (a, x, b)) bound;
      above_def (Node (a, x, Node (b, y, c))) bound;
      above_def (Node (b, y, c)) bound;
      ()

let rotate_left_below (tree : t @ logical) (bound : int)
    : unit{ below (rotate_left tree) bound = below tree bound } =
  rotate_left_def tree;
  match tree with
  | Leaf -> ()
  | Node (a, x, r) ->
    below_def (Node (a, x, r)) bound;
    match r with
    | Leaf -> ()
    | Node (b, y, c) ->
      below_def (Node (b, y, c)) bound;
      below_def (Node (Node (a, x, b), y, c)) bound;
      below_def (Node (a, x, b)) bound;
      ()

let rotate_left_above (tree : t @ logical) (bound : int)
    : unit{ above (rotate_left tree) bound = above tree bound } =
  rotate_left_def tree;
  match tree with
  | Leaf -> ()
  | Node (a, x, r) ->
    above_def (Node (a, x, r)) bound;
    match r with
    | Leaf -> ()
    | Node (b, y, c) ->
      above_def (Node (b, y, c)) bound;
      above_def (Node (Node (a, x, b), y, c)) bound;
      above_def (Node (a, x, b)) bound;
      ()

let rotate_right_ordered (tree : t @ logical)
    (_ordered : unit{ ordered tree = true })
    : unit{ ordered (rotate_right tree) = true } =
  rotate_right_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, y, c) ->
    ordered_def (Node (l, y, c));
    below_def (Node (l, y, c)) y;
    match l with
    | Leaf -> ()
    | Node (a, x, b) ->
      ordered_def (Node (a, x, b));
      below_def (Node (a, x, b)) y;
      ordered_def (Node (a, x, Node (b, y, c)));
      ordered_def (Node (b, y, c));
      above_def (Node (b, y, c)) x;
      above_weaken x y c ();
      ()

let rotate_left_ordered (tree : t @ logical)
    (_ordered : unit{ ordered tree = true })
    : unit{ ordered (rotate_left tree) = true } =
  rotate_left_def tree;
  match tree with
  | Leaf -> ()
  | Node (a, x, r) ->
    ordered_def (Node (a, x, r));
    match r with
    | Leaf -> ()
    | Node (b, y, c) ->
      ordered_def (Node (b, y, c));
      above_def (Node (b, y, c)) x;
      ordered_def (Node (Node (a, x, b), y, c));
      ordered_def (Node (a, x, b));
      below_def (Node (a, x, b)) y;
      below_weaken y x a ();
      ()

let node_occurs_congruence_left (l1 : t @ logical) (l2 : t @ logical)
    (k : int) (r : t @ logical) (query : int)
    (_eq : unit{ occurs query l1 = occurs query l2 })
    : unit{
      occurs query (Node (l1, k, r)) = occurs query (Node (l2, k, r))
    }
  =
  let _ = occurs_def query (Node (l1, k, r)) in
  let _ = occurs_def query (Node (l2, k, r)) in
  ()

let node_occurs_congruence_right (l : t @ logical) (k : int)
    (r1 : t @ logical) (r2 : t @ logical) (query : int)
    (_eq : unit{ occurs query r1 = occurs query r2 })
    : unit{
      occurs query (Node (l, k, r1)) = occurs query (Node (l, k, r2))
    }
  =
  let _ = occurs_def query (Node (l, k, r1)) in
  let _ = occurs_def query (Node (l, k, r2)) in
  ()

let[@vox.def] rotate_left_right (tree : t @ logical) : t =
  match tree with
  | Leaf -> tree
  | Node (l, k, r) -> rotate_right (Node (rotate_left l, k, r))

let[@vox.def] rotate_right_left (tree : t @ logical) : t =
  match tree with
  | Leaf -> tree
  | Node (l, k, r) -> rotate_left (Node (l, k, rotate_right r))

let rotate_left_right_preserves_occurs (tree : t @ logical) (query : int)
    : unit{ occurs query (rotate_left_right tree) = occurs query tree }
  =
  let _ = rotate_left_right_def tree in
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    let rl = rotate_left_preserves_occurs l query in
    let _ = node_occurs_congruence_left (rotate_left l) l k r query rl in
    let _ =
      rotate_right_preserves_occurs (Node (rotate_left l, k, r)) query
    in
    ()

let rotate_right_left_preserves_occurs (tree : t @ logical) (query : int)
    : unit{ occurs query (rotate_right_left tree) = occurs query tree }
  =
  let _ = rotate_right_left_def tree in
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    let rr = rotate_right_preserves_occurs r query in
    let _ = node_occurs_congruence_right l k (rotate_right r) r query rr in
    let _ =
      rotate_left_preserves_occurs (Node (l, k, rotate_right r)) query
    in
    ()

let rotate_left_right_below (tree : t @ logical) (bound : int)
    : unit{ below (rotate_left_right tree) bound = below tree bound } =
  rotate_left_right_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    rotate_left_below l bound;
    rotate_right_below (Node (rotate_left l, k, r)) bound;
    below_def (Node (rotate_left l, k, r)) bound;
    below_def (Node (l, k, r)) bound;
    ()

let rotate_left_right_above (tree : t @ logical) (bound : int)
    : unit{ above (rotate_left_right tree) bound = above tree bound } =
  rotate_left_right_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    rotate_left_above l bound;
    rotate_right_above (Node (rotate_left l, k, r)) bound;
    above_def (Node (rotate_left l, k, r)) bound;
    above_def (Node (l, k, r)) bound;
    ()

let rotate_right_left_below (tree : t @ logical) (bound : int)
    : unit{ below (rotate_right_left tree) bound = below tree bound } =
  rotate_right_left_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    rotate_right_below r bound;
    rotate_left_below (Node (l, k, rotate_right r)) bound;
    below_def (Node (l, k, rotate_right r)) bound;
    below_def (Node (l, k, r)) bound;
    ()

let rotate_right_left_above (tree : t @ logical) (bound : int)
    : unit{ above (rotate_right_left tree) bound = above tree bound } =
  rotate_right_left_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    rotate_right_above r bound;
    rotate_left_above (Node (l, k, rotate_right r)) bound;
    above_def (Node (l, k, rotate_right r)) bound;
    above_def (Node (l, k, r)) bound;
    ()

let rotate_left_right_ordered (tree : t @ logical)
    (_ordered : unit{ ordered tree = true })
    : unit{ ordered (rotate_left_right tree) = true } =
  rotate_left_right_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    ordered_def (Node (l, k, r));
    rotate_left_ordered l ();
    rotate_left_below l k;
    ordered_def (Node (rotate_left l, k, r));
    rotate_right_ordered (Node (rotate_left l, k, r)) ();
    ()

let rotate_right_left_ordered (tree : t @ logical)
    (_ordered : unit{ ordered tree = true })
    : unit{ ordered (rotate_right_left tree) = true } =
  rotate_right_left_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    ordered_def (Node (l, k, r));
    rotate_right_ordered r ();
    rotate_right_above r k;
    ordered_def (Node (l, k, rotate_right r));
    rotate_left_ordered (Node (l, k, rotate_right r)) ();
    ()

(* Heights are mathematical integers.  With machine [int] heights the
   balance invariant below would only hold under an unstated assumption
   that no tree is deep enough for [height] to wrap, and nothing here
   bounds the size of a tree. *)
let[@vox.def] max_height (a : Bigint.t @ logical) (b : Bigint.t @ logical) =
  if Bigint.lt a b then b else a

let[@vox.def] rec height (tree : t @ logical) : Bigint.t =
  match tree with
  | Leaf -> Bigint.zero
  | Node (l, _, r) ->
    Bigint.add Bigint.one (max_height (height l) (height r))

let rec height_nonneg (tree : t @ logical)
    : unit{ Bigint.ge (height tree) Bigint.zero = true } =
  match tree with
  | Leaf ->
    height_def Leaf;
    ()
  | Node (l, k, r) ->
    height_def (Node (l, k, r));
    max_height_def (height l) (height r);
    height_nonneg l;
    height_nonneg r;
    ()

(* Height balance: at every node the two subtrees differ in height by at
   most one. *)
let[@vox.def] rec balanced (tree : t @ logical) =
  match tree with
  | Leaf -> true
  | Node (l, _, r) ->
    balanced l && balanced r
    && Bigint.le (height l) (Bigint.add (height r) Bigint.one)
    && Bigint.le (height r) (Bigint.add (height l) Bigint.one)

(* Single rotation when the outer grandchild is the taller one, double
   rotation when the inner one is. *)
let[@vox.def] rebalance (tree : t @ logical) : t =
  match tree with
  | Leaf -> tree
  | Node (l, _, r) ->
    if Bigint.lt (Bigint.add (height r) Bigint.one) (height l)
    then
      (match l with
       | Leaf -> tree
       | Node (ll, _, lr) ->
         if Bigint.le (height lr) (height ll)
         then rotate_right tree
         else rotate_left_right tree)
    else if Bigint.lt (Bigint.add (height l) Bigint.one) (height r)
    then
      (match r with
       | Leaf -> tree
       | Node (rl, _, rr) ->
         if Bigint.le (height rl) (height rr)
         then rotate_left tree
         else rotate_right_left tree)
    else tree

let rebalance_preserves_occurs (tree : t @ logical) (query : int)
    : unit{ occurs query (rebalance tree) = occurs query tree }
  =
  rebalance_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, _k, r) ->
    if Bigint.lt (Bigint.add (height r) Bigint.one) (height l)
    then
      (match l with
       | Leaf -> ()
       | Node (ll, _lk, lr) ->
         if Bigint.le (height lr) (height ll)
         then rotate_right_preserves_occurs tree query
         else rotate_left_right_preserves_occurs tree query)
    else if Bigint.lt (Bigint.add (height l) Bigint.one) (height r)
    then
      (match r with
       | Leaf -> ()
       | Node (rl, _rk, rr) ->
         if Bigint.le (height rl) (height rr)
         then rotate_left_preserves_occurs tree query
         else rotate_right_left_preserves_occurs tree query)
    else ()

let rebalance_below (tree : t @ logical) (bound : int)
    : unit{ below (rebalance tree) bound = below tree bound } =
  rebalance_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, _k, r) ->
    if Bigint.lt (Bigint.add (height r) Bigint.one) (height l)
    then
      (match l with
       | Leaf -> ()
       | Node (ll, _lk, lr) ->
         if Bigint.le (height lr) (height ll)
         then rotate_right_below tree bound
         else rotate_left_right_below tree bound)
    else if Bigint.lt (Bigint.add (height l) Bigint.one) (height r)
    then
      (match r with
       | Leaf -> ()
       | Node (rl, _rk, rr) ->
         if Bigint.le (height rl) (height rr)
         then rotate_left_below tree bound
         else rotate_right_left_below tree bound)
    else ()

let rebalance_above (tree : t @ logical) (bound : int)
    : unit{ above (rebalance tree) bound = above tree bound } =
  rebalance_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, _k, r) ->
    if Bigint.lt (Bigint.add (height r) Bigint.one) (height l)
    then
      (match l with
       | Leaf -> ()
       | Node (ll, _lk, lr) ->
         if Bigint.le (height lr) (height ll)
         then rotate_right_above tree bound
         else rotate_left_right_above tree bound)
    else if Bigint.lt (Bigint.add (height l) Bigint.one) (height r)
    then
      (match r with
       | Leaf -> ()
       | Node (rl, _rk, rr) ->
         if Bigint.le (height rl) (height rr)
         then rotate_left_above tree bound
         else rotate_right_left_above tree bound)
    else ()

let rebalance_ordered (tree : t @ logical)
    (_ordered : unit{ ordered tree = true })
    : unit{ ordered (rebalance tree) = true } =
  rebalance_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, _k, r) ->
    if Bigint.lt (Bigint.add (height r) Bigint.one) (height l)
    then
      (match l with
       | Leaf -> ()
       | Node (ll, _lk, lr) ->
         if Bigint.le (height lr) (height ll)
         then rotate_right_ordered tree ()
         else rotate_left_right_ordered tree ())
    else if Bigint.lt (Bigint.add (height l) Bigint.one) (height r)
    then
      (match r with
       | Leaf -> ()
       | Node (rl, _rk, rr) ->
         if Bigint.le (height rl) (height rr)
         then rotate_left_ordered tree ()
         else rotate_right_left_ordered tree ())
    else ()

(* Okasaki-style statement of the rebalance step: from subtrees that are
   themselves balanced and out of step by at most two, [rebalance] returns a
   balanced tree whose height is that of the unrebalanced node or one less,
   and is exactly that height when no rotation was needed. *)
let rebalance_ok (l : t @ logical) (key : int) (r : t @ logical)
    (_hypotheses : unit{
       balanced l = true
       && balanced r = true
       && Bigint.le (height l)
            (Bigint.add (height r) (Bigint.add Bigint.one Bigint.one)) = true
       && Bigint.le (height r)
            (Bigint.add (height l) (Bigint.add Bigint.one Bigint.one)) = true
     })
    : unit{
      balanced (rebalance (Node (l, key, r))) = true
      && Bigint.le (height (rebalance (Node (l, key, r))))
           (height (Node (l, key, r))) = true
      && Bigint.le (Bigint.sub (height (Node (l, key, r))) Bigint.one)
           (height (rebalance (Node (l, key, r)))) = true
      && (Bigint.le (height l) (Bigint.add (height r) Bigint.one) = false
          || Bigint.le (height r) (Bigint.add (height l) Bigint.one) = false
          || Bigint.equal (height (rebalance (Node (l, key, r))))
               (height (Node (l, key, r))) = true)
    }
  =
  rebalance_def (Node (l, key, r));
  height_def (Node (l, key, r));
  max_height_def (height l) (height r);
  height_nonneg l;
  height_nonneg r;
  balanced_def (Node (l, key, r));
  if Bigint.lt (Bigint.add (height r) Bigint.one) (height l)
  then
    (match l with
     | Leaf ->
       height_def Leaf;
       ()
     | Node (ll, lk, lr) ->
       balanced_def (Node (ll, lk, lr));
       height_def (Node (ll, lk, lr));
       max_height_def (height ll) (height lr);
       height_nonneg ll;
       height_nonneg lr;
       if Bigint.le (height lr) (height ll)
       then begin
         rotate_right_def (Node (l, key, r));
         height_def (Node (ll, lk, Node (lr, key, r)));
         height_def (Node (lr, key, r));
         max_height_def (height ll) (height (Node (lr, key, r)));
         max_height_def (height lr) (height r);
         balanced_def (Node (ll, lk, Node (lr, key, r)));
         balanced_def (Node (lr, key, r));
         ()
       end
       else
         (match lr with
          | Leaf ->
            height_def Leaf;
            ()
          | Node (b, y, c) ->
            balanced_def (Node (b, y, c));
            height_def (Node (b, y, c));
            max_height_def (height b) (height c);
            height_nonneg b;
            height_nonneg c;
            rotate_left_right_def (Node (l, key, r));
            rotate_left_def l;
            rotate_right_def (Node (rotate_left l, key, r));
            height_def (Node (Node (ll, lk, b), y, Node (c, key, r)));
            height_def (Node (ll, lk, b));
            height_def (Node (c, key, r));
            max_height_def
              (height (Node (ll, lk, b))) (height (Node (c, key, r)));
            max_height_def (height ll) (height b);
            max_height_def (height c) (height r);
            balanced_def (Node (Node (ll, lk, b), y, Node (c, key, r)));
            balanced_def (Node (ll, lk, b));
            balanced_def (Node (c, key, r));
            ()))
  else if Bigint.lt (Bigint.add (height l) Bigint.one) (height r)
  then
    (match r with
     | Leaf ->
       height_def Leaf;
       ()
     | Node (rl, rk, rr) ->
       balanced_def (Node (rl, rk, rr));
       height_def (Node (rl, rk, rr));
       max_height_def (height rl) (height rr);
       height_nonneg rl;
       height_nonneg rr;
       if Bigint.le (height rl) (height rr)
       then begin
         rotate_left_def (Node (l, key, r));
         height_def (Node (Node (l, key, rl), rk, rr));
         height_def (Node (l, key, rl));
         max_height_def (height (Node (l, key, rl))) (height rr);
         max_height_def (height l) (height rl);
         balanced_def (Node (Node (l, key, rl), rk, rr));
         balanced_def (Node (l, key, rl));
         ()
       end
       else
         (match rl with
          | Leaf ->
            height_def Leaf;
            ()
          | Node (b, y, c) ->
            balanced_def (Node (b, y, c));
            height_def (Node (b, y, c));
            max_height_def (height b) (height c);
            height_nonneg b;
            height_nonneg c;
            rotate_right_left_def (Node (l, key, r));
            rotate_right_def r;
            rotate_left_def (Node (l, key, rotate_right r));
            height_def (Node (Node (l, key, b), y, Node (c, rk, rr)));
            height_def (Node (l, key, b));
            height_def (Node (c, rk, rr));
            max_height_def
              (height (Node (l, key, b))) (height (Node (c, rk, rr)));
            max_height_def (height l) (height b);
            max_height_def (height c) (height rr);
            balanced_def (Node (Node (l, key, b), y, Node (c, rk, rr)));
            balanced_def (Node (l, key, b));
            balanced_def (Node (c, rk, rr));
            ()))
  else ()

let[@vox.def] rec insert (new_key : int) (tree : t @ logical) : t =
  match tree with
  | Leaf -> Node (Leaf, new_key, Leaf)
  | Node (left, key, right) ->
    if int_equal new_key key
    then tree
    else if int_less new_key key
    then rebalance (Node (insert new_key left, key, right))
    else rebalance (Node (left, key, insert new_key right))

let rec insert_below (bound : int) (new_key : int{ _ < bound })
    (tree : t @ logical)
    (_bounded : unit{ below tree bound = true })
    : unit{ below (insert new_key tree) bound = true } =
  match tree with
  | Leaf ->
    insert_def new_key Leaf;
    below_def (Node (Leaf, new_key, Leaf)) bound;
    below_def Leaf bound;
    ()
  | Node (left, key, right) ->
    insert_def new_key (Node (left, key, right));
    below_def (Node (left, key, right)) bound;
    let choice = direction new_key key in
    direction_def new_key key;
    match choice with
    | Same -> ()
    | Left ->
      insert_below bound new_key left ();
      rebalance_below (Node (insert new_key left, key, right)) bound;
      below_def (Node (insert new_key left, key, right)) bound;
      ()
    | Right ->
      insert_below bound new_key right ();
      rebalance_below (Node (left, key, insert new_key right)) bound;
      below_def (Node (left, key, insert new_key right)) bound;
      ()

let rec insert_above (bound : int) (new_key : int{ bound < _ })
    (tree : t @ logical)
    (_bounded : unit{ above tree bound = true })
    : unit{ above (insert new_key tree) bound = true } =
  match tree with
  | Leaf ->
    insert_def new_key Leaf;
    above_def (Node (Leaf, new_key, Leaf)) bound;
    above_def Leaf bound;
    ()
  | Node (left, key, right) ->
    insert_def new_key (Node (left, key, right));
    above_def (Node (left, key, right)) bound;
    let choice = direction new_key key in
    direction_def new_key key;
    match choice with
    | Same -> ()
    | Left ->
      insert_above bound new_key left ();
      rebalance_above (Node (insert new_key left, key, right)) bound;
      above_def (Node (insert new_key left, key, right)) bound;
      ()
    | Right ->
      insert_above bound new_key right ();
      rebalance_above (Node (left, key, insert new_key right)) bound;
      above_def (Node (left, key, insert new_key right)) bound;
      ()

let rec insert_ordered (new_key : int) (tree : t @ logical)
    (_ordered : unit{ ordered tree = true })
    : unit{ ordered (insert new_key tree) = true } =
  match tree with
  | Leaf ->
    insert_def new_key Leaf;
    ordered_def (Node (Leaf, new_key, Leaf));
    ordered_def Leaf;
    below_def Leaf new_key;
    above_def Leaf new_key;
    ()
  | Node (left, key, right) ->
    insert_def new_key (Node (left, key, right));
    ordered_def (Node (left, key, right));
    let choice = direction new_key key in
    direction_def new_key key;
    match choice with
    | Same -> ()
    | Left ->
      insert_ordered new_key left ();
      insert_below key new_key left ();
      ordered_def (Node (insert new_key left, key, right));
      rebalance_ordered (Node (insert new_key left, key, right)) ();
      ()
    | Right ->
      insert_ordered new_key right ();
      insert_above key new_key right ();
      ordered_def (Node (left, key, insert new_key right));
      rebalance_ordered (Node (left, key, insert new_key right)) ();
      ()

let rec insert_balanced (new_key : int) (tree : t @ logical)
    (_balanced : unit{ balanced tree = true })
    : unit{
      balanced (insert new_key tree) = true
      && Bigint.le (height tree) (height (insert new_key tree)) = true
      && Bigint.le (height (insert new_key tree))
           (Bigint.add (height tree) Bigint.one) = true
    }
  =
  match tree with
  | Leaf ->
    insert_def new_key Leaf;
    balanced_def (Node (Leaf, new_key, Leaf));
    balanced_def Leaf;
    height_def (Node (Leaf, new_key, Leaf));
    height_def Leaf;
    max_height_def (height Leaf) (height Leaf);
    ()
  | Node (left, key, right) ->
    insert_def new_key (Node (left, key, right));
    balanced_def (Node (left, key, right));
    height_def (Node (left, key, right));
    max_height_def (height left) (height right);
    let choice = direction new_key key in
    direction_def new_key key;
    match choice with
    | Same -> ()
    | Left ->
      insert_balanced new_key left ();
      rebalance_ok (insert new_key left) key right ();
      height_def (Node (insert new_key left, key, right));
      max_height_def (height (insert new_key left)) (height right);
      ()
    | Right ->
      insert_balanced new_key right ();
      rebalance_ok left key (insert new_key right) ();
      height_def (Node (left, key, insert new_key right));
      max_height_def (height left) (height (insert new_key right));
      ()

let occurs_insert_leaf (new_key : int) (query : int)
    : unit{
      occurs query (insert new_key Leaf)
      = (query = new_key || occurs query Leaf)
    }
  =
  let _ = insert_def new_key Leaf in
  let _ = occurs_def query (Node (Leaf, new_key, Leaf)) in
  let _ = occurs_def query Leaf in
  ()

let occurs_insert_same (key : int) (new_key : int{ _ = key })
    (left : t @ logical) (right : t @ logical) (query : int)
    : unit{
      occurs query (insert new_key (Node (left, key, right)))
      = (query = new_key || occurs query (Node (left, key, right)))
    }
  =
  let _ = insert_def new_key (Node (left, key, right)) in
  let _ = occurs_def query (Node (left, key, right)) in
  ()

let occurs_insert_left (key : int) (new_key : int{ _ < key })
    (left : t @ logical) (right : t @ logical) (query : int)
    (_ih : unit{
       occurs query (insert new_key left)
       = (query = new_key || occurs query left)
     })
    : unit{
      occurs query (insert new_key (Node (left, key, right)))
      = (query = new_key || occurs query (Node (left, key, right)))
    }
  =
  let _ = insert_def new_key (Node (left, key, right)) in
  let _ =
    rebalance_preserves_occurs
      (Node (insert new_key left, key, right)) query
  in
  let _ = occurs_def query (Node (insert new_key left, key, right)) in
  let _ = occurs_def query (Node (left, key, right)) in
  ()

let occurs_insert_right (key : int)
    (new_key : int{ _ <> key && not (_ < key) })
    (left : t @ logical) (right : t @ logical) (query : int)
    (_ih : unit{
       occurs query (insert new_key right)
       = (query = new_key || occurs query right)
     })
    : unit{
      occurs query (insert new_key (Node (left, key, right)))
      = (query = new_key || occurs query (Node (left, key, right)))
    }
  =
  let _ = insert_def new_key (Node (left, key, right)) in
  let _ =
    rebalance_preserves_occurs
      (Node (left, key, insert new_key right)) query
  in
  let _ = occurs_def query (Node (left, key, insert new_key right)) in
  let _ = occurs_def query (Node (left, key, right)) in
  ()

let rec occurs_insert (new_key : int) (tree : t @ logical) (query : int)
    : unit{
      occurs query (insert new_key tree)
      = (query = new_key || occurs query tree)
    }
  =
  match tree with
  | Leaf -> occurs_insert_leaf new_key query
  | Node (left, key, right) ->
    let choice = direction new_key key in
    let _choice = direction_def new_key key in
    match choice with
    | Same -> occurs_insert_same key new_key left right query
    | Left ->
      let ih = occurs_insert new_key left query in
      occurs_insert_left key new_key left right query ih
    | Right ->
      let ih = occurs_insert new_key right query in
      occurs_insert_right key new_key left right query ih

(* An AVL tree: ordered, and balanced in height. *)
let[@vox.def] invariant (tree : t @ logical) =
  ordered tree && balanced tree

let empty_law ~(query : int) : unit{ member query empty = false } =
  member_def query empty

let empty_invariant : unit{ invariant empty = true } =
  invariant_def empty;
  ordered_def empty;
  balanced_def empty;
  ()

let insert_invariant ~(inserted : int) ~(tree : t @ logical)
    ~(well_formed : unit{ invariant tree = true })
    : unit{ invariant (insert inserted tree) = true } =
  invariant_def tree;
  invariant_def (insert inserted tree);
  insert_ordered inserted tree ();
  insert_balanced inserted tree ();
  ()

let insert_law ~(inserted : int) ~(tree : t @ logical) ~(query : int)
    ~(well_formed : unit{ invariant tree = true })
    : unit{
      member query (insert inserted tree)
      = ((query = inserted) || member query tree)
    }
  =
  invariant_def tree;
  insert_ordered inserted tree ();
  member_occurs query tree ();
  member_occurs query (insert inserted tree) ();
  occurs_insert inserted tree query;
  ()

type membership_side =
  | First
  | Second
  | Neither

let[@vox.def] membership_side first_member second_member =
  if first_member
  then First
  else if second_member then Second else Neither

let[@vox.def] rec agrees (t1 : t @ logical) (t2 : t @ logical)
    (nodes : t @ logical) =
  match nodes with
  | Leaf -> true
  | Node (left, key, right) ->
    let first_member = member key t1 in
    let second_member = member key t2 in
    if first_member
    then
      if second_member
      then if agrees t1 t2 left then agrees t1 t2 right else false
      else false
    else if second_member
    then false
    else if agrees t1 t2 left then agrees t1 t2 right else false

let[@vox.def] equal (t1 : t @ logical) (t2 : t @ logical) =
  if agrees t1 t2 t1 then agrees t1 t2 t2 else false

let agrees_node ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(left : t @ logical) ~(key : int) ~(right : t @ logical)
    ~proof:(_proof : unit{
       agrees t1 t2 (Node (left, key, right)) = true
     })
    : unit{
      member key t1 = member key t2
      && agrees t1 t2 left = true
      && agrees t1 t2 right = true
    } =
  agrees_def t1 t2 (Node (left, key, right));
  ()

let rec agrees_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(nodes : t @ logical) ~(query : int)
    ~(agreement : unit{ agrees t1 t2 nodes = true })
    ~(present : unit{ member query nodes = true })
    : unit{ member query t1 = member query t2 } =
  match nodes with
  | Leaf ->
    member_def query Leaf;
    ()
  | Node (left, key, right) ->
    let facts =
      agrees_node ~t1 ~t2 ~left ~key ~right ~proof:agreement
    in
    member_def query (Node (left, key, right));
    let choice = direction query key in
    direction_def query key;
    match choice with
    | Same -> facts
    | Left ->
      agrees_member ~t1 ~t2 ~nodes:left ~query
        ~agreement:facts ~present:()
    | Right ->
      agrees_member ~t1 ~t2 ~nodes:right ~query
        ~agreement:facts ~present:()

let prove_equal_member ~(t1 : t @ logical)
    ~(t2 : t{ equal t1 _ = true } @ logical)
    ~(query : int)
    : unit{ member query t1 = member query t2 } =
  equal_def t1 t2;
  let first_member = member query t1 in
  let second_member = member query t2 in
  let side = membership_side first_member second_member in
  membership_side_def first_member second_member;
  match side with
  | First ->
    agrees_member ~t1 ~t2 ~nodes:t1 ~query
      ~agreement:() ~present:()
  | Second ->
    agrees_member ~t1 ~t2 ~nodes:t2 ~query
      ~agreement:() ~present:()
  | Neither -> ()

let equal_forward_law ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(equal_trees : unit{ equal t1 t2 = true }) ~(query : int)
    : unit{ member query t1 = member query t2 } =
  prove_equal_member ~t1 ~t2 ~query

let equal_backward_law ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(pointwise : query:int ->
                   unit{ member query t1 = member query t2 })
    : unit{ equal t1 t2 = true } =
  let rec prove nodes : unit{ agrees t1 t2 nodes = true } =
    match nodes with
    | Leaf ->
      agrees_def t1 t2 Leaf;
      ()
    | Node (left, key, right) ->
      pointwise ~query:key;
      prove left;
      prove right;
      agrees_def t1 t2 (Node (left, key, right));
      ()
  in
  prove t1;
  prove t2;
  equal_def t1 t2;
  ()

(* ------------------------------------------------------------------ *)
(* Size and a unary height, and the two bounds that pin the height.     *)
(* ------------------------------------------------------------------ *)

let[@vox.def] rec size (tree : t @ logical) : Bigint.t =
  match tree with
  | Leaf -> Bigint.zero
  | Node (l, _, r) -> Bigint.add Bigint.one (Bigint.add (size l) (size r))

let rec size_nonneg (tree : t @ logical)
    : unit{ Bigint.le Bigint.zero (size tree) = true } =
  match tree with
  | Leaf -> size_def Leaf; ()
  | Node (l, k, r) ->
    size_def (Node (l, k, r)); size_nonneg l; size_nonneg r; ()

(* Rotations move nodes about; they do not create or destroy any. *)
let rotate_right_size (tree : t @ logical)
    : unit{ size (rotate_right tree) = size tree } =
  rotate_right_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, y, c) ->
    (match l with
     | Leaf -> ()
     | Node (a, x, b) ->
       size_def (Node (l, y, c));
       size_def (Node (a, x, b));
       size_def (Node (a, x, Node (b, y, c)));
       size_def (Node (b, y, c));
       ())

let rotate_left_size (tree : t @ logical)
    : unit{ size (rotate_left tree) = size tree } =
  rotate_left_def tree;
  match tree with
  | Leaf -> ()
  | Node (a, x, r) ->
    (match r with
     | Leaf -> ()
     | Node (b, y, c) ->
       size_def (Node (a, x, r));
       size_def (Node (b, y, c));
       size_def (Node (Node (a, x, b), y, c));
       size_def (Node (a, x, b));
       ())

let rotate_left_right_size (tree : t @ logical)
    : unit{ size (rotate_left_right tree) = size tree } =
  rotate_left_right_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    rotate_right_size (Node (rotate_left l, k, r));
    rotate_left_size l;
    size_def (Node (rotate_left l, k, r));
    size_def (Node (l, k, r));
    ()

let rotate_right_left_size (tree : t @ logical)
    : unit{ size (rotate_right_left tree) = size tree } =
  rotate_right_left_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    rotate_left_size (Node (l, k, rotate_right r));
    rotate_right_size r;
    size_def (Node (l, k, rotate_right r));
    size_def (Node (l, k, r));
    ()

let rebalance_size (tree : t @ logical)
    : unit{ size (rebalance tree) = size tree } =
  rebalance_def tree;
  match tree with
  | Leaf -> ()
  | Node (l, _k, r) ->
    if Bigint.lt (Bigint.add (height r) Bigint.one) (height l)
    then
      (match l with
       | Leaf -> ()
       | Node (ll, _lk, lr) ->
         if Bigint.le (height lr) (height ll)
         then rotate_right_size tree
         else rotate_left_right_size tree)
    else if Bigint.lt (Bigint.add (height l) Bigint.one) (height r)
    then
      (match r with
       | Leaf -> ()
       | Node (rl, _rk, rr) ->
         if Bigint.le (height rl) (height rr)
         then rotate_left_size tree
         else rotate_right_left_size tree)
    else ()

(* [insert] adds exactly one node, and none when the key is already
   found.  This is what refuses a duplicate-pushing insert. *)
let rec size_insert_step (new_key : int) (tree : t @ logical)
    : unit{
      size (insert new_key tree)
      = (if member new_key tree
         then size tree
         else Bigint.add (size tree) Bigint.one)
    } =
  match tree with
  | Leaf ->
    insert_def new_key Leaf;
    member_def new_key Leaf;
    size_def Leaf;
    size_def (Node (Leaf, new_key, Leaf));
    ()
  | Node (l, key, r) ->
    insert_def new_key (Node (l, key, r));
    member_def new_key (Node (l, key, r));
    size_def (Node (l, key, r));
    let choice = direction new_key key in
    direction_def new_key key;
    (match choice with
     | Same -> ()
     | Left ->
       size_insert_step new_key l;
       rebalance_size (Node (insert new_key l, key, r));
       size_def (Node (insert new_key l, key, r));
       ()
     | Right ->
       size_insert_step new_key r;
       rebalance_size (Node (l, key, insert new_key r));
       size_def (Node (l, key, insert new_key r));
       ())

(* A unary copy of the height, so that the two bound functions can be
   structurally recursive.  The [Bigint] height and its balance proof are
   untouched; [height_bridge] connects them. *)
let[@vox.def] rec depth (tree : t @ logical) : Bal_intf.nat =
  match tree with
  | Leaf -> Bal_intf.Z
  | Node (l, _, r) -> Bal_intf.S (Bal_intf.nmax (depth l) (depth r))

let nat_to_big_nmax (a : Bal_intf.nat @ logical)
    (b : Bal_intf.nat @ logical)
    : unit{
      Bal_intf.nat_to_big (Bal_intf.nmax a b)
      = max_height (Bal_intf.nat_to_big a) (Bal_intf.nat_to_big b)
    } =
  max_height_def (Bal_intf.nat_to_big a) (Bal_intf.nat_to_big b);
  Bal_intf.nle_total a b;
  Bal_intf.nle_iff a b;
  Bal_intf.nle_iff b a;
  let leaning = Bal_intf.nle a b in
  (match leaning with
   | true -> Bal_intf.nmax_right a b ()
   | false -> Bal_intf.nmax_left a b ())

let rec height_bridge (tree : t @ logical)
    : unit{ Bal_intf.nat_to_big (depth tree) = height tree } =
  match tree with
  | Leaf -> depth_def Leaf; height_def Leaf; Bal_intf.nat_to_big_def Bal_intf.Z; ()
  | Node (l, k, r) ->
    depth_def (Node (l, k, r));
    height_def (Node (l, k, r));
    Bal_intf.nat_to_big_def
      (Bal_intf.S (Bal_intf.nmax (depth l) (depth r)));
    nat_to_big_nmax (depth l) (depth r);
    height_bridge l;
    height_bridge r;
    ()

(* Height is not understated: fewer than 2^(h+1) keys at height h.  True
   of any binary tree, balanced or not. *)
let rec pow2_bound (tree : t @ logical)
    : unit{
      Bigint.lt (size tree) (Bal_intf.pow2 (Bal_intf.S (depth tree)))
      = true
    } =
  match tree with
  | Leaf ->
    size_def Leaf;
    depth_def Leaf;
    Bal_intf.pow2_def (Bal_intf.S Bal_intf.Z);
    Bal_intf.pow2_def Bal_intf.Z;
    ()
  | Node (l, k, r) ->
    size_def (Node (l, k, r));
    depth_def (Node (l, k, r));
    Bal_intf.pow2_def
      (Bal_intf.S (Bal_intf.S (Bal_intf.nmax (depth l) (depth r))));
    pow2_bound l;
    pow2_bound r;
    Bal_intf.nle_nmax_left (depth l) (depth r);
    Bal_intf.nle_nmax_right (depth l) (depth r);
    Bal_intf.nle_def (Bal_intf.S (depth l))
      (Bal_intf.S (Bal_intf.nmax (depth l) (depth r)));
    Bal_intf.nle_def (Bal_intf.S (depth r))
      (Bal_intf.S (Bal_intf.nmax (depth l) (depth r)));
    Bal_intf.pow2_mono (Bal_intf.S (depth l))
      (Bal_intf.S (Bal_intf.nmax (depth l) (depth r))) ();
    Bal_intf.pow2_mono (Bal_intf.S (depth r))
      (Bal_intf.S (Bal_intf.nmax (depth l) (depth r))) ();
    ()

(* Height is not overstated: at least [fib h] keys at height h.  This is
   the AVL minimum-size theorem, and it is false without [balanced]. *)
let rec min_size (tree : t @ logical)
    (_balanced : unit{ balanced tree = true })
    : unit{
      Bigint.le (Bal_intf.fib (depth tree)) (size tree) = true
    } =
  match tree with
  | Leaf ->
    balanced_def Leaf;
    depth_def Leaf;
    size_def Leaf;
    Bal_intf.fib_def Bal_intf.Z;
    ()
  | Node (l, k, r) ->
    balanced_def (Node (l, k, r));
    depth_def (Node (l, k, r));
    size_def (Node (l, k, r));
    min_size l ();
    min_size r ();
    size_nonneg l;
    size_nonneg r;
    (* carry the [Bigint] balance condition across to the unary side *)
    height_bridge l;
    height_bridge r;
    Bal_intf.nat_to_big_def (Bal_intf.S (depth l));
    Bal_intf.nat_to_big_def (Bal_intf.S (depth r));
    Bal_intf.nle_iff (depth l) (Bal_intf.S (depth r));
    Bal_intf.nle_iff (depth r) (Bal_intf.S (depth l));
    Bal_intf.nle_total (depth l) (depth r);
    let leaning = Bal_intf.nle (depth l) (depth r) in
    (match leaning with
     | true ->
       Bal_intf.nmax_right (depth l) (depth r) ();
       (match depth r with
        | Bal_intf.Z -> Bal_intf.fib_def (Bal_intf.S Bal_intf.Z); ()
        | Bal_intf.S qq ->
          Bal_intf.fib_def (Bal_intf.S (Bal_intf.S qq));
          Bal_intf.nle_def (Bal_intf.S qq) (Bal_intf.S (depth l));
          Bal_intf.fib_mono qq (depth l) ();
          ())
     | false ->
       Bal_intf.nmax_left (depth l) (depth r) ();
       (match depth l with
        | Bal_intf.Z -> Bal_intf.fib_def (Bal_intf.S Bal_intf.Z); ()
        | Bal_intf.S pp ->
          Bal_intf.fib_def (Bal_intf.S (Bal_intf.S pp));
          Bal_intf.nle_def (Bal_intf.S pp) (Bal_intf.S (depth r));
          Bal_intf.fib_mono pp (depth r) ();
          ()))

(* ------------------------------------------------------------------ *)
(* The [BALANCED_SET] laws.                                            *)
(* ------------------------------------------------------------------ *)

let size_empty : unit{ size empty = Bigint.zero } =
  size_def Leaf;
  ()

let size_insert ~(inserted : int) ~(tree : t @ logical)
    ~(well_formed : unit{ invariant tree = true })
    : unit{
      size (insert inserted tree)
      = (if member inserted tree
         then size tree
         else Bigint.add (size tree) Bigint.one)
    } =
  size_insert_step inserted tree

let size_depth_bound ~(tree : t @ logical)
    ~(well_formed : unit{ invariant tree = true })
    : unit{
      Bigint.lt (size tree) (Bal_intf.pow2 (Bal_intf.S (depth tree)))
      = true
    } =
  pow2_bound tree

let depth_size_bound ~(tree : t @ logical)
    ~(well_formed : unit{ invariant tree = true })
    : unit{
      Bigint.le (Bal_intf.fib (depth tree)) (size tree) = true
    } =
  invariant_def tree;
  min_size tree ()
