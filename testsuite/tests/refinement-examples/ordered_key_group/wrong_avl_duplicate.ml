external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"

module Make (K : Key_intf.ORDERED_KEY) = struct
  type key = K.t

  type t =
    | Leaf
    | Node of t * key * t

  let key_witness : key = K.witness

  type direction =
    | Same
    | Left
    | Right

  type membership_side =
    | First
    | Second
    | Neither

  let[@vox.def] direction (probe : key @ logical) (pivot : key @ logical) =
    if int_equal (K.compare probe pivot) 0
    then Same
    else if int_less (K.compare probe pivot) 0
    then Left
    else Right

  let[@vox.def] membership_side first_member second_member =
    if first_member
    then First
    else if second_member then Second else Neither

  let empty = Leaf

  let[@vox.def] rec member (query : key @ logical) (tree : t @ logical) =
    match tree with
    | Leaf -> false
    | Node (left, pivot, right) ->
      if int_equal (K.compare query pivot) 0
      then true
      else if int_less (K.compare query pivot) 0
      then member query left
      else member query right

  let[@vox.def] rec occurs (query : key @ logical) (tree : t @ logical) =
    match tree with
    | Leaf -> false
    | Node (left, pivot, right) ->
      int_equal (K.compare query pivot) 0
      || occurs query left
      || occurs query right

  let[@vox.def] rec below (tree : t @ logical) (bound : key @ logical) =
    match tree with
    | Leaf -> true
    | Node (left, pivot, right) ->
      int_less (K.compare pivot bound) 0
      && below left bound && below right bound

  let[@vox.def] rec above (tree : t @ logical) (bound : key @ logical) =
    match tree with
    | Leaf -> true
    | Node (left, pivot, right) ->
      int_less (K.compare bound pivot) 0
      && above left bound && above right bound

  let[@vox.def] rec ordered (tree : t @ logical) =
    match tree with
    | Leaf -> true
    | Node (left, pivot, right) ->
      ordered left && ordered right
      && below left pivot && above right pivot

  (* Widening a bound needs transitivity at every node.  For an [int] key
     the solver does this on its own. *)
  let rec below_weaken (hi : key @ logical)
      (lo : key{ K.compare _ hi < 0 } @ logical) (tree : t @ logical)
      (_bounded : unit{ below tree lo = true })
      : unit{ below tree hi = true } =
    match tree with
    | Leaf ->
      below_def Leaf hi;
      ()
    | Node (left, pivot, right) ->
      below_def (Node (left, pivot, right)) lo;
      below_def (Node (left, pivot, right)) hi;
      K.compare_negative_transitive ~first:pivot ~second:lo ~third:hi;
      below_weaken hi lo left ();
      below_weaken hi lo right ();
      ()

  let rec above_weaken (lo : key @ logical)
      (hi : key{ K.compare lo _ < 0 } @ logical) (tree : t @ logical)
      (_bounded : unit{ above tree hi = true })
      : unit{ above tree lo = true } =
    match tree with
    | Leaf ->
      above_def Leaf lo;
      ()
    | Node (left, pivot, right) ->
      above_def (Node (left, pivot, right)) hi;
      above_def (Node (left, pivot, right)) lo;
      K.compare_negative_transitive ~first:lo ~second:hi ~third:pivot;
      above_weaken lo hi left ();
      above_weaken lo hi right ();
      ()

  let rec below_absent (bound : key @ logical)
      (query : key{ K.compare bound _ < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ below tree bound = true })
      : unit{ occurs query tree = false } =
    match tree with
    | Leaf ->
      occurs_def query Leaf;
      ()
    | Node (left, pivot, right) ->
      below_def (Node (left, pivot, right)) bound;
      occurs_def query (Node (left, pivot, right));
      K.compare_negative_transitive ~first:pivot ~second:bound ~third:query;
      K.compare_sign_reversal ~left:pivot ~right:query;
      below_absent bound query left ();
      below_absent bound query right ();
      ()

  let rec above_absent (bound : key @ logical)
      (query : key{ K.compare _ bound < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ above tree bound = true })
      : unit{ occurs query tree = false } =
    match tree with
    | Leaf ->
      occurs_def query Leaf;
      ()
    | Node (left, pivot, right) ->
      above_def (Node (left, pivot, right)) bound;
      occurs_def query (Node (left, pivot, right));
      K.compare_negative_transitive ~first:query ~second:bound ~third:pivot;
      above_absent bound query left ();
      above_absent bound query right ();
      ()

  let rec member_occurs (query : key @ logical) (tree : t @ logical)
      (_ordered : unit{ ordered tree = true })
      : unit{ member query tree = occurs query tree } =
    match tree with
    | Leaf ->
      member_def query Leaf;
      occurs_def query Leaf;
      ()
    | Node (left, pivot, right) ->
      ordered_def (Node (left, pivot, right));
      member_def query (Node (left, pivot, right));
      occurs_def query (Node (left, pivot, right));
      member_occurs query left ();
      member_occurs query right ();
      let choice = direction query pivot in
      direction_def query pivot;
      match choice with
      | Same -> ()
      | Left -> above_absent pivot query right ()
      | Right ->
        K.compare_sign_reversal ~left:pivot ~right:query;
        below_absent pivot query left ()

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

  let rotate_right_preserves_occurs (tree : t @ logical)
      (query : key @ logical)
      : unit{ occurs query (rotate_right tree) = occurs query tree } =
    rotate_right_def tree;
    match tree with
    | Leaf -> ()
    | Node (l, y, c) ->
      occurs_def query (Node (l, y, c));
      match l with
      | Leaf -> ()
      | Node (a, x, b) ->
        occurs_def query (Node (a, x, b));
        occurs_def query (Node (a, x, Node (b, y, c)));
        occurs_def query (Node (b, y, c));
        ()

  let rotate_left_preserves_occurs (tree : t @ logical)
      (query : key @ logical)
      : unit{ occurs query (rotate_left tree) = occurs query tree } =
    rotate_left_def tree;
    match tree with
    | Leaf -> ()
    | Node (a, x, r) ->
      occurs_def query (Node (a, x, r));
      match r with
      | Leaf -> ()
      | Node (b, y, c) ->
        occurs_def query (Node (b, y, c));
        occurs_def query (Node (Node (a, x, b), y, c));
        occurs_def query (Node (a, x, b));
        ()

  let rotate_right_below (tree : t @ logical) (bound : key @ logical)
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

  let rotate_right_above (tree : t @ logical) (bound : key @ logical)
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

  let rotate_left_below (tree : t @ logical) (bound : key @ logical)
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

  let rotate_left_above (tree : t @ logical) (bound : key @ logical)
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
      (k : key @ logical) (r : t @ logical) (query : key @ logical)
      (_eq : unit{ occurs query l1 = occurs query l2 })
      : unit{
        occurs query (Node (l1, k, r)) = occurs query (Node (l2, k, r))
      } =
    occurs_def query (Node (l1, k, r));
    occurs_def query (Node (l2, k, r));
    ()

  let node_occurs_congruence_right (l : t @ logical) (k : key @ logical)
      (r1 : t @ logical) (r2 : t @ logical) (query : key @ logical)
      (_eq : unit{ occurs query r1 = occurs query r2 })
      : unit{
        occurs query (Node (l, k, r1)) = occurs query (Node (l, k, r2))
      } =
    occurs_def query (Node (l, k, r1));
    occurs_def query (Node (l, k, r2));
    ()

  let[@vox.def] rotate_left_right (tree : t @ logical) : t =
    match tree with
    | Leaf -> tree
    | Node (l, k, r) -> rotate_right (Node (rotate_left l, k, r))

  let[@vox.def] rotate_right_left (tree : t @ logical) : t =
    match tree with
    | Leaf -> tree
    | Node (l, k, r) -> rotate_left (Node (l, k, rotate_right r))

  let rotate_left_right_preserves_occurs (tree : t @ logical)
      (query : key @ logical)
      : unit{ occurs query (rotate_left_right tree) = occurs query tree } =
    rotate_left_right_def tree;
    match tree with
    | Leaf -> ()
    | Node (l, k, r) ->
      let rl = rotate_left_preserves_occurs l query in
      node_occurs_congruence_left (rotate_left l) l k r query rl;
      rotate_right_preserves_occurs (Node (rotate_left l, k, r)) query;
      ()

  let rotate_right_left_preserves_occurs (tree : t @ logical)
      (query : key @ logical)
      : unit{ occurs query (rotate_right_left tree) = occurs query tree } =
    rotate_right_left_def tree;
    match tree with
    | Leaf -> ()
    | Node (l, k, r) ->
      let rr = rotate_right_preserves_occurs r query in
      node_occurs_congruence_right l k (rotate_right r) r query rr;
      rotate_left_preserves_occurs (Node (l, k, rotate_right r)) query;
      ()

  let rotate_left_right_below (tree : t @ logical) (bound : key @ logical)
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

  let rotate_left_right_above (tree : t @ logical) (bound : key @ logical)
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

  let rotate_right_left_below (tree : t @ logical) (bound : key @ logical)
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

  let rotate_right_left_above (tree : t @ logical) (bound : key @ logical)
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

  (* Heights are mathematical integers: nothing bounds the size of a tree,
     so a machine-word height would need a no-wrap side condition that the
     interface cannot carry. *)
  let[@vox.def] max_height (a : Bigint.t @ logical)
      (b : Bigint.t @ logical) =
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

  let[@vox.def] rec balanced (tree : t @ logical) =
    match tree with
    | Leaf -> true
    | Node (l, _, r) ->
      balanced l && balanced r
      && Bigint.le (height l) (Bigint.add (height r) Bigint.one)
      && Bigint.le (height r) (Bigint.add (height l) Bigint.one)

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

  let rebalance_preserves_occurs (tree : t @ logical)
      (query : key @ logical)
      : unit{ occurs query (rebalance tree) = occurs query tree } =
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

  let rebalance_below (tree : t @ logical) (bound : key @ logical)
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

  let rebalance_above (tree : t @ logical) (bound : key @ logical)
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

  let rebalance_ok (l : t @ logical) (key : key @ logical)
      (r : t @ logical)
      (_hypotheses : unit{
         balanced l = true
         && balanced r = true
         && Bigint.le (height l)
              (Bigint.add (height r) (Bigint.add Bigint.one Bigint.one))
            = true
         && Bigint.le (height r)
              (Bigint.add (height l) (Bigint.add Bigint.one Bigint.one))
            = true
       })
      : unit{
        balanced (rebalance (Node (l, key, r))) = true
        && Bigint.le (height (rebalance (Node (l, key, r))))
             (height (Node (l, key, r))) = true
        && Bigint.le (Bigint.sub (height (Node (l, key, r))) Bigint.one)
             (height (rebalance (Node (l, key, r)))) = true
        && (Bigint.le (height l) (Bigint.add (height r) Bigint.one) = false
            || Bigint.le (height r) (Bigint.add (height l) Bigint.one)
               = false
            || Bigint.equal (height (rebalance (Node (l, key, r))))
                 (height (Node (l, key, r))) = true)
      } =
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

  let[@vox.def] rec insert (fresh : key @ logical) (tree : t @ logical) : t =
    match tree with
    | Leaf -> Node (Leaf, fresh, Leaf)
    | Node (left, pivot, right) ->
      if int_equal (K.compare fresh pivot) 0
      then rebalance (Node (insert fresh left, pivot, right))
      else if int_less (K.compare fresh pivot) 0
      then rebalance (Node (insert fresh left, pivot, right))
      else rebalance (Node (left, pivot, insert fresh right))

  let rec insert_below (bound : key @ logical)
      (fresh : key{ K.compare _ bound < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ below tree bound = true })
      : unit{ below (insert fresh tree) bound = true } =
    match tree with
    | Leaf ->
      insert_def fresh Leaf;
      below_def (Node (Leaf, fresh, Leaf)) bound;
      below_def Leaf bound;
      ()
    | Node (left, pivot, right) ->
      insert_def fresh (Node (left, pivot, right));
      below_def (Node (left, pivot, right)) bound;
      let choice = direction fresh pivot in
      direction_def fresh pivot;
      match choice with
      | Same -> ()
      | Left ->
        insert_below bound fresh left ();
        rebalance_below (Node (insert fresh left, pivot, right)) bound;
        below_def (Node (insert fresh left, pivot, right)) bound;
        ()
      | Right ->
        insert_below bound fresh right ();
        rebalance_below (Node (left, pivot, insert fresh right)) bound;
        below_def (Node (left, pivot, insert fresh right)) bound;
        ()

  let rec insert_above (bound : key @ logical)
      (fresh : key{ K.compare bound _ < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ above tree bound = true })
      : unit{ above (insert fresh tree) bound = true } =
    match tree with
    | Leaf ->
      insert_def fresh Leaf;
      above_def (Node (Leaf, fresh, Leaf)) bound;
      above_def Leaf bound;
      ()
    | Node (left, pivot, right) ->
      insert_def fresh (Node (left, pivot, right));
      above_def (Node (left, pivot, right)) bound;
      let choice = direction fresh pivot in
      direction_def fresh pivot;
      match choice with
      | Same -> ()
      | Left ->
        insert_above bound fresh left ();
        rebalance_above (Node (insert fresh left, pivot, right)) bound;
        above_def (Node (insert fresh left, pivot, right)) bound;
        ()
      | Right ->
        insert_above bound fresh right ();
        rebalance_above (Node (left, pivot, insert fresh right)) bound;
        above_def (Node (left, pivot, insert fresh right)) bound;
        ()

  let rec insert_ordered (fresh : key @ logical) (tree : t @ logical)
      (_ordered : unit{ ordered tree = true })
      : unit{ ordered (insert fresh tree) = true } =
    match tree with
    | Leaf ->
      insert_def fresh Leaf;
      ordered_def (Node (Leaf, fresh, Leaf));
      ordered_def Leaf;
      below_def Leaf fresh;
      above_def Leaf fresh;
      ()
    | Node (left, pivot, right) ->
      insert_def fresh (Node (left, pivot, right));
      ordered_def (Node (left, pivot, right));
      let choice = direction fresh pivot in
      direction_def fresh pivot;
      match choice with
      | Same -> ()
      | Left ->
        insert_ordered fresh left ();
        insert_below pivot fresh left ();
        ordered_def (Node (insert fresh left, pivot, right));
        rebalance_ordered (Node (insert fresh left, pivot, right)) ();
        ()
      | Right ->
        K.compare_sign_reversal ~left:pivot ~right:fresh;
        insert_ordered fresh right ();
        insert_above pivot fresh right ();
        ordered_def (Node (left, pivot, insert fresh right));
        rebalance_ordered (Node (left, pivot, insert fresh right)) ();
        ()

  let rec insert_balanced (fresh : key @ logical) (tree : t @ logical)
      (_balanced : unit{ balanced tree = true })
      : unit{
        balanced (insert fresh tree) = true
        && Bigint.le (height tree) (height (insert fresh tree)) = true
        && Bigint.le (height (insert fresh tree))
             (Bigint.add (height tree) Bigint.one) = true
      } =
    match tree with
    | Leaf ->
      insert_def fresh Leaf;
      balanced_def (Node (Leaf, fresh, Leaf));
      balanced_def Leaf;
      height_def (Node (Leaf, fresh, Leaf));
      height_def Leaf;
      max_height_def (height Leaf) (height Leaf);
      ()
    | Node (left, pivot, right) ->
      insert_def fresh (Node (left, pivot, right));
      balanced_def (Node (left, pivot, right));
      height_def (Node (left, pivot, right));
      max_height_def (height left) (height right);
      let choice = direction fresh pivot in
      direction_def fresh pivot;
      match choice with
      | Same -> ()
      | Left ->
        insert_balanced fresh left ();
        rebalance_ok (insert fresh left) pivot right ();
        height_def (Node (insert fresh left, pivot, right));
        max_height_def (height (insert fresh left)) (height right);
        ()
      | Right ->
        insert_balanced fresh right ();
        rebalance_ok left pivot (insert fresh right) ();
        height_def (Node (left, pivot, insert fresh right));
        max_height_def (height left) (height (insert fresh right));
        ()

  let occurs_insert_leaf (fresh : key @ logical) (query : key @ logical)
      : unit{
        occurs query (insert fresh Leaf)
        = (K.compare query fresh = 0 || occurs query Leaf)
      } =
    insert_def fresh Leaf;
    occurs_def query (Node (Leaf, fresh, Leaf));
    occurs_def query Leaf;
    ()

  let occurs_insert_same (pivot : key @ logical)
      (fresh : key{ K.compare _ pivot = 0 } @ logical)
      (left : t @ logical) (right : t @ logical) (query : key @ logical)
      : unit{
        occurs query (insert fresh (Node (left, pivot, right)))
        = (K.compare query fresh = 0
           || occurs query (Node (left, pivot, right)))
      } =
    insert_def fresh (Node (left, pivot, right));
    occurs_def query (Node (left, pivot, right));
    K.compare_zero_iff_equal ~left:fresh ~right:pivot;
    ()

  let occurs_insert_left (pivot : key @ logical)
      (fresh : key{ K.compare _ pivot < 0 } @ logical)
      (left : t @ logical) (right : t @ logical) (query : key @ logical)
      (_ih : unit{
         occurs query (insert fresh left)
         = (K.compare query fresh = 0 || occurs query left)
       })
      : unit{
        occurs query (insert fresh (Node (left, pivot, right)))
        = (K.compare query fresh = 0
           || occurs query (Node (left, pivot, right)))
      } =
    insert_def fresh (Node (left, pivot, right));
    rebalance_preserves_occurs (Node (insert fresh left, pivot, right)) query;
    occurs_def query (Node (insert fresh left, pivot, right));
    occurs_def query (Node (left, pivot, right));
    ()

  let occurs_insert_right (pivot : key @ logical)
      (fresh : key{ K.compare _ pivot <> 0 && not (K.compare _ pivot < 0) }
                 @ logical)
      (left : t @ logical) (right : t @ logical) (query : key @ logical)
      (_ih : unit{
         occurs query (insert fresh right)
         = (K.compare query fresh = 0 || occurs query right)
       })
      : unit{
        occurs query (insert fresh (Node (left, pivot, right)))
        = (K.compare query fresh = 0
           || occurs query (Node (left, pivot, right)))
      } =
    insert_def fresh (Node (left, pivot, right));
    rebalance_preserves_occurs (Node (left, pivot, insert fresh right)) query;
    occurs_def query (Node (left, pivot, insert fresh right));
    occurs_def query (Node (left, pivot, right));
    ()

  let rec occurs_insert (fresh : key @ logical) (tree : t @ logical)
      (query : key @ logical)
      : unit{
        occurs query (insert fresh tree)
        = (K.compare query fresh = 0 || occurs query tree)
      } =
    match tree with
    | Leaf -> occurs_insert_leaf fresh query
    | Node (left, pivot, right) ->
      let choice = direction fresh pivot in
      direction_def fresh pivot;
      match choice with
      | Same -> occurs_insert_same pivot fresh left right query
      | Left ->
        let ih = occurs_insert fresh left query in
        occurs_insert_left pivot fresh left right query ih
      | Right ->
        let ih = occurs_insert fresh right query in
        occurs_insert_right pivot fresh left right query ih

  (* An AVL tree: ordered, and balanced in height. *)
  let[@vox.def] invariant (tree : t @ logical) =
    ordered tree && balanced tree

  let empty_law ~(query : key @ logical)
      : unit{ member query empty = false } =
    member_def query empty;
    ()

  let empty_invariant : unit{ invariant empty = true } =
    invariant_def empty;
    ordered_def empty;
    balanced_def empty;
    ()

  let insert_invariant ~(inserted : key @ logical) ~(set : t @ logical)
      ~(well_formed : unit{ invariant set = true })
      : unit{ invariant (insert inserted set) = true } =
    invariant_def set;
    invariant_def (insert inserted set);
    insert_ordered inserted set ();
    insert_balanced inserted set ();
    ()

  let insert_law ~(inserted : key @ logical) ~(set : t @ logical)
      ~(query : key @ logical)
      ~(well_formed : unit{ invariant set = true })
      : unit{
        member query (insert inserted set)
        = ((query = inserted) || member query set)
      } =
    invariant_def set;
    insert_ordered inserted set ();
    member_occurs query set ();
    member_occurs query (insert inserted set) ();
    occurs_insert inserted set query;
    K.compare_zero_iff_equal ~left:query ~right:inserted;
    ()

  let[@vox.def] rec agrees (left : t @ logical) (right : t @ logical)
      (nodes : t @ logical) =
    match nodes with
    | Leaf -> true
    | Node (subleft, pivot, subright) ->
      let first_member = member pivot left in
      let second_member = member pivot right in
      if first_member
      then
        if second_member
        then
          if agrees left right subleft
          then agrees left right subright
          else false
        else false
      else if second_member
      then false
      else if agrees left right subleft
      then agrees left right subright
      else false

  let[@vox.def] equal (left : t @ logical) (right : t @ logical) =
    if agrees left right left then agrees left right right else false

  let agrees_node ~(left : t @ logical) ~(right : t @ logical)
      ~(subleft : t @ logical) ~(pivot : key @ logical)
      ~(subright : t @ logical)
      ~proof:(_proof : unit{
         agrees left right (Node (subleft, pivot, subright)) = true
       })
      : unit{
        member pivot left = member pivot right
        && agrees left right subleft = true
        && agrees left right subright = true
      } =
    agrees_def left right (Node (subleft, pivot, subright));
    ()

  let rec agrees_member ~(left : t @ logical) ~(right : t @ logical)
      ~(nodes : t @ logical) ~(query : key @ logical)
      ~(agreement : unit{ agrees left right nodes = true })
      ~(present : unit{ member query nodes = true })
      : unit{ member query left = member query right } =
    match nodes with
    | Leaf ->
      member_def query Leaf;
      ()
    | Node (subleft, pivot, subright) ->
      let facts =
        agrees_node ~left ~right ~subleft ~pivot ~subright ~proof:agreement
      in
      member_def query (Node (subleft, pivot, subright));
      let choice = direction query pivot in
      direction_def query pivot;
      K.compare_zero_iff_equal ~left:query ~right:pivot;
      match choice with
      | Same -> facts
      | Left ->
        agrees_member ~left ~right ~nodes:subleft ~query
          ~agreement:facts ~present:()
      | Right ->
        agrees_member ~left ~right ~nodes:subright ~query
          ~agreement:facts ~present:()

  let prove_equal_member ~(left : t @ logical)
      ~(right : t{ equal left _ = true } @ logical)
      ~(query : key @ logical)
      : unit{ member query left = member query right } =
    equal_def left right;
    let first_member = member query left in
    let second_member = member query right in
    let side = membership_side first_member second_member in
    membership_side_def first_member second_member;
    match side with
    | First ->
      agrees_member ~left ~right ~nodes:left ~query
        ~agreement:() ~present:()
    | Second ->
      agrees_member ~left ~right ~nodes:right ~query
        ~agreement:() ~present:()
    | Neither -> ()

  let equal_forward_law ~(left : t @ logical) ~(right : t @ logical)
      ~(equal_sets : unit{ equal left right = true })
      ~(query : key @ logical)
      : unit{ member query left = member query right } =
    K.compare_zero_iff_equal ~left:K.witness ~right:K.witness;
    prove_equal_member ~left ~right ~query

  let equal_backward_law ~(left : t @ logical) ~(right : t @ logical)
      ~(pointwise :
          query:key @ logical ->
          unit{ member query left = member query right })
      : unit{ equal left right = true } =
    K.compare_zero_iff_equal ~left:K.witness ~right:K.witness;
    let rec prove nodes : unit{ agrees left right nodes = true } =
      match nodes with
      | Leaf ->
        agrees_def left right Leaf;
        ()
      | Node (subleft, pivot, subright) ->
        pointwise ~query:pivot;
        prove subleft;
        prove subright;
        agrees_def left right (Node (subleft, pivot, subright));
        ()
    in
    prove left;
    prove right;
    equal_def left right;
    ()
end
