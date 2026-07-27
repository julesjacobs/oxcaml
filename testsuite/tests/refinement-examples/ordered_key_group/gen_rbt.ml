external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"

module Make (K : Key_intf.ORDERED_KEY) = struct
  type key = K.t

  let key_witness : key = K.witness

  type color =
    | Red
    | Black

  type t =
    | Empty
    | Node of color * t * key * t


  type direction =
    | Same
    | Left
    | Right

  let[@vox.def] direction (probe : key @ logical) (pivot : key @ logical) =
      if int_equal (K.compare probe pivot) 0
      then Same
      else if int_less (K.compare probe pivot) 0
      then Left
      else Right

  (* Search membership: one comparison per level, descending a single spine.
     Correct only on ordered trees, which is what [invariant] records. *)
  let[@vox.def] rec member (query : key @ logical) (tree : t @ logical) =
    match tree with
    | Empty -> false
    | Node (_colour, left, pivot, right) ->
      if int_equal (K.compare query pivot) 0
      then true
      else if int_less (K.compare query pivot) 0
      then member query left
      else member query right

  (* Occurrence anywhere in the tree.  Okasaki's rotations are ordering-free
     rearrangements, so they are stated against this; [member_occurs] carries
     their conclusions back to the spine. *)
  let[@vox.def] rec occurs (query : key @ logical) (tree : t @ logical) =
    match tree with
    | Empty -> false
    | Node (_colour, left, pivot, right) ->
      int_equal (K.compare query pivot) 0
      || occurs query left
      || occurs query right

  let[@vox.def] rec below (tree : t @ logical) (bound : key @ logical) =
    match tree with
    | Empty -> true
    | Node (_colour, left, pivot, right) ->
      int_less (K.compare pivot bound) 0
      && below left bound && below right bound

  let[@vox.def] rec above (tree : t @ logical) (bound : key @ logical) =
    match tree with
    | Empty -> true
    | Node (_colour, left, pivot, right) ->
      int_less (K.compare bound pivot) 0
      && above left bound && above right bound

  let[@vox.def] rec ordered (tree : t @ logical) =
    match tree with
    | Empty -> true
    | Node (_colour, left, pivot, right) ->
      ordered left && ordered right && below left pivot && above right pivot

  let rec below_weaken (hi : key @ logical) (lo : key{ K.compare _ hi < 0 } @ logical) (tree : t @ logical)
      (_bounded : unit{ below tree lo = true })
      : unit{ below tree hi = true } =
    match tree with
    | Empty ->
      below_def Empty hi;
      ()
    | Node (colour, left, pivot, right) ->
      below_def (Node (colour, left, pivot, right)) lo;
      below_def (Node (colour, left, pivot, right)) hi;
      K.compare_negative_transitive ~first:pivot ~second:lo ~third:hi;
      below_weaken hi lo left ();
      below_weaken hi lo right ();
      ()

  let rec above_weaken (lo : key @ logical) (hi : key{ K.compare lo _ < 0 } @ logical) (tree : t @ logical)
      (_bounded : unit{ above tree hi = true })
      : unit{ above tree lo = true } =
    match tree with
    | Empty ->
      above_def Empty lo;
      ()
    | Node (colour, left, pivot, right) ->
      above_def (Node (colour, left, pivot, right)) hi;
      above_def (Node (colour, left, pivot, right)) lo;
      K.compare_negative_transitive ~first:lo ~second:hi ~third:pivot;
      above_weaken lo hi left ();
      above_weaken lo hi right ();
      ()

  let rec below_absent (bound : key @ logical) (query : key{ K.compare bound _ < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ below tree bound = true })
      : unit{ occurs query tree = false } =
    match tree with
    | Empty ->
      occurs_def query Empty;
      ()
    | Node (colour, left, pivot, right) ->
      below_def (Node (colour, left, pivot, right)) bound;
      occurs_def query (Node (colour, left, pivot, right));
      K.compare_negative_transitive ~first:pivot ~second:bound ~third:query;
      K.compare_sign_reversal ~left:pivot ~right:query;
      below_absent bound query left ();
      below_absent bound query right ();
      ()

  let rec above_absent (bound : key @ logical) (query : key{ K.compare _ bound < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ above tree bound = true })
      : unit{ occurs query tree = false } =
    match tree with
    | Empty ->
      occurs_def query Empty;
      ()
    | Node (colour, left, pivot, right) ->
      above_def (Node (colour, left, pivot, right)) bound;
      occurs_def query (Node (colour, left, pivot, right));
      K.compare_negative_transitive ~first:query ~second:bound ~third:pivot;
      above_absent bound query left ();
      above_absent bound query right ();
      ()

  (* On an ordered tree the single spine finds exactly the keys that occur. *)
  let rec member_occurs (query : key @ logical) (tree : t @ logical)
      (_ordered : unit{ ordered tree = true })
      : unit{ member query tree = occurs query tree } =
    match tree with
    | Empty ->
      member_def query Empty;
      occurs_def query Empty;
      ()
    | Node (colour, left, pivot, right) ->
      ordered_def (Node (colour, left, pivot, right));
      member_def query (Node (colour, left, pivot, right));
      occurs_def query (Node (colour, left, pivot, right));
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

  (* The two red-black conditions.  [no_red_red] is the colour condition; a
     tree produced by [ins] satisfies it everywhere except possibly at its own
     root, which is what [infrared] allows for. *)
  let[@vox.def] is_black (tree : t @ logical) =
    match tree with
    | Empty -> true
    | Node (colour, _left, _key, _right) ->
      match colour with
      | Red -> false
      | Black -> true

  let[@vox.def] rec no_red_red (tree : t @ logical) =
    match tree with
    | Empty -> true
    | Node (colour, left, _key, right) ->
      match colour with
      | Black -> no_red_red left && no_red_red right
      | Red ->
        is_black left && is_black right
        && no_red_red left && no_red_red right

  let[@vox.def] infrared (tree : t @ logical) =
    match tree with
    | Empty -> true
    | Node (_colour, left, _key, right) -> no_red_red left && no_red_red right

  (* Black heights are mathematical integers.  A machine [int] black height
     would need a bound on the size of a tree that nothing here supplies. *)
  let[@vox.def] rec black_height (tree : t @ logical) : Bigint.t =
    match tree with
    | Empty -> Bigint.zero
    | Node (colour, left, _key, _right) ->
      match colour with
      | Black -> Bigint.add Bigint.one (black_height left)
      | Red -> black_height left

  let[@vox.def] rec black_balanced (tree : t @ logical) =
    match tree with
    | Empty -> true
    | Node (_colour, left, _key, right) ->
      black_balanced left && black_balanced right
      && Bigint.equal (black_height left) (black_height right)

  let no_red_red_infrared (tree : t @ logical)
      (_proper : unit{ no_red_red tree = true })
      : unit{ infrared tree = true } =
    infrared_def tree;
    match tree with
    | Empty -> ()
    | Node (colour, left, pivot, right) ->
      no_red_red_def (Node (colour, left, pivot, right));
      match colour with
      | Red -> ()
      | Black -> ()

  (* Right-leaning half of Okasaki's balance: the caller has already ruled out a
     red-red violation on the left spine, so we only inspect the right child. *)
  let[@vox.def] rotate_right_side (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical) : t =
    match right with
    | Empty -> Node (Black, left, pivot, right)
    | Node (rc, rl, rk, rr) ->
      match rc with
      | Black -> Node (Black, left, pivot, right)
      | Red ->
        match rl with
        | Node (rlc, b, y, c) ->
          (match rlc with
           | Red ->
             Node (Red, Node (Black, left, pivot, b), y, Node (Black, c, rk, rr))
           | Black ->
             (match rr with
              | Node (rrc, c, z, d) ->
                (match rrc with
                 | Red ->
                   Node (Red, Node (Black, left, pivot, rl), rk,
                         Node (Black, c, z, d))
                 | Black -> Node (Black, left, pivot, right))
              | Empty -> Node (Black, left, pivot, right)))
        | Empty ->
          (match rr with
           | Node (rrc, c, z, d) ->
             (match rrc with
              | Red ->
                Node (Red, Node (Black, left, pivot, rl), rk,
                      Node (Black, c, z, d))
              | Black -> Node (Black, left, pivot, right))
           | Empty -> Node (Black, left, pivot, right))

  (* Okasaki rotation dispatcher.  Red roots pass through; a black root may
     repair a red-red shape on either spine. *)
  let[@vox.def] balance (colour : color) (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical) : t =
    match colour with
    | Red -> Node (Red, left, pivot, right)
    | Black ->
      match left with
      | Empty -> rotate_right_side left pivot right
      | Node (lc, ll, lk, lr) ->
        match lc with
        | Black -> rotate_right_side left pivot right
        | Red ->
          match ll with
          | Node (llc, a, x, b) ->
            (match llc with
             | Red ->
               Node (Red, Node (Black, a, x, b), lk,
                     Node (Black, lr, pivot, right))
             | Black ->
               (match lr with
                | Node (lrc, b, y, c) ->
                  (match lrc with
                   | Red ->
                     Node (Red, Node (Black, ll, lk, b), y,
                           Node (Black, c, pivot, right))
                   | Black -> rotate_right_side left pivot right)
                | Empty -> rotate_right_side left pivot right))
          | Empty ->
            (match lr with
             | Node (lrc, b, y, c) ->
               (match lrc with
                | Red ->
                  Node (Red, Node (Black, ll, lk, b), y,
                        Node (Black, c, pivot, right))
                | Black -> rotate_right_side left pivot right)
             | Empty -> rotate_right_side left pivot right)

  let rotate_right_side_preserves (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical) (query : key @ logical)
      : unit{
        occurs query (rotate_right_side left pivot right)
        = (int_equal (K.compare query pivot) 0
           || occurs query left || occurs query right)
      }
    =
    let _ = rotate_right_side_def left pivot right in
    match right with
    | Empty -> let _ = occurs_def query (Node (Black, left, pivot, right)) in ()
    | Node (rc, rl, rk, rr) ->
      match rc with
      | Black -> let _ = occurs_def query (Node (Black, left, pivot, right)) in ()
      | Red ->
        match rl with
        | Node (rlc, b, y, c) ->
          (match rlc with
           | Red ->
             let _ = occurs_def query
               (Node (Red, Node (Black, left, pivot, b), y,
                      Node (Black, c, rk, rr))) in
             let _ = occurs_def query (Node (Black, left, pivot, b)) in
             let _ = occurs_def query (Node (Black, c, rk, rr)) in
             let _ = occurs_def query (Node (Red, rl, rk, rr)) in
             let _ = occurs_def query (Node (Red, b, y, c)) in
             ()
           | Black ->
             (match rr with
              | Node (rrc, c, z, d) ->
                (match rrc with
                 | Red ->
                   let _ = occurs_def query
                     (Node (Red, Node (Black, left, pivot, rl), rk,
                            Node (Black, c, z, d))) in
                   let _ = occurs_def query (Node (Black, left, pivot, rl)) in
                   let _ = occurs_def query (Node (Black, c, z, d)) in
                   let _ = occurs_def query (Node (Red, rl, rk, rr)) in
                   let _ = occurs_def query (Node (Red, c, z, d)) in
                   ()
                 | Black ->
                   let _ =
                     occurs_def query (Node (Black, left, pivot, right))
                   in
                   ())
              | Empty ->
                let _ = occurs_def query (Node (Black, left, pivot, right)) in
                ()))
        | Empty ->
          (match rr with
           | Node (rrc, c, z, d) ->
             (match rrc with
              | Red ->
                let _ = occurs_def query
                  (Node (Red, Node (Black, left, pivot, rl), rk,
                         Node (Black, c, z, d))) in
                let _ = occurs_def query (Node (Black, left, pivot, rl)) in
                let _ = occurs_def query (Node (Black, c, z, d)) in
                let _ = occurs_def query (Node (Red, rl, rk, rr)) in
                let _ = occurs_def query (Node (Red, c, z, d)) in
                let _ = occurs_def query Empty in
                ()
              | Black ->
                let _ = occurs_def query (Node (Black, left, pivot, right)) in
                ())
           | Empty ->
             let _ = occurs_def query (Node (Black, left, pivot, right)) in
             ())

  let balance_preserves (colour : color) (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical) (query : key @ logical)
      : unit{
        occurs query (balance colour left pivot right)
        = (int_equal (K.compare query pivot) 0
           || occurs query left || occurs query right)
      }
    =
    let _ = balance_def colour left pivot right in
    match colour with
    | Red -> let _ = occurs_def query (Node (Red, left, pivot, right)) in ()
    | Black ->
      match left with
      | Empty -> rotate_right_side_preserves left pivot right query
      | Node (lc, ll, lk, lr) ->
        match lc with
        | Black -> rotate_right_side_preserves left pivot right query
        | Red ->
          match ll with
          | Node (llc, a, x, b) ->
            (match llc with
             | Red ->
               let _ = occurs_def query
                 (Node (Red, Node (Black, a, x, b), lk,
                        Node (Black, lr, pivot, right))) in
               let _ = occurs_def query (Node (Black, a, x, b)) in
               let _ = occurs_def query (Node (Black, lr, pivot, right)) in
               let _ = occurs_def query (Node (Red, ll, lk, lr)) in
               let _ = occurs_def query (Node (Red, a, x, b)) in
               ()
             | Black ->
               (match lr with
                | Node (lrc, b, y, c) ->
                  (match lrc with
                   | Red ->
                     let _ = occurs_def query
                       (Node (Red, Node (Black, ll, lk, b), y,
                              Node (Black, c, pivot, right))) in
                     let _ = occurs_def query (Node (Black, ll, lk, b)) in
                     let _ = occurs_def query (Node (Black, c, pivot, right)) in
                     let _ = occurs_def query (Node (Red, ll, lk, lr)) in
                     let _ = occurs_def query (Node (Red, b, y, c)) in
                     ()
                   | Black -> rotate_right_side_preserves left pivot right query)
                | Empty -> rotate_right_side_preserves left pivot right query))
          | Empty ->
            (match lr with
             | Node (lrc, b, y, c) ->
               (match lrc with
                | Red ->
                  let _ = occurs_def query
                    (Node (Red, Node (Black, ll, lk, b), y,
                           Node (Black, c, pivot, right))) in
                  let _ = occurs_def query (Node (Black, ll, lk, b)) in
                  let _ = occurs_def query (Node (Black, c, pivot, right)) in
                  let _ = occurs_def query (Node (Red, ll, lk, lr)) in
                  let _ = occurs_def query (Node (Red, b, y, c)) in
                  let _ = occurs_def query Empty in
                  ()
                | Black -> rotate_right_side_preserves left pivot right query)
             | Empty -> rotate_right_side_preserves left pivot right query)

  let rotate_right_side_below (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical) (bound : key @ logical)
      : unit{
        below (rotate_right_side left pivot right) bound
        = (int_less (K.compare pivot bound) 0
           && below left bound && below right bound)
      }
    =
    let _ = rotate_right_side_def left pivot right in
    match right with
    | Empty -> let _ = below_def (Node (Black, left, pivot, right)) bound in ()
    | Node (rc, rl, rk, rr) ->
      match rc with
      | Black -> let _ = below_def (Node (Black, left, pivot, right)) bound in ()
      | Red ->
        match rl with
        | Node (rlc, b, y, c) ->
          (match rlc with
           | Red ->
             let _ = below_def
               (Node (Red, Node (Black, left, pivot, b), y,
                      Node (Black, c, rk, rr))) bound in
             let _ = below_def (Node (Black, left, pivot, b)) bound in
             let _ = below_def (Node (Black, c, rk, rr)) bound in
             let _ = below_def (Node (Red, rl, rk, rr)) bound in
             let _ = below_def (Node (Red, b, y, c)) bound in
             ()
           | Black ->
             (match rr with
              | Node (rrc, c, z, d) ->
                (match rrc with
                 | Red ->
                   let _ = below_def
                     (Node (Red, Node (Black, left, pivot, rl), rk,
                            Node (Black, c, z, d))) bound in
                   let _ = below_def (Node (Black, left, pivot, rl)) bound in
                   let _ = below_def (Node (Black, c, z, d)) bound in
                   let _ = below_def (Node (Red, rl, rk, rr)) bound in
                   let _ = below_def (Node (Red, c, z, d)) bound in
                   ()
                 | Black ->
                   let _ = below_def (Node (Black, left, pivot, right)) bound in
                   ())
              | Empty ->
                let _ = below_def (Node (Black, left, pivot, right)) bound in
                ()))
        | Empty ->
          (match rr with
           | Node (rrc, c, z, d) ->
             (match rrc with
              | Red ->
                let _ = below_def
                  (Node (Red, Node (Black, left, pivot, rl), rk,
                         Node (Black, c, z, d))) bound in
                let _ = below_def (Node (Black, left, pivot, rl)) bound in
                let _ = below_def (Node (Black, c, z, d)) bound in
                let _ = below_def (Node (Red, rl, rk, rr)) bound in
                let _ = below_def (Node (Red, c, z, d)) bound in
                let _ = below_def Empty bound in
                ()
              | Black ->
                let _ = below_def (Node (Black, left, pivot, right)) bound in
                ())
           | Empty ->
             let _ = below_def (Node (Black, left, pivot, right)) bound in
             ())

  let rotate_right_side_above (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical) (bound : key @ logical)
      : unit{
        above (rotate_right_side left pivot right) bound
        = (int_less (K.compare bound pivot) 0
           && above left bound && above right bound)
      }
    =
    let _ = rotate_right_side_def left pivot right in
    match right with
    | Empty -> let _ = above_def (Node (Black, left, pivot, right)) bound in ()
    | Node (rc, rl, rk, rr) ->
      match rc with
      | Black -> let _ = above_def (Node (Black, left, pivot, right)) bound in ()
      | Red ->
        match rl with
        | Node (rlc, b, y, c) ->
          (match rlc with
           | Red ->
             let _ = above_def
               (Node (Red, Node (Black, left, pivot, b), y,
                      Node (Black, c, rk, rr))) bound in
             let _ = above_def (Node (Black, left, pivot, b)) bound in
             let _ = above_def (Node (Black, c, rk, rr)) bound in
             let _ = above_def (Node (Red, rl, rk, rr)) bound in
             let _ = above_def (Node (Red, b, y, c)) bound in
             ()
           | Black ->
             (match rr with
              | Node (rrc, c, z, d) ->
                (match rrc with
                 | Red ->
                   let _ = above_def
                     (Node (Red, Node (Black, left, pivot, rl), rk,
                            Node (Black, c, z, d))) bound in
                   let _ = above_def (Node (Black, left, pivot, rl)) bound in
                   let _ = above_def (Node (Black, c, z, d)) bound in
                   let _ = above_def (Node (Red, rl, rk, rr)) bound in
                   let _ = above_def (Node (Red, c, z, d)) bound in
                   ()
                 | Black ->
                   let _ = above_def (Node (Black, left, pivot, right)) bound in
                   ())
              | Empty ->
                let _ = above_def (Node (Black, left, pivot, right)) bound in
                ()))
        | Empty ->
          (match rr with
           | Node (rrc, c, z, d) ->
             (match rrc with
              | Red ->
                let _ = above_def
                  (Node (Red, Node (Black, left, pivot, rl), rk,
                         Node (Black, c, z, d))) bound in
                let _ = above_def (Node (Black, left, pivot, rl)) bound in
                let _ = above_def (Node (Black, c, z, d)) bound in
                let _ = above_def (Node (Red, rl, rk, rr)) bound in
                let _ = above_def (Node (Red, c, z, d)) bound in
                let _ = above_def Empty bound in
                ()
              | Black ->
                let _ = above_def (Node (Black, left, pivot, right)) bound in
                ())
           | Empty ->
             let _ = above_def (Node (Black, left, pivot, right)) bound in
             ())

  let balance_below (colour : color) (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical) (bound : key @ logical)
      : unit{
        below (balance colour left pivot right) bound
        = (int_less (K.compare pivot bound) 0
           && below left bound && below right bound)
      }
    =
    let _ = balance_def colour left pivot right in
    match colour with
    | Red -> let _ = below_def (Node (Red, left, pivot, right)) bound in ()
    | Black ->
      match left with
      | Empty -> rotate_right_side_below left pivot right bound
      | Node (lc, ll, lk, lr) ->
        match lc with
        | Black -> rotate_right_side_below left pivot right bound
        | Red ->
          match ll with
          | Node (llc, a, x, b) ->
            (match llc with
             | Red ->
               let _ = below_def
                 (Node (Red, Node (Black, a, x, b), lk,
                        Node (Black, lr, pivot, right))) bound in
               let _ = below_def (Node (Black, a, x, b)) bound in
               let _ = below_def (Node (Black, lr, pivot, right)) bound in
               let _ = below_def (Node (Red, ll, lk, lr)) bound in
               let _ = below_def (Node (Red, a, x, b)) bound in
               ()
             | Black ->
               (match lr with
                | Node (lrc, b, y, c) ->
                  (match lrc with
                   | Red ->
                     let _ = below_def
                       (Node (Red, Node (Black, ll, lk, b), y,
                              Node (Black, c, pivot, right))) bound in
                     let _ = below_def (Node (Black, ll, lk, b)) bound in
                     let _ = below_def (Node (Black, c, pivot, right)) bound in
                     let _ = below_def (Node (Red, ll, lk, lr)) bound in
                     let _ = below_def (Node (Red, b, y, c)) bound in
                     ()
                   | Black -> rotate_right_side_below left pivot right bound)
                | Empty -> rotate_right_side_below left pivot right bound))
          | Empty ->
            (match lr with
             | Node (lrc, b, y, c) ->
               (match lrc with
                | Red ->
                  let _ = below_def
                    (Node (Red, Node (Black, ll, lk, b), y,
                           Node (Black, c, pivot, right))) bound in
                  let _ = below_def (Node (Black, ll, lk, b)) bound in
                  let _ = below_def (Node (Black, c, pivot, right)) bound in
                  let _ = below_def (Node (Red, ll, lk, lr)) bound in
                  let _ = below_def (Node (Red, b, y, c)) bound in
                  let _ = below_def Empty bound in
                  ()
                | Black -> rotate_right_side_below left pivot right bound)
             | Empty -> rotate_right_side_below left pivot right bound)

  let balance_above (colour : color) (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical) (bound : key @ logical)
      : unit{
        above (balance colour left pivot right) bound
        = (int_less (K.compare bound pivot) 0
           && above left bound && above right bound)
      }
    =
    let _ = balance_def colour left pivot right in
    match colour with
    | Red -> let _ = above_def (Node (Red, left, pivot, right)) bound in ()
    | Black ->
      match left with
      | Empty -> rotate_right_side_above left pivot right bound
      | Node (lc, ll, lk, lr) ->
        match lc with
        | Black -> rotate_right_side_above left pivot right bound
        | Red ->
          match ll with
          | Node (llc, a, x, b) ->
            (match llc with
             | Red ->
               let _ = above_def
                 (Node (Red, Node (Black, a, x, b), lk,
                        Node (Black, lr, pivot, right))) bound in
               let _ = above_def (Node (Black, a, x, b)) bound in
               let _ = above_def (Node (Black, lr, pivot, right)) bound in
               let _ = above_def (Node (Red, ll, lk, lr)) bound in
               let _ = above_def (Node (Red, a, x, b)) bound in
               ()
             | Black ->
               (match lr with
                | Node (lrc, b, y, c) ->
                  (match lrc with
                   | Red ->
                     let _ = above_def
                       (Node (Red, Node (Black, ll, lk, b), y,
                              Node (Black, c, pivot, right))) bound in
                     let _ = above_def (Node (Black, ll, lk, b)) bound in
                     let _ = above_def (Node (Black, c, pivot, right)) bound in
                     let _ = above_def (Node (Red, ll, lk, lr)) bound in
                     let _ = above_def (Node (Red, b, y, c)) bound in
                     ()
                   | Black -> rotate_right_side_above left pivot right bound)
                | Empty -> rotate_right_side_above left pivot right bound))
          | Empty ->
            (match lr with
             | Node (lrc, b, y, c) ->
               (match lrc with
                | Red ->
                  let _ = above_def
                    (Node (Red, Node (Black, ll, lk, b), y,
                           Node (Black, c, pivot, right))) bound in
                  let _ = above_def (Node (Black, ll, lk, b)) bound in
                  let _ = above_def (Node (Black, c, pivot, right)) bound in
                  let _ = above_def (Node (Red, ll, lk, lr)) bound in
                  let _ = above_def (Node (Red, b, y, c)) bound in
                  let _ = above_def Empty bound in
                  ()
                | Black -> rotate_right_side_above left pivot right bound)
             | Empty -> rotate_right_side_above left pivot right bound)

  let rotate_right_side_ordered (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical)
      (_ordered : unit{
         ordered left = true
         && ordered right = true
         && below left pivot = true
         && above right pivot = true
       })
      : unit{ ordered (rotate_right_side left pivot right) = true }
    =
    let _ = rotate_right_side_def left pivot right in
    match right with
    | Empty -> let _ = ordered_def (Node (Black, left, pivot, right)) in ()
    | Node (rc, rl, rk, rr) ->
      match rc with
      | Black -> let _ = ordered_def (Node (Black, left, pivot, right)) in ()
      | Red ->
        let _ = ordered_def (Node (Red, rl, rk, rr)) in
        let _ = above_def (Node (Red, rl, rk, rr)) pivot in
        (match rl with
         | Node (rlc, b, y, c) ->
           (match rlc with
            | Red ->
              let _ = ordered_def (Node (Red, b, y, c)) in
              let _ = above_def (Node (Red, b, y, c)) pivot in
              let _ = below_def (Node (Red, b, y, c)) rk in
              let _ = below_weaken y pivot left () in
              let _ = above_weaken y rk rr () in
              let _ = ordered_def
                (Node (Red, Node (Black, left, pivot, b), y,
                       Node (Black, c, rk, rr))) in
              let _ = ordered_def (Node (Black, left, pivot, b)) in
              let _ = ordered_def (Node (Black, c, rk, rr)) in
              let _ = below_def (Node (Black, left, pivot, b)) y in
              let _ = above_def (Node (Black, c, rk, rr)) y in
              ()
            | Black ->
              (match rr with
               | Node (rrc, c, z, d) ->
                 (match rrc with
                  | Red ->
                    let _ = ordered_def (Node (Red, c, z, d)) in
                    let _ = above_def (Node (Red, c, z, d)) rk in
                    let _ = below_weaken rk pivot left () in
                    let _ = ordered_def
                      (Node (Red, Node (Black, left, pivot, rl), rk,
                             Node (Black, c, z, d))) in
                    let _ = ordered_def (Node (Black, left, pivot, rl)) in
                    let _ = ordered_def (Node (Black, c, z, d)) in
                    let _ = below_def (Node (Black, left, pivot, rl)) rk in
                    let _ = above_def (Node (Black, c, z, d)) rk in
                    ()
                  | Black ->
                    let _ = ordered_def (Node (Black, left, pivot, right)) in
                    ())
               | Empty ->
                 let _ = ordered_def (Node (Black, left, pivot, right)) in
                 ()))
         | Empty ->
           (match rr with
            | Node (rrc, c, z, d) ->
              (match rrc with
               | Red ->
                 let _ = ordered_def (Node (Red, c, z, d)) in
                 let _ = above_def (Node (Red, c, z, d)) rk in
                 let _ = below_weaken rk pivot left () in
                 let _ = ordered_def
                   (Node (Red, Node (Black, left, pivot, rl), rk,
                          Node (Black, c, z, d))) in
                 let _ = ordered_def (Node (Black, left, pivot, rl)) in
                 let _ = ordered_def (Node (Black, c, z, d)) in
                 let _ = below_def (Node (Black, left, pivot, rl)) rk in
                 let _ = above_def (Node (Black, c, z, d)) rk in
                 ()
               | Black ->
                 let _ = ordered_def (Node (Black, left, pivot, right)) in
                 ())
            | Empty ->
              let _ = ordered_def (Node (Black, left, pivot, right)) in
              ()))

  let balance_ordered (colour : color) (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical)
      (_ordered : unit{
         ordered left = true
         && ordered right = true
         && below left pivot = true
         && above right pivot = true
       })
      : unit{ ordered (balance colour left pivot right) = true }
    =
    let _ = balance_def colour left pivot right in
    match colour with
    | Red -> let _ = ordered_def (Node (Red, left, pivot, right)) in ()
    | Black ->
      match left with
      | Empty -> rotate_right_side_ordered left pivot right ()
      | Node (lc, ll, lk, lr) ->
        match lc with
        | Black -> rotate_right_side_ordered left pivot right ()
        | Red ->
          let _ = ordered_def (Node (Red, ll, lk, lr)) in
          let _ = below_def (Node (Red, ll, lk, lr)) pivot in
          (match ll with
           | Node (llc, a, x, b) ->
             (match llc with
              | Red ->
                let _ = ordered_def (Node (Red, a, x, b)) in
                let _ = below_def (Node (Red, a, x, b)) lk in
                let _ = above_weaken lk pivot right () in
                let _ = ordered_def
                  (Node (Red, Node (Black, a, x, b), lk,
                         Node (Black, lr, pivot, right))) in
                let _ = ordered_def (Node (Black, a, x, b)) in
                let _ = ordered_def (Node (Black, lr, pivot, right)) in
                let _ = below_def (Node (Black, a, x, b)) lk in
                let _ = above_def (Node (Black, lr, pivot, right)) lk in
                ()
              | Black ->
                (match lr with
                 | Node (lrc, b, y, c) ->
                   (match lrc with
                    | Red ->
                      let _ = ordered_def (Node (Red, b, y, c)) in
                      let _ = above_def (Node (Red, b, y, c)) lk in
                      let _ = below_def (Node (Red, b, y, c)) pivot in
                      let _ = below_weaken y lk ll () in
                      let _ = above_weaken y pivot right () in
                      let _ = ordered_def
                        (Node (Red, Node (Black, ll, lk, b), y,
                               Node (Black, c, pivot, right))) in
                      let _ = ordered_def (Node (Black, ll, lk, b)) in
                      let _ = ordered_def (Node (Black, c, pivot, right)) in
                      let _ = below_def (Node (Black, ll, lk, b)) y in
                      let _ = above_def (Node (Black, c, pivot, right)) y in
                      ()
                    | Black -> rotate_right_side_ordered left pivot right ())
                 | Empty -> rotate_right_side_ordered left pivot right ()))
           | Empty ->
             (match lr with
              | Node (lrc, b, y, c) ->
                (match lrc with
                 | Red ->
                   let _ = ordered_def (Node (Red, b, y, c)) in
                   let _ = above_def (Node (Red, b, y, c)) lk in
                   let _ = below_def (Node (Red, b, y, c)) pivot in
                   let _ = below_weaken y lk ll () in
                   let _ = above_weaken y pivot right () in
                   let _ = ordered_def
                     (Node (Red, Node (Black, ll, lk, b), y,
                            Node (Black, c, pivot, right))) in
                   let _ = ordered_def (Node (Black, ll, lk, b)) in
                   let _ = ordered_def (Node (Black, c, pivot, right)) in
                   let _ = below_def (Node (Black, ll, lk, b)) y in
                   let _ = above_def (Node (Black, c, pivot, right)) y in
                   ()
                 | Black -> rotate_right_side_ordered left pivot right ())
              | Empty -> rotate_right_side_ordered left pivot right ()))

  let rotate_right_side_proper (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical)
      (_proper : unit{ no_red_red left = true && no_red_red right = true })
      : unit{ no_red_red (rotate_right_side left pivot right) = true }
    =
    rotate_right_side_def left pivot right;
    match right with
    | Empty ->
      no_red_red_def (Node (Black, left, pivot, right));
      ()
    | Node (rc, rl, rk, rr) ->
      match rc with
      | Black ->
        no_red_red_def (Node (Black, left, pivot, right));
        ()
      | Red ->
        (* A proper red node has black children, so no reshaping branch of
           [rotate_right_side] can fire. *)
        no_red_red_def (Node (Red, rl, rk, rr));
        (match rl with
         | Node (rlc, b, y, c) ->
           is_black_def (Node (rlc, b, y, c));
           (match rlc with
            | Red -> ()
            | Black ->
              (match rr with
               | Node (rrc, c2, z, d) ->
                 is_black_def (Node (rrc, c2, z, d));
                 (match rrc with
                  | Red -> ()
                  | Black ->
                    no_red_red_def (Node (Black, left, pivot, right));
                    ())
               | Empty ->
                 no_red_red_def (Node (Black, left, pivot, right));
                 ()))
         | Empty ->
           (match rr with
            | Node (rrc, c2, z, d) ->
              is_black_def (Node (rrc, c2, z, d));
              (match rrc with
               | Red -> ()
               | Black ->
                 no_red_red_def (Node (Black, left, pivot, right));
                 ())
            | Empty ->
              no_red_red_def (Node (Black, left, pivot, right));
              ()))

  let rotate_right_side_repair (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical)
      (_proper : unit{ no_red_red left = true && infrared right = true })
      : unit{ no_red_red (rotate_right_side left pivot right) = true }
    =
    rotate_right_side_def left pivot right;
    infrared_def right;
    match right with
    | Empty ->
      no_red_red_def Empty;
      no_red_red_def (Node (Black, left, pivot, right));
      ()
    | Node (rc, rl, rk, rr) ->
      match rc with
      | Black ->
        no_red_red_def (Node (Black, rl, rk, rr));
        no_red_red_def (Node (Black, left, pivot, right));
        ()
      | Red ->
        (match rl with
         | Node (rlc, b, y, c) ->
           is_black_def (Node (rlc, b, y, c));
           (match rlc with
            | Red ->
              no_red_red_def (Node (Red, b, y, c));
              no_red_red_def
                (Node (Red, Node (Black, left, pivot, b), y,
                       Node (Black, c, rk, rr)));
              no_red_red_def (Node (Black, left, pivot, b));
              no_red_red_def (Node (Black, c, rk, rr));
              is_black_def (Node (Black, left, pivot, b));
              is_black_def (Node (Black, c, rk, rr));
              ()
            | Black ->
              (match rr with
               | Node (rrc, c2, z, d) ->
                 is_black_def (Node (rrc, c2, z, d));
                 (match rrc with
                  | Red ->
                    no_red_red_def (Node (Red, c2, z, d));
                    no_red_red_def
                      (Node (Red, Node (Black, left, pivot, rl), rk,
                             Node (Black, c2, z, d)));
                    no_red_red_def (Node (Black, left, pivot, rl));
                    no_red_red_def (Node (Black, c2, z, d));
                    is_black_def (Node (Black, left, pivot, rl));
                    is_black_def (Node (Black, c2, z, d));
                    ()
                  | Black ->
                    no_red_red_def (Node (Red, rl, rk, rr));
                    no_red_red_def (Node (Black, left, pivot, right));
                    ())
               | Empty ->
                 is_black_def Empty;
                 no_red_red_def (Node (Red, rl, rk, rr));
                 no_red_red_def (Node (Black, left, pivot, right));
                 ()))
         | Empty ->
           is_black_def Empty;
           (match rr with
            | Node (rrc, c2, z, d) ->
              is_black_def (Node (rrc, c2, z, d));
              (match rrc with
               | Red ->
                 no_red_red_def (Node (Red, c2, z, d));
                 no_red_red_def
                   (Node (Red, Node (Black, left, pivot, rl), rk,
                          Node (Black, c2, z, d)));
                 no_red_red_def (Node (Black, left, pivot, rl));
                 no_red_red_def (Node (Black, c2, z, d));
                 is_black_def (Node (Black, left, pivot, rl));
                 is_black_def (Node (Black, c2, z, d));
                 ()
               | Black ->
                 no_red_red_def (Node (Red, rl, rk, rr));
                 no_red_red_def (Node (Black, left, pivot, right));
                 ())
            | Empty ->
              no_red_red_def (Node (Red, rl, rk, rr));
              no_red_red_def (Node (Black, left, pivot, right));
              ()))

  (* A red-red shape at the root of the left child is exactly what the left
     half of [balance] repairs. *)
  let balance_black_left (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical)
      (_proper : unit{ infrared left = true && no_red_red right = true })
      : unit{ no_red_red (balance Black left pivot right) = true }
    =
    balance_def Black left pivot right;
    infrared_def left;
    match left with
    | Empty ->
      no_red_red_def Empty;
      rotate_right_side_proper left pivot right ()
    | Node (lc, ll, lk, lr) ->
      match lc with
      | Black ->
        no_red_red_def (Node (Black, ll, lk, lr));
        rotate_right_side_proper left pivot right ()
      | Red ->
        (match ll with
         | Node (llc, a, x, b) ->
           is_black_def (Node (llc, a, x, b));
           (match llc with
            | Red ->
              no_red_red_def (Node (Red, a, x, b));
              no_red_red_def
                (Node (Red, Node (Black, a, x, b), lk,
                       Node (Black, lr, pivot, right)));
              no_red_red_def (Node (Black, a, x, b));
              no_red_red_def (Node (Black, lr, pivot, right));
              is_black_def (Node (Black, a, x, b));
              is_black_def (Node (Black, lr, pivot, right));
              ()
            | Black ->
              (match lr with
               | Node (lrc, b, y, c) ->
                 is_black_def (Node (lrc, b, y, c));
                 (match lrc with
                  | Red ->
                    no_red_red_def (Node (Red, b, y, c));
                    no_red_red_def
                      (Node (Red, Node (Black, ll, lk, b), y,
                             Node (Black, c, pivot, right)));
                    no_red_red_def (Node (Black, ll, lk, b));
                    no_red_red_def (Node (Black, c, pivot, right));
                    is_black_def (Node (Black, ll, lk, b));
                    is_black_def (Node (Black, c, pivot, right));
                    ()
                  | Black ->
                    no_red_red_def (Node (Red, ll, lk, lr));
                    rotate_right_side_proper left pivot right ())
               | Empty ->
                 is_black_def Empty;
                 no_red_red_def Empty;
                 no_red_red_def (Node (Red, ll, lk, lr));
                 rotate_right_side_proper left pivot right ()))
         | Empty ->
           is_black_def Empty;
           no_red_red_def Empty;
           (match lr with
            | Node (lrc, b, y, c) ->
              is_black_def (Node (lrc, b, y, c));
              (match lrc with
               | Red ->
                 no_red_red_def (Node (Red, b, y, c));
                 no_red_red_def
                   (Node (Red, Node (Black, ll, lk, b), y,
                          Node (Black, c, pivot, right)));
                 no_red_red_def (Node (Black, ll, lk, b));
                 no_red_red_def (Node (Black, c, pivot, right));
                 is_black_def (Node (Black, ll, lk, b));
                 is_black_def (Node (Black, c, pivot, right));
                 ()
               | Black ->
                 no_red_red_def (Node (Red, ll, lk, lr));
                 rotate_right_side_proper left pivot right ())
            | Empty ->
              no_red_red_def (Node (Red, ll, lk, lr));
              rotate_right_side_proper left pivot right ()))

  (* When the left child is already proper the left reshaping branches cannot
     fire, and the repair happens on the right spine. *)
  let balance_black_right (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical)
      (_proper : unit{ no_red_red left = true && infrared right = true })
      : unit{ no_red_red (balance Black left pivot right) = true }
    =
    balance_def Black left pivot right;
    match left with
    | Empty -> rotate_right_side_repair left pivot right ()
    | Node (lc, ll, lk, lr) ->
      no_red_red_def (Node (lc, ll, lk, lr));
      match lc with
      | Black -> rotate_right_side_repair left pivot right ()
      | Red ->
        (match ll with
         | Node (llc, a, x, b) ->
           is_black_def (Node (llc, a, x, b));
           (match llc with
            | Red -> ()
            | Black ->
              (match lr with
               | Node (lrc, b, y, c) ->
                 is_black_def (Node (lrc, b, y, c));
                 (match lrc with
                  | Red -> ()
                  | Black -> rotate_right_side_repair left pivot right ())
               | Empty -> rotate_right_side_repair left pivot right ()))
         | Empty ->
           (match lr with
            | Node (lrc, b, y, c) ->
              is_black_def (Node (lrc, b, y, c));
              (match lrc with
               | Red -> ()
               | Black -> rotate_right_side_repair left pivot right ())
            | Empty -> rotate_right_side_repair left pivot right ()))

  (* An existing pivot is returned unchanged.  Without this case [ins] appends a
     second copy of the pivot on the right spine, which breaks the ordering
     invariant and makes the structure not a set. *)
  let[@vox.def] rec ins (new_key : key @ logical) (tree : t @ logical) : t =
    match tree with
    | Empty -> Node (Red, Empty, new_key, Empty)
    | Node (c, l, k, r) ->
      if int_equal (K.compare new_key k) 0
      then tree
      else if int_less (K.compare new_key k) 0
      then balance c (ins new_key l) k r
      else balance c l k (ins new_key r)

  let rec ins_preserves (new_key : key @ logical) (tree : t @ logical) (query : key @ logical)
      : unit{
        occurs query (ins new_key tree)
        = (int_equal (K.compare query new_key) 0 || occurs query tree)
      }
    =
    match tree with
    | Empty ->
      let _ = ins_def new_key Empty in
      let _ = occurs_def query (Node (Red, Empty, new_key, Empty)) in
      let _ = occurs_def query Empty in
      ()
    | Node (c, l, k, r) ->
      let _ = ins_def new_key (Node (c, l, k, r)) in
      let _ = occurs_def query (Node (c, l, k, r)) in
      let choice = direction new_key k in
      let _ = direction_def new_key k in
      (* [compare new_key k = 0] is not [new_key = k] until the law says so,
         and only that turns [compare query new_key] into
         [compare query k]. *)
      K.compare_zero_iff_equal ~left:new_key ~right:k;
      match choice with
      | Same -> ()
      | Left ->
        let _ = balance_preserves c (ins new_key l) k r query in
        let _ = ins_preserves new_key l query in
        ()
      | Right ->
        let _ = balance_preserves c l k (ins new_key r) query in
        let _ = ins_preserves new_key r query in
        ()

  let rec ins_below (bound : key @ logical)
      (new_key : key{ K.compare _ bound < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ below tree bound = true })
      : unit{ below (ins new_key tree) bound = true } =
    match tree with
    | Empty ->
      ins_def new_key Empty;
      below_def (Node (Red, Empty, new_key, Empty)) bound;
      below_def Empty bound;
      ()
    | Node (c, l, k, r) ->
      ins_def new_key (Node (c, l, k, r));
      below_def (Node (c, l, k, r)) bound;
      let choice = direction new_key k in
      direction_def new_key k;
      match choice with
      | Same -> ()
      | Left ->
        ins_below bound new_key l ();
        balance_below c (ins new_key l) k r bound;
        ()
      | Right ->
        ins_below bound new_key r ();
        balance_below c l k (ins new_key r) bound;
        ()

  let rec ins_above (bound : key @ logical)
      (new_key : key{ K.compare bound _ < 0 } @ logical)
      (tree : t @ logical)
      (_bounded : unit{ above tree bound = true })
      : unit{ above (ins new_key tree) bound = true } =
    match tree with
    | Empty ->
      ins_def new_key Empty;
      above_def (Node (Red, Empty, new_key, Empty)) bound;
      above_def Empty bound;
      ()
    | Node (c, l, k, r) ->
      ins_def new_key (Node (c, l, k, r));
      above_def (Node (c, l, k, r)) bound;
      let choice = direction new_key k in
      direction_def new_key k;
      match choice with
      | Same -> ()
      | Left ->
        ins_above bound new_key l ();
        balance_above c (ins new_key l) k r bound;
        ()
      | Right ->
        ins_above bound new_key r ();
        balance_above c l k (ins new_key r) bound;
        ()

  let rec ins_ordered (new_key : key @ logical) (tree : t @ logical)
      (_ordered : unit{ ordered tree = true })
      : unit{ ordered (ins new_key tree) = true } =
    match tree with
    | Empty ->
      ins_def new_key Empty;
      ordered_def (Node (Red, Empty, new_key, Empty));
      ordered_def Empty;
      below_def Empty new_key;
      above_def Empty new_key;
      ()
    | Node (c, l, k, r) ->
      ins_def new_key (Node (c, l, k, r));
      ordered_def (Node (c, l, k, r));
      let choice = direction new_key k in
      direction_def new_key k;
      match choice with
      | Same -> ()
      | Left ->
        ins_ordered new_key l ();
        ins_below k new_key l ();
        balance_ordered c (ins new_key l) k r ();
        ()
      | Right ->
        K.compare_sign_reversal ~left:k ~right:new_key;
        ins_ordered new_key r ();
        ins_above k new_key r ();
        balance_ordered c l k (ins new_key r) ();
        ()

  let rec ins_rb (new_key : key @ logical) (tree : t @ logical)
      (_proper : unit{ no_red_red tree = true })
      : unit{
        infrared (ins new_key tree) = true
        && (is_black tree = false
            || no_red_red (ins new_key tree) = true)
      }
    =
    match tree with
    | Empty ->
      ins_def new_key Empty;
      is_black_def Empty;
      no_red_red_def Empty;
      is_black_def (Node (Red, Empty, new_key, Empty));
      no_red_red_def (Node (Red, Empty, new_key, Empty));
      infrared_def (Node (Red, Empty, new_key, Empty));
      ()
    | Node (c, l, k, r) ->
      ins_def new_key (Node (c, l, k, r));
      no_red_red_def (Node (c, l, k, r));
      is_black_def (Node (c, l, k, r));
      let choice = direction new_key k in
      direction_def new_key k;
      match choice with
      | Same -> no_red_red_infrared (Node (c, l, k, r)) ()
      | Left ->
        ins_rb new_key l ();
        (match c with
         | Black ->
           balance_black_left (ins new_key l) k r ();
           no_red_red_infrared (balance Black (ins new_key l) k r) ();
           ()
         | Red ->
           balance_def Red (ins new_key l) k r;
           infrared_def (Node (Red, ins new_key l, k, r));
           ())
      | Right ->
        ins_rb new_key r ();
        (match c with
         | Black ->
           balance_black_right l k (ins new_key r) ();
           no_red_red_infrared (balance Black l k (ins new_key r)) ();
           ()
         | Red ->
           balance_def Red l k (ins new_key r);
           infrared_def (Node (Red, l, k, ins new_key r));
           ())

  let rotate_right_side_black_balanced (left : t @ logical) (pivot : key @ logical)
      (right : t @ logical)
      (_balanced : unit{
         black_balanced left = true
         && black_balanced right = true
         && Bigint.equal (black_height left) (black_height right) = true
       })
      : unit{
        black_balanced (rotate_right_side left pivot right) = true
        && Bigint.equal (black_height (rotate_right_side left pivot right))
             (Bigint.add Bigint.one (black_height left)) = true
      }
    =
    rotate_right_side_def left pivot right;
    black_balanced_def (Node (Black, left, pivot, right));
    black_height_def (Node (Black, left, pivot, right));
    match right with
    | Empty -> ()
    | Node (rc, rl, rk, rr) ->
      black_balanced_def (Node (rc, rl, rk, rr));
      black_height_def (Node (rc, rl, rk, rr));
      match rc with
      | Black -> ()
      | Red ->
        (match rl with
         | Node (rlc, b, y, c) ->
           (match rlc with
            | Red ->
              black_balanced_def (Node (Red, b, y, c));
              black_height_def (Node (Red, b, y, c));
              black_balanced_def
                (Node (Red, Node (Black, left, pivot, b), y,
                       Node (Black, c, rk, rr)));
              black_height_def
                (Node (Red, Node (Black, left, pivot, b), y,
                       Node (Black, c, rk, rr)));
              black_balanced_def (Node (Black, left, pivot, b));
              black_height_def (Node (Black, left, pivot, b));
              black_balanced_def (Node (Black, c, rk, rr));
              black_height_def (Node (Black, c, rk, rr));
              ()
            | Black ->
              (match rr with
               | Node (rrc, c2, z, d) ->
                 (match rrc with
                  | Red ->
                    black_balanced_def (Node (Red, c2, z, d));
                    black_height_def (Node (Red, c2, z, d));
                    black_balanced_def
                      (Node (Red, Node (Black, left, pivot, rl), rk,
                             Node (Black, c2, z, d)));
                    black_height_def
                      (Node (Red, Node (Black, left, pivot, rl), rk,
                             Node (Black, c2, z, d)));
                    black_balanced_def (Node (Black, left, pivot, rl));
                    black_height_def (Node (Black, left, pivot, rl));
                    black_balanced_def (Node (Black, c2, z, d));
                    black_height_def (Node (Black, c2, z, d));
                    ()
                  | Black -> ())
               | Empty -> ()))
         | Empty ->
           (match rr with
            | Node (rrc, c2, z, d) ->
              (match rrc with
               | Red ->
                 black_balanced_def (Node (Red, c2, z, d));
                 black_height_def (Node (Red, c2, z, d));
                 black_balanced_def
                   (Node (Red, Node (Black, left, pivot, rl), rk,
                          Node (Black, c2, z, d)));
                 black_height_def
                   (Node (Red, Node (Black, left, pivot, rl), rk,
                          Node (Black, c2, z, d)));
                 black_balanced_def (Node (Black, left, pivot, rl));
                 black_height_def (Node (Black, left, pivot, rl));
                 black_balanced_def (Node (Black, c2, z, d));
                 black_height_def (Node (Black, c2, z, d));
                 ()
               | Black -> ())
            | Empty -> ()))

  let balance_black_balanced (colour : color) (left : t @ logical)
      (pivot : key @ logical) (right : t @ logical)
      (_balanced : unit{
         black_balanced left = true
         && black_balanced right = true
         && Bigint.equal (black_height left) (black_height right) = true
       })
      : unit{
        black_balanced (balance colour left pivot right) = true
        && Bigint.equal (black_height (balance colour left pivot right))
             (black_height (Node (colour, left, pivot, right))) = true
      }
    =
    balance_def colour left pivot right;
    black_height_def (Node (colour, left, pivot, right));
    match colour with
    | Red ->
      black_balanced_def (Node (Red, left, pivot, right));
      black_height_def (Node (Red, left, pivot, right));
      ()
    | Black ->
      black_height_def (Node (Black, left, pivot, right));
      (match left with
       | Empty -> rotate_right_side_black_balanced left pivot right ()
       | Node (lc, ll, lk, lr) ->
         black_balanced_def (Node (lc, ll, lk, lr));
         black_height_def (Node (lc, ll, lk, lr));
         (match lc with
          | Black -> rotate_right_side_black_balanced left pivot right ()
          | Red ->
            (match ll with
             | Node (llc, a, x, b) ->
               (match llc with
                | Red ->
                  black_balanced_def (Node (Red, a, x, b));
                  black_height_def (Node (Red, a, x, b));
                  black_balanced_def
                    (Node (Red, Node (Black, a, x, b), lk,
                           Node (Black, lr, pivot, right)));
                  black_height_def
                    (Node (Red, Node (Black, a, x, b), lk,
                           Node (Black, lr, pivot, right)));
                  black_balanced_def (Node (Black, a, x, b));
                  black_height_def (Node (Black, a, x, b));
                  black_balanced_def (Node (Black, lr, pivot, right));
                  black_height_def (Node (Black, lr, pivot, right));
                  ()
                | Black ->
                  (match lr with
                   | Node (lrc, b, y, c) ->
                     (match lrc with
                      | Red ->
                        black_balanced_def (Node (Red, b, y, c));
                        black_height_def (Node (Red, b, y, c));
                        black_balanced_def
                          (Node (Red, Node (Black, ll, lk, b), y,
                                 Node (Black, c, pivot, right)));
                        black_height_def
                          (Node (Red, Node (Black, ll, lk, b), y,
                                 Node (Black, c, pivot, right)));
                        black_balanced_def (Node (Black, ll, lk, b));
                        black_height_def (Node (Black, ll, lk, b));
                        black_balanced_def (Node (Black, c, pivot, right));
                        black_height_def (Node (Black, c, pivot, right));
                        ()
                      | Black ->
                        rotate_right_side_black_balanced left pivot right ())
                   | Empty ->
                     rotate_right_side_black_balanced left pivot right ()))
             | Empty ->
               (match lr with
                | Node (lrc, b, y, c) ->
                  (match lrc with
                   | Red ->
                     black_balanced_def (Node (Red, b, y, c));
                     black_height_def (Node (Red, b, y, c));
                     black_balanced_def
                       (Node (Red, Node (Black, ll, lk, b), y,
                              Node (Black, c, pivot, right)));
                     black_height_def
                       (Node (Red, Node (Black, ll, lk, b), y,
                              Node (Black, c, pivot, right)));
                     black_balanced_def (Node (Black, ll, lk, b));
                     black_height_def (Node (Black, ll, lk, b));
                     black_balanced_def (Node (Black, c, pivot, right));
                     black_height_def (Node (Black, c, pivot, right));
                     ()
                   | Black ->
                     rotate_right_side_black_balanced left pivot right ())
                | Empty ->
                  rotate_right_side_black_balanced left pivot right ()))))

  let rec ins_black_balanced (new_key : key @ logical) (tree : t @ logical)
      (_balanced : unit{ black_balanced tree = true })
      : unit{
        black_balanced (ins new_key tree) = true
        && Bigint.equal (black_height (ins new_key tree))
             (black_height tree) = true
      }
    =
    match tree with
    | Empty ->
      ins_def new_key Empty;
      black_balanced_def (Node (Red, Empty, new_key, Empty));
      black_height_def (Node (Red, Empty, new_key, Empty));
      black_balanced_def Empty;
      black_height_def Empty;
      ()
    | Node (c, l, k, r) ->
      ins_def new_key (Node (c, l, k, r));
      black_balanced_def (Node (c, l, k, r));
      black_height_def (Node (c, l, k, r));
      let choice = direction new_key k in
      direction_def new_key k;
      match choice with
      | Same -> ()
      | Left ->
        ins_black_balanced new_key l ();
        balance_black_balanced c (ins new_key l) k r ();
        black_height_def (Node (c, ins new_key l, k, r));
        ()
      | Right ->
        ins_black_balanced new_key r ();
        balance_black_balanced c l k (ins new_key r) ();
        black_height_def (Node (c, l, k, ins new_key r));
        ()

  let[@vox.def] blacken (tree : t @ logical) : t =
    match tree with
    | Empty -> Empty
    | Node (_c, l, k, r) -> Node (Black, l, k, r)

  let blacken_preserves (tree : t @ logical) (query : key @ logical)
      : unit{ occurs query (blacken tree) = occurs query tree } =
    let _ = blacken_def tree in
    match tree with
    | Empty ->
      let _ = occurs_def query Empty in
      ()
    | Node (c, l, k, r) ->
      let _ = occurs_def query (Node (Black, l, k, r)) in
      let _ = occurs_def query (Node (c, l, k, r)) in
      ()

  let blacken_ordered (tree : t @ logical)
      (_ordered : unit{ ordered tree = true })
      : unit{ ordered (blacken tree) = true } =
    blacken_def tree;
    match tree with
    | Empty ->
      ordered_def Empty;
      ()
    | Node (c, l, k, r) ->
      ordered_def (Node (c, l, k, r));
      ordered_def (Node (Black, l, k, r));
      ()

  let blacken_rb (tree : t @ logical)
      (_infrared : unit{ infrared tree = true })
      : unit{ no_red_red (blacken tree) = true } =
    blacken_def tree;
    infrared_def tree;
    match tree with
    | Empty ->
      no_red_red_def Empty;
      ()
    | Node (c, l, k, r) ->
      no_red_red_def (Node (Black, l, k, r));
      ()

  let blacken_black_balanced (tree : t @ logical)
      (_balanced : unit{ black_balanced tree = true })
      : unit{ black_balanced (blacken tree) = true } =
    blacken_def tree;
    match tree with
    | Empty ->
      black_balanced_def Empty;
      ()
    | Node (c, l, k, r) ->
      black_balanced_def (Node (c, l, k, r));
      black_balanced_def (Node (Black, l, k, r));
      ()

  let empty = Empty

  let[@vox.def] insert (new_key : key @ logical) (tree : t @ logical) : t =
    blacken (ins new_key tree)

  let insert_preserves (new_key : key @ logical) (tree : t @ logical) (query : key @ logical)
      : unit{
        occurs query (insert new_key tree)
        = (int_equal (K.compare query new_key) 0 || occurs query tree)
      }
    =
    let _ = insert_def new_key tree in
    let _ = blacken_preserves (ins new_key tree) query in
    let _ = ins_preserves new_key tree query in
    ()

  let insert_ordered (new_key : key @ logical) (tree : t @ logical)
      (_ordered : unit{ ordered tree = true })
      : unit{ ordered (insert new_key tree) = true } =
    insert_def new_key tree;
    ins_ordered new_key tree ();
    blacken_ordered (ins new_key tree) ();
    ()

  let insert_rb (new_key : key @ logical) (tree : t @ logical)
      (_proper : unit{ no_red_red tree = true })
      : unit{ no_red_red (insert new_key tree) = true } =
    insert_def new_key tree;
    ins_rb new_key tree ();
    blacken_rb (ins new_key tree) ();
    ()

  let insert_black_balanced (new_key : key @ logical) (tree : t @ logical)
      (_balanced : unit{ black_balanced tree = true })
      : unit{ black_balanced (insert new_key tree) = true } =
    insert_def new_key tree;
    ins_black_balanced new_key tree ();
    blacken_black_balanced (ins new_key tree) ();
    ()

  (* A red-black tree: ordered, no red node with a red child, and every
     path to a leaf crossing the same number of black nodes. *)
  let[@vox.def] invariant (tree : t @ logical) =
    ordered tree && no_red_red tree && black_balanced tree

  let empty_law ~(query : key @ logical) : unit{ member query empty = false } =
    let _ = member_def query empty in
    ()

  let empty_invariant : unit{ invariant empty = true } =
    invariant_def empty;
    ordered_def empty;
    no_red_red_def empty;
    black_balanced_def empty;
    ()

  let insert_invariant ~(inserted : key @ logical) ~(set : t @ logical)
      ~(well_formed : unit{ invariant set = true })
      : unit{ invariant (insert inserted set) = true } =
    invariant_def set;
    invariant_def (insert inserted set);
    insert_ordered inserted set ();
    insert_rb inserted set ();
    insert_black_balanced inserted set ();
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
    insert_preserves inserted set query;
    K.compare_zero_iff_equal ~left:query ~right:inserted;
    ()

  type membership_side =
    | First
    | Second
    | Neither

  let[@vox.def] membership_side first_member second_member =
    if first_member
    then First
    else if second_member then Second else Neither

  let[@vox.def] rec agrees (left : t @ logical) (right : t @ logical)
      (nodes : t @ logical) =
    match nodes with
    | Empty -> true
    | Node (_colour, subleft, pivot, subright) ->
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
      ~(colour : color) ~(subleft : t @ logical) ~(pivot : key @ logical)
      ~(subright : t @ logical)
      ~proof:(_proof : unit{
         agrees left right (Node (colour, subleft, pivot, subright)) = true
       })
      : unit{
        member pivot left = member pivot right
        && agrees left right subleft = true
        && agrees left right subright = true
      } =
    agrees_def left right (Node (colour, subleft, pivot, subright));
    ()

  let rec agrees_member ~(left : t @ logical) ~(right : t @ logical)
      ~(nodes : t @ logical) ~(query : key @ logical)
      ~(agreement : unit{ agrees left right nodes = true })
      ~(present : unit{ member query nodes = true })
      : unit{ member query left = member query right } =
    match nodes with
    | Empty ->
      member_def query Empty;
      ()
    | Node (colour, subleft, pivot, subright) ->
      let facts =
        agrees_node ~left ~right ~colour ~subleft ~pivot ~subright
          ~proof:agreement
      in
      member_def query (Node (colour, subleft, pivot, subright));
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
      | Empty ->
        agrees_def left right Empty;
        ()
      | Node (colour, subleft, pivot, subright) ->
        pointwise ~query:pivot;
        prove subleft;
        prove subright;
        agrees_def left right (Node (colour, subleft, pivot, subright));
        ()
    in
    prove left;
    prove right;
    equal_def left right;
    ()
end
