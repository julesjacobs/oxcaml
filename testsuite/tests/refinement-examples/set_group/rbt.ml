type color =
  | Red
  | Black

type t =
  | Empty
  | Node of color * t * int * t

external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"

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
  | Empty -> false
  | Node (_colour, left, key, right) ->
    if int_equal query key
    then true
    else if int_less query key
    then member query left
    else member query right

(* Occurrence anywhere in the tree.  Okasaki's rotations are ordering-free
   rearrangements, so they are stated against this; [member_occurs] carries
   their conclusions back to the spine. *)
let[@vox.def] rec occurs (query : int) (tree : t @ logical) =
  match tree with
  | Empty -> false
  | Node (_colour, left, key, right) ->
    int_equal query key || occurs query left || occurs query right

let[@vox.def] rec below (tree : t @ logical) (bound : int) =
  match tree with
  | Empty -> true
  | Node (_colour, left, key, right) ->
    int_less key bound && below left bound && below right bound

let[@vox.def] rec above (tree : t @ logical) (bound : int) =
  match tree with
  | Empty -> true
  | Node (_colour, left, key, right) ->
    int_less bound key && above left bound && above right bound

let[@vox.def] rec ordered (tree : t @ logical) =
  match tree with
  | Empty -> true
  | Node (_colour, left, key, right) ->
    ordered left && ordered right && below left key && above right key

let rec below_weaken (hi : int) (lo : int{ _ < hi }) (tree : t @ logical)
    (_bounded : unit{ below tree lo = true })
    : unit{ below tree hi = true } =
  match tree with
  | Empty ->
    below_def Empty hi;
    ()
  | Node (colour, left, key, right) ->
    below_def (Node (colour, left, key, right)) lo;
    below_def (Node (colour, left, key, right)) hi;
    below_weaken hi lo left ();
    below_weaken hi lo right ();
    ()

let rec above_weaken (lo : int) (hi : int{ lo < _ }) (tree : t @ logical)
    (_bounded : unit{ above tree hi = true })
    : unit{ above tree lo = true } =
  match tree with
  | Empty ->
    above_def Empty lo;
    ()
  | Node (colour, left, key, right) ->
    above_def (Node (colour, left, key, right)) hi;
    above_def (Node (colour, left, key, right)) lo;
    above_weaken lo hi left ();
    above_weaken lo hi right ();
    ()

let rec below_absent (bound : int) (query : int{ bound < _ })
    (tree : t @ logical)
    (_bounded : unit{ below tree bound = true })
    : unit{ occurs query tree = false } =
  match tree with
  | Empty ->
    occurs_def query Empty;
    ()
  | Node (colour, left, key, right) ->
    below_def (Node (colour, left, key, right)) bound;
    occurs_def query (Node (colour, left, key, right));
    below_absent bound query left ();
    below_absent bound query right ();
    ()

let rec above_absent (bound : int) (query : int{ _ < bound })
    (tree : t @ logical)
    (_bounded : unit{ above tree bound = true })
    : unit{ occurs query tree = false } =
  match tree with
  | Empty ->
    occurs_def query Empty;
    ()
  | Node (colour, left, key, right) ->
    above_def (Node (colour, left, key, right)) bound;
    occurs_def query (Node (colour, left, key, right));
    above_absent bound query left ();
    above_absent bound query right ();
    ()

(* On an ordered tree the single spine finds exactly the keys that occur. *)
let rec member_occurs (query : int) (tree : t @ logical)
    (_ordered : unit{ ordered tree = true })
    : unit{ member query tree = occurs query tree } =
  match tree with
  | Empty ->
    member_def query Empty;
    occurs_def query Empty;
    ()
  | Node (colour, left, key, right) ->
    ordered_def (Node (colour, left, key, right));
    member_def query (Node (colour, left, key, right));
    occurs_def query (Node (colour, left, key, right));
    member_occurs query left ();
    member_occurs query right ();
    let choice = direction query key in
    direction_def query key;
    match choice with
    | Same -> ()
    | Left -> above_absent key query right ()
    | Right -> below_absent key query left ()

(* Right-leaning half of Okasaki's balance: the caller has already ruled out a
   red-red violation on the left spine, so we only inspect the right child. *)
let[@vox.def] rotate_right_side (left : t @ logical) (key : int)
    (right : t @ logical) : t =
  match right with
  | Empty -> Node (Black, left, key, right)
  | Node (rc, rl, rk, rr) ->
    match rc with
    | Black -> Node (Black, left, key, right)
    | Red ->
      match rl with
      | Node (rlc, b, y, c) ->
        (match rlc with
         | Red ->
           Node (Red, Node (Black, left, key, b), y, Node (Black, c, rk, rr))
         | Black ->
           (match rr with
            | Node (rrc, c, z, d) ->
              (match rrc with
               | Red ->
                 Node (Red, Node (Black, left, key, rl), rk,
                       Node (Black, c, z, d))
               | Black -> Node (Black, left, key, right))
            | Empty -> Node (Black, left, key, right)))
      | Empty ->
        (match rr with
         | Node (rrc, c, z, d) ->
           (match rrc with
            | Red ->
              Node (Red, Node (Black, left, key, rl), rk,
                    Node (Black, c, z, d))
            | Black -> Node (Black, left, key, right))
         | Empty -> Node (Black, left, key, right))

(* Okasaki rotation dispatcher.  Red roots pass through; a black root may
   repair a red-red shape on either spine. *)
let[@vox.def] balance (colour : color) (left : t @ logical) (key : int)
    (right : t @ logical) : t =
  match colour with
  | Red -> Node (Red, left, key, right)
  | Black ->
    match left with
    | Empty -> rotate_right_side left key right
    | Node (lc, ll, lk, lr) ->
      match lc with
      | Black -> rotate_right_side left key right
      | Red ->
        match ll with
        | Node (llc, a, x, b) ->
          (match llc with
           | Red ->
             Node (Red, Node (Black, a, x, b), lk,
                   Node (Black, lr, key, right))
           | Black ->
             (match lr with
              | Node (lrc, b, y, c) ->
                (match lrc with
                 | Red ->
                   Node (Red, Node (Black, ll, lk, b), y,
                         Node (Black, c, key, right))
                 | Black -> rotate_right_side left key right)
              | Empty -> rotate_right_side left key right))
        | Empty ->
          (match lr with
           | Node (lrc, b, y, c) ->
             (match lrc with
              | Red ->
                Node (Red, Node (Black, ll, lk, b), y,
                      Node (Black, c, key, right))
              | Black -> rotate_right_side left key right)
           | Empty -> rotate_right_side left key right)

let rotate_right_side_preserves (left : t @ logical) (key : int)
    (right : t @ logical) (query : int)
    : unit{
      occurs query (rotate_right_side left key right)
      = (int_equal query key || occurs query left || occurs query right)
    }
  =
  let _ = rotate_right_side_def left key right in
  match right with
  | Empty -> let _ = occurs_def query (Node (Black, left, key, right)) in ()
  | Node (rc, rl, rk, rr) ->
    match rc with
    | Black -> let _ = occurs_def query (Node (Black, left, key, right)) in ()
    | Red ->
      match rl with
      | Node (rlc, b, y, c) ->
        (match rlc with
         | Red ->
           let _ = occurs_def query
             (Node (Red, Node (Black, left, key, b), y,
                    Node (Black, c, rk, rr))) in
           let _ = occurs_def query (Node (Black, left, key, b)) in
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
                   (Node (Red, Node (Black, left, key, rl), rk,
                          Node (Black, c, z, d))) in
                 let _ = occurs_def query (Node (Black, left, key, rl)) in
                 let _ = occurs_def query (Node (Black, c, z, d)) in
                 let _ = occurs_def query (Node (Red, rl, rk, rr)) in
                 let _ = occurs_def query (Node (Red, c, z, d)) in
                 ()
               | Black ->
                 let _ =
                   occurs_def query (Node (Black, left, key, right))
                 in
                 ())
            | Empty ->
              let _ = occurs_def query (Node (Black, left, key, right)) in
              ()))
      | Empty ->
        (match rr with
         | Node (rrc, c, z, d) ->
           (match rrc with
            | Red ->
              let _ = occurs_def query
                (Node (Red, Node (Black, left, key, rl), rk,
                       Node (Black, c, z, d))) in
              let _ = occurs_def query (Node (Black, left, key, rl)) in
              let _ = occurs_def query (Node (Black, c, z, d)) in
              let _ = occurs_def query (Node (Red, rl, rk, rr)) in
              let _ = occurs_def query (Node (Red, c, z, d)) in
              let _ = occurs_def query Empty in
              ()
            | Black ->
              let _ = occurs_def query (Node (Black, left, key, right)) in
              ())
         | Empty ->
           let _ = occurs_def query (Node (Black, left, key, right)) in
           ())

let balance_preserves (colour : color) (left : t @ logical) (key : int)
    (right : t @ logical) (query : int)
    : unit{
      occurs query (balance colour left key right)
      = (int_equal query key || occurs query left || occurs query right)
    }
  =
  let _ = balance_def colour left key right in
  match colour with
  | Red -> let _ = occurs_def query (Node (Red, left, key, right)) in ()
  | Black ->
    match left with
    | Empty -> rotate_right_side_preserves left key right query
    | Node (lc, ll, lk, lr) ->
      match lc with
      | Black -> rotate_right_side_preserves left key right query
      | Red ->
        match ll with
        | Node (llc, a, x, b) ->
          (match llc with
           | Red ->
             let _ = occurs_def query
               (Node (Red, Node (Black, a, x, b), lk,
                      Node (Black, lr, key, right))) in
             let _ = occurs_def query (Node (Black, a, x, b)) in
             let _ = occurs_def query (Node (Black, lr, key, right)) in
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
                            Node (Black, c, key, right))) in
                   let _ = occurs_def query (Node (Black, ll, lk, b)) in
                   let _ = occurs_def query (Node (Black, c, key, right)) in
                   let _ = occurs_def query (Node (Red, ll, lk, lr)) in
                   let _ = occurs_def query (Node (Red, b, y, c)) in
                   ()
                 | Black -> rotate_right_side_preserves left key right query)
              | Empty -> rotate_right_side_preserves left key right query))
        | Empty ->
          (match lr with
           | Node (lrc, b, y, c) ->
             (match lrc with
              | Red ->
                let _ = occurs_def query
                  (Node (Red, Node (Black, ll, lk, b), y,
                         Node (Black, c, key, right))) in
                let _ = occurs_def query (Node (Black, ll, lk, b)) in
                let _ = occurs_def query (Node (Black, c, key, right)) in
                let _ = occurs_def query (Node (Red, ll, lk, lr)) in
                let _ = occurs_def query (Node (Red, b, y, c)) in
                let _ = occurs_def query Empty in
                ()
              | Black -> rotate_right_side_preserves left key right query)
           | Empty -> rotate_right_side_preserves left key right query)

let rotate_right_side_below (left : t @ logical) (key : int)
    (right : t @ logical) (bound : int)
    : unit{
      below (rotate_right_side left key right) bound
      = (int_less key bound && below left bound && below right bound)
    }
  =
  let _ = rotate_right_side_def left key right in
  match right with
  | Empty -> let _ = below_def (Node (Black, left, key, right)) bound in ()
  | Node (rc, rl, rk, rr) ->
    match rc with
    | Black -> let _ = below_def (Node (Black, left, key, right)) bound in ()
    | Red ->
      match rl with
      | Node (rlc, b, y, c) ->
        (match rlc with
         | Red ->
           let _ = below_def
             (Node (Red, Node (Black, left, key, b), y,
                    Node (Black, c, rk, rr))) bound in
           let _ = below_def (Node (Black, left, key, b)) bound in
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
                   (Node (Red, Node (Black, left, key, rl), rk,
                          Node (Black, c, z, d))) bound in
                 let _ = below_def (Node (Black, left, key, rl)) bound in
                 let _ = below_def (Node (Black, c, z, d)) bound in
                 let _ = below_def (Node (Red, rl, rk, rr)) bound in
                 let _ = below_def (Node (Red, c, z, d)) bound in
                 ()
               | Black ->
                 let _ = below_def (Node (Black, left, key, right)) bound in
                 ())
            | Empty ->
              let _ = below_def (Node (Black, left, key, right)) bound in
              ()))
      | Empty ->
        (match rr with
         | Node (rrc, c, z, d) ->
           (match rrc with
            | Red ->
              let _ = below_def
                (Node (Red, Node (Black, left, key, rl), rk,
                       Node (Black, c, z, d))) bound in
              let _ = below_def (Node (Black, left, key, rl)) bound in
              let _ = below_def (Node (Black, c, z, d)) bound in
              let _ = below_def (Node (Red, rl, rk, rr)) bound in
              let _ = below_def (Node (Red, c, z, d)) bound in
              let _ = below_def Empty bound in
              ()
            | Black ->
              let _ = below_def (Node (Black, left, key, right)) bound in
              ())
         | Empty ->
           let _ = below_def (Node (Black, left, key, right)) bound in
           ())

let rotate_right_side_above (left : t @ logical) (key : int)
    (right : t @ logical) (bound : int)
    : unit{
      above (rotate_right_side left key right) bound
      = (int_less bound key && above left bound && above right bound)
    }
  =
  let _ = rotate_right_side_def left key right in
  match right with
  | Empty -> let _ = above_def (Node (Black, left, key, right)) bound in ()
  | Node (rc, rl, rk, rr) ->
    match rc with
    | Black -> let _ = above_def (Node (Black, left, key, right)) bound in ()
    | Red ->
      match rl with
      | Node (rlc, b, y, c) ->
        (match rlc with
         | Red ->
           let _ = above_def
             (Node (Red, Node (Black, left, key, b), y,
                    Node (Black, c, rk, rr))) bound in
           let _ = above_def (Node (Black, left, key, b)) bound in
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
                   (Node (Red, Node (Black, left, key, rl), rk,
                          Node (Black, c, z, d))) bound in
                 let _ = above_def (Node (Black, left, key, rl)) bound in
                 let _ = above_def (Node (Black, c, z, d)) bound in
                 let _ = above_def (Node (Red, rl, rk, rr)) bound in
                 let _ = above_def (Node (Red, c, z, d)) bound in
                 ()
               | Black ->
                 let _ = above_def (Node (Black, left, key, right)) bound in
                 ())
            | Empty ->
              let _ = above_def (Node (Black, left, key, right)) bound in
              ()))
      | Empty ->
        (match rr with
         | Node (rrc, c, z, d) ->
           (match rrc with
            | Red ->
              let _ = above_def
                (Node (Red, Node (Black, left, key, rl), rk,
                       Node (Black, c, z, d))) bound in
              let _ = above_def (Node (Black, left, key, rl)) bound in
              let _ = above_def (Node (Black, c, z, d)) bound in
              let _ = above_def (Node (Red, rl, rk, rr)) bound in
              let _ = above_def (Node (Red, c, z, d)) bound in
              let _ = above_def Empty bound in
              ()
            | Black ->
              let _ = above_def (Node (Black, left, key, right)) bound in
              ())
         | Empty ->
           let _ = above_def (Node (Black, left, key, right)) bound in
           ())

let balance_below (colour : color) (left : t @ logical) (key : int)
    (right : t @ logical) (bound : int)
    : unit{
      below (balance colour left key right) bound
      = (int_less key bound && below left bound && below right bound)
    }
  =
  let _ = balance_def colour left key right in
  match colour with
  | Red -> let _ = below_def (Node (Red, left, key, right)) bound in ()
  | Black ->
    match left with
    | Empty -> rotate_right_side_below left key right bound
    | Node (lc, ll, lk, lr) ->
      match lc with
      | Black -> rotate_right_side_below left key right bound
      | Red ->
        match ll with
        | Node (llc, a, x, b) ->
          (match llc with
           | Red ->
             let _ = below_def
               (Node (Red, Node (Black, a, x, b), lk,
                      Node (Black, lr, key, right))) bound in
             let _ = below_def (Node (Black, a, x, b)) bound in
             let _ = below_def (Node (Black, lr, key, right)) bound in
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
                            Node (Black, c, key, right))) bound in
                   let _ = below_def (Node (Black, ll, lk, b)) bound in
                   let _ = below_def (Node (Black, c, key, right)) bound in
                   let _ = below_def (Node (Red, ll, lk, lr)) bound in
                   let _ = below_def (Node (Red, b, y, c)) bound in
                   ()
                 | Black -> rotate_right_side_below left key right bound)
              | Empty -> rotate_right_side_below left key right bound))
        | Empty ->
          (match lr with
           | Node (lrc, b, y, c) ->
             (match lrc with
              | Red ->
                let _ = below_def
                  (Node (Red, Node (Black, ll, lk, b), y,
                         Node (Black, c, key, right))) bound in
                let _ = below_def (Node (Black, ll, lk, b)) bound in
                let _ = below_def (Node (Black, c, key, right)) bound in
                let _ = below_def (Node (Red, ll, lk, lr)) bound in
                let _ = below_def (Node (Red, b, y, c)) bound in
                let _ = below_def Empty bound in
                ()
              | Black -> rotate_right_side_below left key right bound)
           | Empty -> rotate_right_side_below left key right bound)

let balance_above (colour : color) (left : t @ logical) (key : int)
    (right : t @ logical) (bound : int)
    : unit{
      above (balance colour left key right) bound
      = (int_less bound key && above left bound && above right bound)
    }
  =
  let _ = balance_def colour left key right in
  match colour with
  | Red -> let _ = above_def (Node (Red, left, key, right)) bound in ()
  | Black ->
    match left with
    | Empty -> rotate_right_side_above left key right bound
    | Node (lc, ll, lk, lr) ->
      match lc with
      | Black -> rotate_right_side_above left key right bound
      | Red ->
        match ll with
        | Node (llc, a, x, b) ->
          (match llc with
           | Red ->
             let _ = above_def
               (Node (Red, Node (Black, a, x, b), lk,
                      Node (Black, lr, key, right))) bound in
             let _ = above_def (Node (Black, a, x, b)) bound in
             let _ = above_def (Node (Black, lr, key, right)) bound in
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
                            Node (Black, c, key, right))) bound in
                   let _ = above_def (Node (Black, ll, lk, b)) bound in
                   let _ = above_def (Node (Black, c, key, right)) bound in
                   let _ = above_def (Node (Red, ll, lk, lr)) bound in
                   let _ = above_def (Node (Red, b, y, c)) bound in
                   ()
                 | Black -> rotate_right_side_above left key right bound)
              | Empty -> rotate_right_side_above left key right bound))
        | Empty ->
          (match lr with
           | Node (lrc, b, y, c) ->
             (match lrc with
              | Red ->
                let _ = above_def
                  (Node (Red, Node (Black, ll, lk, b), y,
                         Node (Black, c, key, right))) bound in
                let _ = above_def (Node (Black, ll, lk, b)) bound in
                let _ = above_def (Node (Black, c, key, right)) bound in
                let _ = above_def (Node (Red, ll, lk, lr)) bound in
                let _ = above_def (Node (Red, b, y, c)) bound in
                let _ = above_def Empty bound in
                ()
              | Black -> rotate_right_side_above left key right bound)
           | Empty -> rotate_right_side_above left key right bound)

let rotate_right_side_ordered (left : t @ logical) (key : int)
    (right : t @ logical)
    (_ordered : unit{
       ordered left = true
       && ordered right = true
       && below left key = true
       && above right key = true
     })
    : unit{ ordered (rotate_right_side left key right) = true }
  =
  let _ = rotate_right_side_def left key right in
  match right with
  | Empty -> let _ = ordered_def (Node (Black, left, key, right)) in ()
  | Node (rc, rl, rk, rr) ->
    match rc with
    | Black -> let _ = ordered_def (Node (Black, left, key, right)) in ()
    | Red ->
      let _ = ordered_def (Node (Red, rl, rk, rr)) in
      let _ = above_def (Node (Red, rl, rk, rr)) key in
      (match rl with
       | Node (rlc, b, y, c) ->
         (match rlc with
          | Red ->
            let _ = ordered_def (Node (Red, b, y, c)) in
            let _ = above_def (Node (Red, b, y, c)) key in
            let _ = below_def (Node (Red, b, y, c)) rk in
            let _ = below_weaken y key left () in
            let _ = above_weaken y rk rr () in
            let _ = ordered_def
              (Node (Red, Node (Black, left, key, b), y,
                     Node (Black, c, rk, rr))) in
            let _ = ordered_def (Node (Black, left, key, b)) in
            let _ = ordered_def (Node (Black, c, rk, rr)) in
            let _ = below_def (Node (Black, left, key, b)) y in
            let _ = above_def (Node (Black, c, rk, rr)) y in
            ()
          | Black ->
            (match rr with
             | Node (rrc, c, z, d) ->
               (match rrc with
                | Red ->
                  let _ = ordered_def (Node (Red, c, z, d)) in
                  let _ = above_def (Node (Red, c, z, d)) rk in
                  let _ = below_weaken rk key left () in
                  let _ = ordered_def
                    (Node (Red, Node (Black, left, key, rl), rk,
                           Node (Black, c, z, d))) in
                  let _ = ordered_def (Node (Black, left, key, rl)) in
                  let _ = ordered_def (Node (Black, c, z, d)) in
                  let _ = below_def (Node (Black, left, key, rl)) rk in
                  let _ = above_def (Node (Black, c, z, d)) rk in
                  ()
                | Black ->
                  let _ = ordered_def (Node (Black, left, key, right)) in
                  ())
             | Empty ->
               let _ = ordered_def (Node (Black, left, key, right)) in
               ()))
       | Empty ->
         (match rr with
          | Node (rrc, c, z, d) ->
            (match rrc with
             | Red ->
               let _ = ordered_def (Node (Red, c, z, d)) in
               let _ = above_def (Node (Red, c, z, d)) rk in
               let _ = below_weaken rk key left () in
               let _ = ordered_def
                 (Node (Red, Node (Black, left, key, rl), rk,
                        Node (Black, c, z, d))) in
               let _ = ordered_def (Node (Black, left, key, rl)) in
               let _ = ordered_def (Node (Black, c, z, d)) in
               let _ = below_def (Node (Black, left, key, rl)) rk in
               let _ = above_def (Node (Black, c, z, d)) rk in
               ()
             | Black ->
               let _ = ordered_def (Node (Black, left, key, right)) in
               ())
          | Empty ->
            let _ = ordered_def (Node (Black, left, key, right)) in
            ()))

let balance_ordered (colour : color) (left : t @ logical) (key : int)
    (right : t @ logical)
    (_ordered : unit{
       ordered left = true
       && ordered right = true
       && below left key = true
       && above right key = true
     })
    : unit{ ordered (balance colour left key right) = true }
  =
  let _ = balance_def colour left key right in
  match colour with
  | Red -> let _ = ordered_def (Node (Red, left, key, right)) in ()
  | Black ->
    match left with
    | Empty -> rotate_right_side_ordered left key right ()
    | Node (lc, ll, lk, lr) ->
      match lc with
      | Black -> rotate_right_side_ordered left key right ()
      | Red ->
        let _ = ordered_def (Node (Red, ll, lk, lr)) in
        let _ = below_def (Node (Red, ll, lk, lr)) key in
        (match ll with
         | Node (llc, a, x, b) ->
           (match llc with
            | Red ->
              let _ = ordered_def (Node (Red, a, x, b)) in
              let _ = below_def (Node (Red, a, x, b)) lk in
              let _ = above_weaken lk key right () in
              let _ = ordered_def
                (Node (Red, Node (Black, a, x, b), lk,
                       Node (Black, lr, key, right))) in
              let _ = ordered_def (Node (Black, a, x, b)) in
              let _ = ordered_def (Node (Black, lr, key, right)) in
              let _ = below_def (Node (Black, a, x, b)) lk in
              let _ = above_def (Node (Black, lr, key, right)) lk in
              ()
            | Black ->
              (match lr with
               | Node (lrc, b, y, c) ->
                 (match lrc with
                  | Red ->
                    let _ = ordered_def (Node (Red, b, y, c)) in
                    let _ = above_def (Node (Red, b, y, c)) lk in
                    let _ = below_def (Node (Red, b, y, c)) key in
                    let _ = below_weaken y lk ll () in
                    let _ = above_weaken y key right () in
                    let _ = ordered_def
                      (Node (Red, Node (Black, ll, lk, b), y,
                             Node (Black, c, key, right))) in
                    let _ = ordered_def (Node (Black, ll, lk, b)) in
                    let _ = ordered_def (Node (Black, c, key, right)) in
                    let _ = below_def (Node (Black, ll, lk, b)) y in
                    let _ = above_def (Node (Black, c, key, right)) y in
                    ()
                  | Black -> rotate_right_side_ordered left key right ())
               | Empty -> rotate_right_side_ordered left key right ()))
         | Empty ->
           (match lr with
            | Node (lrc, b, y, c) ->
              (match lrc with
               | Red ->
                 let _ = ordered_def (Node (Red, b, y, c)) in
                 let _ = above_def (Node (Red, b, y, c)) lk in
                 let _ = below_def (Node (Red, b, y, c)) key in
                 let _ = below_weaken y lk ll () in
                 let _ = above_weaken y key right () in
                 let _ = ordered_def
                   (Node (Red, Node (Black, ll, lk, b), y,
                          Node (Black, c, key, right))) in
                 let _ = ordered_def (Node (Black, ll, lk, b)) in
                 let _ = ordered_def (Node (Black, c, key, right)) in
                 let _ = below_def (Node (Black, ll, lk, b)) y in
                 let _ = above_def (Node (Black, c, key, right)) y in
                 ()
               | Black -> rotate_right_side_ordered left key right ())
            | Empty -> rotate_right_side_ordered left key right ()))

(* An existing key is returned unchanged.  Without this case [ins] appends a
   second copy of the key on the right spine, which breaks the ordering
   invariant and makes the structure not a set. *)
let[@vox.def] rec ins (new_key : int) (tree : t @ logical) : t =
  match tree with
  | Empty -> Node (Red, Empty, new_key, Empty)
  | Node (c, l, k, r) ->
    if int_equal new_key k
    then tree
    else if int_less new_key k
    then balance c (ins new_key l) k r
    else balance c l k (ins new_key r)

let rec ins_preserves (new_key : int) (tree : t @ logical) (query : int)
    : unit{
      occurs query (ins new_key tree)
      = (int_equal query new_key || occurs query tree)
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

let rec ins_below (bound : int) (new_key : int{ _ < bound })
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

let rec ins_above (bound : int) (new_key : int{ bound < _ })
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

let rec ins_ordered (new_key : int) (tree : t @ logical)
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
      ins_ordered new_key r ();
      ins_above k new_key r ();
      balance_ordered c l k (ins new_key r) ();
      ()

let[@vox.def] blacken (tree : t @ logical) : t =
  match tree with
  | Empty -> Empty
  | Node (_c, l, k, r) -> Node (Black, l, k, r)

let blacken_preserves (tree : t @ logical) (query : int)
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

let empty = Empty

let[@vox.def] insert (new_key : int) (tree : t @ logical) : t =
  blacken (ins new_key tree)

let insert_preserves (new_key : int) (tree : t @ logical) (query : int)
    : unit{
      occurs query (insert new_key tree)
      = (int_equal query new_key || occurs query tree)
    }
  =
  let _ = insert_def new_key tree in
  let _ = blacken_preserves (ins new_key tree) query in
  let _ = ins_preserves new_key tree query in
  ()

let insert_ordered (new_key : int) (tree : t @ logical)
    (_ordered : unit{ ordered tree = true })
    : unit{ ordered (insert new_key tree) = true } =
  insert_def new_key tree;
  ins_ordered new_key tree ();
  blacken_ordered (ins new_key tree) ();
  ()

let[@vox.def] invariant (tree : t @ logical) = ordered tree

let empty_law ~(query : int) : unit{ member query empty = false } =
  let _ = member_def query empty in
  ()

let empty_invariant : unit{ invariant empty = true } =
  invariant_def empty;
  ordered_def empty;
  ()

let insert_invariant ~(inserted : int) ~(tree : t @ logical)
    ~(well_formed : unit{ invariant tree = true })
    : unit{ invariant (insert inserted tree) = true } =
  invariant_def tree;
  invariant_def (insert inserted tree);
  insert_ordered inserted tree ();
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
  insert_preserves inserted tree query;
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
  | Empty -> true
  | Node (_colour, left, key, right) ->
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
    ~(colour : color) ~(left : t @ logical) ~(key : int)
    ~(right : t @ logical)
    ~proof:(_proof : unit{
       agrees t1 t2 (Node (colour, left, key, right)) = true
     })
    : unit{
      member key t1 = member key t2
      && agrees t1 t2 left = true
      && agrees t1 t2 right = true
    } =
  agrees_def t1 t2 (Node (colour, left, key, right));
  ()

let rec agrees_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(nodes : t @ logical) ~(query : int)
    ~(agreement : unit{ agrees t1 t2 nodes = true })
    ~(present : unit{ member query nodes = true })
    : unit{ member query t1 = member query t2 } =
  match nodes with
  | Empty ->
    member_def query Empty;
    ()
  | Node (colour, left, key, right) ->
    let facts =
      agrees_node ~t1 ~t2 ~colour ~left ~key ~right ~proof:agreement
    in
    member_def query (Node (colour, left, key, right));
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
    | Empty ->
      agrees_def t1 t2 Empty;
      ()
    | Node (colour, left, key, right) ->
      pointwise ~query:key;
      prove left;
      prove right;
      agrees_def t1 t2 (Node (colour, left, key, right));
      ()
  in
  prove t1;
  prove t2;
  equal_def t1 t2;
  ()
