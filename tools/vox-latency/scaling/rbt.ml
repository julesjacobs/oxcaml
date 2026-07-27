type color =
  | Red
  | Black

type t =
  | Empty
  | Node of color * t * int * t

external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"
let[@vox.def] rec member (query : int) (tree : t @ logical) =
  match tree with
  | Empty -> false
  | Node (_colour, left, key, right) ->
    if int_equal query key
    then true
    else if member query left then true else member query right

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
      member query (rotate_right_side left key right)
      = (int_equal query key || member query left || member query right)
    }
  =
  let _ = rotate_right_side_def left key right in
  match right with
  | Empty -> let _ = member_def query (Node (Black, left, key, right)) in ()
  | Node (rc, rl, rk, rr) ->
    match rc with
    | Black -> let _ = member_def query (Node (Black, left, key, right)) in ()
    | Red ->
      match rl with
      | Node (rlc, b, y, c) ->
        (match rlc with
         | Red ->
           let _ = member_def query
             (Node (Red, Node (Black, left, key, b), y,
                    Node (Black, c, rk, rr))) in
           let _ = member_def query (Node (Black, left, key, b)) in
           let _ = member_def query (Node (Black, c, rk, rr)) in
           let _ = member_def query (Node (Red, rl, rk, rr)) in
           let _ = member_def query (Node (Red, b, y, c)) in
           ()
         | Black ->
           (match rr with
            | Node (rrc, c, z, d) ->
              (match rrc with
               | Red ->
                 let _ = member_def query
                   (Node (Red, Node (Black, left, key, rl), rk,
                          Node (Black, c, z, d))) in
                 let _ = member_def query (Node (Black, left, key, rl)) in
                 let _ = member_def query (Node (Black, c, z, d)) in
                 let _ = member_def query (Node (Red, rl, rk, rr)) in
                 let _ = member_def query (Node (Red, c, z, d)) in
                 ()
               | Black ->
                 let _ =
                   member_def query (Node (Black, left, key, right))
                 in
                 ())
            | Empty ->
              let _ = member_def query (Node (Black, left, key, right)) in
              ()))
      | Empty ->
        (match rr with
         | Node (rrc, c, z, d) ->
           (match rrc with
            | Red ->
              let _ = member_def query
                (Node (Red, Node (Black, left, key, rl), rk,
                       Node (Black, c, z, d))) in
              let _ = member_def query (Node (Black, left, key, rl)) in
              let _ = member_def query (Node (Black, c, z, d)) in
              let _ = member_def query (Node (Red, rl, rk, rr)) in
              let _ = member_def query (Node (Red, c, z, d)) in
              ()
            | Black ->
              let _ = member_def query (Node (Black, left, key, right)) in
              ())
         | Empty ->
           let _ = member_def query (Node (Black, left, key, right)) in
           ())

let balance_preserves (colour : color) (left : t @ logical) (key : int)
    (right : t @ logical) (query : int)
    : unit{
      member query (balance colour left key right)
      = (int_equal query key || member query left || member query right)
    }
  =
  let _ = balance_def colour left key right in
  match colour with
  | Red -> let _ = member_def query (Node (Red, left, key, right)) in ()
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
             let _ = member_def query
               (Node (Red, Node (Black, a, x, b), lk,
                      Node (Black, lr, key, right))) in
             let _ = member_def query (Node (Black, a, x, b)) in
             let _ = member_def query (Node (Black, lr, key, right)) in
             let _ = member_def query (Node (Red, ll, lk, lr)) in
             let _ = member_def query (Node (Red, a, x, b)) in
             ()
           | Black ->
             (match lr with
              | Node (lrc, b, y, c) ->
                (match lrc with
                 | Red ->
                   let _ = member_def query
                     (Node (Red, Node (Black, ll, lk, b), y,
                            Node (Black, c, key, right))) in
                   let _ = member_def query (Node (Black, ll, lk, b)) in
                   let _ = member_def query (Node (Black, c, key, right)) in
                   let _ = member_def query (Node (Red, ll, lk, lr)) in
                   let _ = member_def query (Node (Red, b, y, c)) in
                   ()
                 | Black -> rotate_right_side_preserves left key right query)
              | Empty -> rotate_right_side_preserves left key right query))
        | Empty ->
          (match lr with
           | Node (lrc, b, y, c) ->
             (match lrc with
              | Red ->
                let _ = member_def query
                  (Node (Red, Node (Black, ll, lk, b), y,
                         Node (Black, c, key, right))) in
                let _ = member_def query (Node (Black, ll, lk, b)) in
                let _ = member_def query (Node (Black, c, key, right)) in
                let _ = member_def query (Node (Red, ll, lk, lr)) in
                let _ = member_def query (Node (Red, b, y, c)) in
                ()
              | Black -> rotate_right_side_preserves left key right query)
           | Empty -> rotate_right_side_preserves left key right query)

let[@vox.def] rec ins (new_key : int) (tree : t @ logical) : t =
  match tree with
  | Empty -> Node (Red, Empty, new_key, Empty)
  | Node (c, l, k, r) ->
    if int_less new_key k
    then balance c (ins new_key l) k r
    else balance c l k (ins new_key r)

let rec ins_preserves (new_key : int) (tree : t @ logical) (query : int)
    : unit{
      member query (ins new_key tree)
      = (int_equal query new_key || member query tree)
    }
  =
  match tree with
  | Empty ->
    let _ = ins_def new_key Empty in
    let _ = member_def query (Node (Red, Empty, new_key, Empty)) in
    let _ = member_def query Empty in
    ()
  | Node (c, l, k, r) ->
    let _ = ins_def new_key (Node (c, l, k, r)) in
    let _ = member_def query (Node (c, l, k, r)) in
    let _ = balance_preserves c (ins new_key l) k r query in
    let _ = balance_preserves c l k (ins new_key r) query in
    let _ = ins_preserves new_key l query in
    let _ = ins_preserves new_key r query in
    ()

let[@vox.def] blacken (tree : t @ logical) : t =
  match tree with
  | Empty -> Empty
  | Node (_c, l, k, r) -> Node (Black, l, k, r)

let blacken_preserves (tree : t @ logical) (query : int)
    : unit{ member query (blacken tree) = member query tree } =
  let _ = blacken_def tree in
  match tree with
  | Empty ->
    let _ = member_def query Empty in
    ()
  | Node (c, l, k, r) ->
    let _ = member_def query (Node (Black, l, k, r)) in
    let _ = member_def query (Node (c, l, k, r)) in
    ()

let empty = Empty

let[@vox.def] insert (new_key : int) (tree : t @ logical) : t =
  blacken (ins new_key tree)

let insert_preserves (new_key : int) (tree : t @ logical) (query : int)
    : unit{
      member query (insert new_key tree)
      = (int_equal query new_key || member query tree)
    }
  =
  let _ = insert_def new_key tree in
  let _ = blacken_preserves (ins new_key tree) query in
  let _ = ins_preserves new_key tree query in
  ()

let empty_law ~(query : int) : unit{ member query empty = false } =
  let _ = member_def query empty in
  ()

let insert_law ~(inserted : int) ~(tree : t @ logical) ~(query : int)
    : unit{
      member query (insert inserted tree)
      = ((query = inserted) || member query tree)
    }
  =
  insert_preserves inserted tree query

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
  let _definition =
    agrees_def t1 t2 (Node (colour, left, key, right))
  in
  let first_member = member key t1 in
  let second_member = member key t2 in
  if first_member
  then
    if second_member
    then if agrees t1 t2 left then () else ()
    else ()
  else if second_member
  then ()
  else if agrees t1 t2 left then () else ()

let rec agrees_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(nodes : t @ logical) ~(query : int)
    ~(agreement : unit{ agrees t1 t2 nodes = true })
    : unit{
      member query nodes = false
      || member query t1 = member query t2
    } =
  match nodes with
  | Empty ->
    let _member = member_def query Empty in
    ()
  | Node (colour, left, key, right) ->
    let facts =
      agrees_node ~t1 ~t2 ~colour ~left ~key ~right ~proof:agreement
    in
    let _member = member_def query (Node (colour, left, key, right)) in
    let _left =
      agrees_member ~t1 ~t2 ~nodes:left ~query ~agreement:facts
    in
    let _right =
      agrees_member ~t1 ~t2 ~nodes:right ~query ~agreement:facts
    in
    if int_equal query key
    then ()
    else if member query left then () else ()

let present_agrees_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(nodes : t @ logical) ~(query : int)
    ~(agreement : unit{ agrees t1 t2 nodes = true })
    ~(present : unit{ member query nodes = true })
    : unit{ member query t1 = member query t2 } =
  let _implication =
    agrees_member ~t1 ~t2 ~nodes ~query ~agreement
  in
  ()

let prove_equal_member ~(t1 : t @ logical)
    ~(t2 : t{ equal t1 _ = true } @ logical)
    ~(query : int)
    : unit{ member query t1 = member query t2 } =
  let _definition = equal_def t1 t2 in
  let first_member = member query t1 in
  let second_member = member query t2 in
  let side = membership_side first_member second_member in
  let _side = membership_side_def first_member second_member in
  match side with
  | First ->
    present_agrees_member ~t1 ~t2 ~nodes:t1 ~query
      ~agreement:() ~present:()
  | Second ->
    present_agrees_member ~t1 ~t2 ~nodes:t2 ~query
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
      let _definition = agrees_def t1 t2 Empty in
      ()
    | Node (colour, left, key, right) ->
      let _same_membership = pointwise ~query:key in
      let _left = prove left in
      let _right = prove right in
      let _definition =
        agrees_def t1 t2 (Node (colour, left, key, right))
      in
      let first_member = member key t1 in
      let second_member = member key t2 in
      if first_member
      then ()
      else if second_member then () else ()
  in
  let _first = prove t1 in
  let _second = prove t2 in
  let _definition = equal_def t1 t2 in
  ()
