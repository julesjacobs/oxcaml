type t =
  | Leaf
  | Node of t * int * t

external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"
external int_leq : int -> int -> bool @@ total = "%lessequal"
external int_add : int -> int -> int @@ total = "%addint"
external int_sub : int -> int -> int @@ total = "%subint"

(* Membership traverses the full tree, so the shared SET laws do not assume
   ordering.  Structural-invariant theorems for ordering and height balance
   are not part of this example. *)

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

let[@vox.def] rec member (query : int) (tree : t @ logical) =
  match tree with
  | Leaf -> false
  | Node (left, key, right) ->
    int_equal query key || member query left || member query right

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

let rotate_right_preserves_member (tree : t @ logical) (query : int)
    : unit{ member query (rotate_right tree) = member query tree }
  =
  let _ = rotate_right_def tree in
  match tree with
  | Leaf -> ()
  | Node (l, y, c) ->
    let _ = member_def query (Node (l, y, c)) in
    match l with
    | Leaf -> ()
    | Node (a, x, b) ->
      let _ = member_def query (Node (a, x, b)) in
      let _ = member_def query (Node (a, x, Node (b, y, c))) in
      let _ = member_def query (Node (b, y, c)) in
      ()

let rotate_left_preserves_member (tree : t @ logical) (query : int)
    : unit{ member query (rotate_left tree) = member query tree }
  =
  let _ = rotate_left_def tree in
  match tree with
  | Leaf -> ()
  | Node (a, x, r) ->
    let _ = member_def query (Node (a, x, r)) in
    match r with
    | Leaf -> ()
    | Node (b, y, c) ->
      let _ = member_def query (Node (b, y, c)) in
      let _ = member_def query (Node (Node (a, x, b), y, c)) in
      let _ = member_def query (Node (a, x, b)) in
      ()

let node_member_congruence_left (l1 : t @ logical) (l2 : t @ logical)
    (k : int) (r : t @ logical) (query : int)
    (_eq : unit{ member query l1 = member query l2 })
    : unit{
      member query (Node (l1, k, r)) = member query (Node (l2, k, r))
    }
  =
  let _ = member_def query (Node (l1, k, r)) in
  let _ = member_def query (Node (l2, k, r)) in
  ()

let node_member_congruence_right (l : t @ logical) (k : int)
    (r1 : t @ logical) (r2 : t @ logical) (query : int)
    (_eq : unit{ member query r1 = member query r2 })
    : unit{
      member query (Node (l, k, r1)) = member query (Node (l, k, r2))
    }
  =
  let _ = member_def query (Node (l, k, r1)) in
  let _ = member_def query (Node (l, k, r2)) in
  ()

let[@vox.def] rotate_left_right (tree : t @ logical) : t =
  match tree with
  | Leaf -> tree
  | Node (l, k, r) -> rotate_right (Node (rotate_left l, k, r))

let[@vox.def] rotate_right_left (tree : t @ logical) : t =
  match tree with
  | Leaf -> tree
  | Node (l, k, r) -> rotate_left (Node (l, k, rotate_right r))

let rotate_left_right_preserves_member (tree : t @ logical) (query : int)
    : unit{ member query (rotate_left_right tree) = member query tree }
  =
  let _ = rotate_left_right_def tree in
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    let rl = rotate_left_preserves_member l query in
    let _ = node_member_congruence_left (rotate_left l) l k r query rl in
    let _ =
      rotate_right_preserves_member (Node (rotate_left l, k, r)) query
    in
    ()

let rotate_right_left_preserves_member (tree : t @ logical) (query : int)
    : unit{ member query (rotate_right_left tree) = member query tree }
  =
  let _ = rotate_right_left_def tree in
  match tree with
  | Leaf -> ()
  | Node (l, k, r) ->
    let rr = rotate_right_preserves_member r query in
    let _ = node_member_congruence_right l k (rotate_right r) r query rr in
    let _ =
      rotate_left_preserves_member (Node (l, k, rotate_right r)) query
    in
    ()

let max_height (a : int) (b : int) : int =
  if int_less a b then b else a

let rec height (tree : t @ logical) : int =
  match tree with
  | Leaf -> 0
  | Node (l, _, r) -> int_add 1 (max_height (height l) (height r))

type rebalance_action =
  | No_rotation
  | Rotate_ll
  | Rotate_lr
  | Rotate_rr
  | Rotate_rl

let choose_action (tree : t @ logical) : rebalance_action =
  match tree with
  | Leaf -> No_rotation
  | Node (l, _, r) ->
    let bf = int_sub (height l) (height r) in
    if int_less 1 bf
    then
      (match l with
       | Leaf -> No_rotation
       | Node (ll, _, lr) ->
         if int_leq (height lr) (height ll) then Rotate_ll else Rotate_lr)
    else if int_less bf (-1)
    then
      (match r with
       | Leaf -> No_rotation
       | Node (rl, _, rr) ->
         if int_leq (height rl) (height rr) then Rotate_rr else Rotate_rl)
    else No_rotation

let[@vox.def] apply_action (action : rebalance_action)
    (tree : t @ logical) : t =
  match action with
  | No_rotation -> tree
  | Rotate_ll -> rotate_right tree
  | Rotate_lr -> rotate_left_right tree
  | Rotate_rr -> rotate_left tree
  | Rotate_rl -> rotate_right_left tree

let[@vox.def] rebalance (tree : t @ logical) : t =
  apply_action (choose_action tree) tree

let rebalance_preserves_member (tree : t @ logical) (query : int)
    : unit{ member query (rebalance tree) = member query tree }
  =
  let _ = rebalance_def tree in
  let action = choose_action tree in
  let _ = apply_action_def action tree in
  match action with
  | No_rotation -> ()
  | Rotate_ll -> rotate_right_preserves_member tree query
  | Rotate_lr -> rotate_left_right_preserves_member tree query
  | Rotate_rr -> rotate_left_preserves_member tree query
  | Rotate_rl -> rotate_right_left_preserves_member tree query

let[@vox.def] rec insert (new_key : int) (tree : t @ logical) : t =
  match tree with
  | Leaf -> Node (Leaf, new_key, Leaf)
  | Node (left, key, right) ->
    if int_equal new_key key
    then tree
    else if int_less new_key key
    then rebalance (Node (insert new_key left, key, right))
    else rebalance (Node (left, key, insert new_key right))

let member_insert_leaf (new_key : int) (query : int)
    : unit{
      member query (insert new_key Leaf)
      = (query = new_key || member query Leaf)
    }
  =
  let _ = insert_def new_key Leaf in
  let _ = member_def query (Node (Leaf, new_key, Leaf)) in
  let _ = member_def query Leaf in
  ()

let member_insert_same (key : int) (new_key : int{ _ = key })
    (left : t @ logical) (right : t @ logical) (query : int)
    : unit{
      member query (insert new_key (Node (left, key, right)))
      = (query = new_key || member query (Node (left, key, right)))
    }
  =
  let _ = insert_def new_key (Node (left, key, right)) in
  let _ = member_def query (Node (left, key, right)) in
  ()

let member_insert_left (key : int) (new_key : int{ _ < key })
    (left : t @ logical) (right : t @ logical) (query : int)
    (_ih : unit{
       member query (insert new_key left)
       = (query = new_key || member query left)
     })
    : unit{
      member query (insert new_key (Node (left, key, right)))
      = (query = new_key || member query (Node (left, key, right)))
    }
  =
  let _ = insert_def new_key (Node (left, key, right)) in
  let _ =
    rebalance_preserves_member
      (Node (insert new_key left, key, right)) query
  in
  let _ = member_def query (Node (insert new_key left, key, right)) in
  let _ = member_def query (Node (left, key, right)) in
  ()

let member_insert_right (key : int)
    (new_key : int{ _ <> key && not (_ < key) })
    (left : t @ logical) (right : t @ logical) (query : int)
    (_ih : unit{
       member query (insert new_key right)
       = (query = new_key || member query right)
     })
    : unit{
      member query (insert new_key (Node (left, key, right)))
      = (query = new_key || member query (Node (left, key, right)))
    }
  =
  let _ = insert_def new_key (Node (left, key, right)) in
  let _ =
    rebalance_preserves_member
      (Node (left, key, insert new_key right)) query
  in
  let _ = member_def query (Node (left, key, insert new_key right)) in
  let _ = member_def query (Node (left, key, right)) in
  ()

let rec member_insert (new_key : int) (tree : t @ logical) (query : int)
    : unit{
      member query (insert new_key tree)
      = (query = new_key || member query tree)
    }
  =
  match tree with
  | Leaf -> member_insert_leaf new_key query
  | Node (left, key, right) ->
    let choice = direction new_key key in
    let _choice = direction_def new_key key in
    match choice with
    | Same -> member_insert_same key new_key left right query
    | Left ->
      let ih = member_insert new_key left query in
      member_insert_left key new_key left right query ih
    | Right ->
      let ih = member_insert new_key right query in
      member_insert_right key new_key left right query ih

let empty_law ~(query : int) : unit{ member query empty = false } =
  member_def query empty

let insert_law ~(inserted : int) ~(tree : t @ logical) ~(query : int)
    : unit{
      member query (insert inserted tree)
      = ((query = inserted) || member query tree)
    }
  =
  member_insert inserted tree query

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
  let _definition = agrees_def t1 t2 (Node (left, key, right)) in
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
  | Leaf ->
    let _member = member_def query Leaf in
    ()
  | Node (left, key, right) ->
    let facts =
      agrees_node ~t1 ~t2 ~left ~key ~right ~proof:agreement
    in
    let _member = member_def query (Node (left, key, right)) in
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
    | Leaf ->
      let _definition = agrees_def t1 t2 Leaf in
      ()
    | Node (left, key, right) ->
      let _same_membership = pointwise ~query:key in
      let _left = prove left in
      let _right = prove right in
      let _definition =
        agrees_def t1 t2 (Node (left, key, right))
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
