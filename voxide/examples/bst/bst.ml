type t =
  | Empty
  | Node of t * int * t

external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"

type direction =
  | Same
  | Left
  | Right

let[@vox.def] direction new_key key =
  if int_equal new_key key
  then Same
  else if int_less new_key key
  then Left
  else Right

let empty = (Empty : t{ _ = Empty })

let[@vox.def] rec member (query : int @ logical) (tree : t @ logical)
    : bool{ _ = true || _ = false }
  =
  match tree with
  | Empty -> false
  | Node (left, key, right) ->
    if int_equal query key
    then true
    else if int_less query key
    then member query left
    else member query right

let[@vox.def] rec insert (new_key : int @ logical) (tree : t @ logical)
    : t{ _ <> Empty }
  =
  match tree with
  | Empty -> Node (Empty, new_key, Empty)
  | Node (left, key, right) ->
    if int_equal new_key key
    then tree
    else if int_less new_key key
    then Node (insert new_key left, key, right)
    else Node (left, key, insert new_key right)

let prove_member_empty query : unit{ member query empty = false } =
  let _proof = member_def query empty in
  ()

let insert_empty new_key
    : unit{
      insert new_key Empty = Node (Empty, new_key, Empty)
    }
  =
  let _proof = insert_def new_key Empty in
  ()

let insert_same key (new_key : int{ _ = key }) left right
    : unit{
      insert new_key (Node (left, key, right)) = Node (left, key, right)
    }
  =
  let _proof = insert_def new_key (Node (left, key, right)) in
  ()

let insert_left key (new_key : int{ _ < key }) left right
    : unit{
      insert new_key (Node (left, key, right))
      = Node (insert new_key left, key, right)
    }
  =
  let _proof = insert_def new_key (Node (left, key, right)) in
  ()

let insert_right key
    (new_key : int{ _ <> key && not (_ < key) }) left right
    : unit{
      insert new_key (Node (left, key, right))
      = Node (left, key, insert new_key right)
    }
  =
  let _proof = insert_def new_key (Node (left, key, right)) in
  ()

let member_empty_node query : unit{ member query Empty = false } =
  let _proof = member_def query Empty in
  ()

let member_node query left key right
    : unit{
      member query (Node (left, key, right))
      = if query = key
        then true
        else if query < key
        then member query left
        else member query right
    }
  =
  let _proof = member_def query (Node (left, key, right)) in
  ()

let member_insert_empty new_key query
    : unit{
      member query (insert new_key Empty)
      = if query = new_key then true else member query Empty
    }
  =
  let _insert = insert_empty new_key in
  let _old_member = member_empty_node query in
  let _new_member = member_node query Empty new_key Empty in
  if int_equal query new_key
  then ()
  else if int_less query new_key
  then ()
  else ()

let member_insert_same key (new_key : int{ _ = key }) left right query
    : unit{
      member query (insert new_key (Node (left, key, right)))
      = if query = new_key
        then true
        else member query (Node (left, key, right))
    }
  =
  let _insert = insert_same key new_key left right in
  let _member = member_node query left key right in
  if int_equal query key
  then ()
  else ()

let member_insert_left key (new_key : int{ _ < key }) left right query
    (_induction : unit{
       member query (insert new_key left)
       = if query = new_key then true else member query left
     })
    : unit{
      member query (insert new_key (Node (left, key, right)))
      = if query = new_key
        then true
        else member query (Node (left, key, right))
    }
  =
  let _insert = insert_left key new_key left right in
  let _old_member = member_node query left key right in
  let _new_member = member_node query (insert new_key left) key right in
  if int_equal query key
  then ()
  else if int_less query key
  then ()
  else ()

let member_insert_right key
    (new_key : int{ _ <> key && not (_ < key) }) left right query
    (_induction : unit{
       member query (insert new_key right)
       = if query = new_key then true else member query right
     })
    : unit{
      member query (insert new_key (Node (left, key, right)))
      = if query = new_key
        then true
        else member query (Node (left, key, right))
    }
  =
  let _insert = insert_right key new_key left right in
  let _old_member = member_node query left key right in
  let _new_member = member_node query left key (insert new_key right) in
  if int_equal query key
  then ()
  else if int_less query key
  then ()
  else ()

let finish_member_insert new_key tree query
    (_proof : unit{
       member query (insert new_key tree)
       = if query = new_key then true else member query tree
     })
    : unit{
      member query (insert new_key tree)
      = if query = new_key then true else member query tree
    }
  =
  ()

let rec member_insert new_key tree query
    : unit{
      member query (insert new_key tree)
      = if query = new_key then true else member query tree
    }
  =
  match tree with
  | Empty ->
    finish_member_insert new_key tree query
      (member_insert_empty new_key query)
  | Node (left, key, right) ->
    let choice = direction new_key key in
    let _choice = direction_def new_key key in
    match choice with
    | Same ->
      finish_member_insert new_key tree query
        (member_insert_same key new_key left right query)
    | Left ->
      let induction = member_insert new_key left query in
      finish_member_insert new_key tree query
        (member_insert_left key new_key left right query induction)
    | Right ->
      let induction = member_insert new_key right query in
      finish_member_insert new_key tree query
        (member_insert_right key new_key left right query induction)

let empty_has_no_zero (_ : unit @ logical)
    : unit{ member 0 empty = false } =
  prove_member_empty 0

let empty_has_no_one (_ : unit @ logical)
    : unit{ member 1 empty = false } =
  prove_member_empty 1

let insert_zero_has_zero (_ : unit @ logical)
    : unit{ member 0 (insert 0 empty) = true } =
  member_insert 0 empty 0

let insert_zero_has_no_one (_ : unit @ logical)
    : unit{ member 1 (insert 0 empty) = false } =
  let _insert = member_insert 0 empty 1 in
  let _empty = prove_member_empty 1 in
  ()

let insert_one_preserves_zero (_ : unit @ logical)
    : unit{ member 0 (insert 1 (insert 0 empty)) = true }
  =
  let _second_insert = member_insert 1 (insert 0 empty) 0 in
  let _first_insert = member_insert 0 empty 0 in
  ()
