type t =
  | Empty
  | Node of t * int * t

external int_equal : int -> int -> bool @@ total = "%equal"
external int_less : int -> int -> bool @@ total = "%lessthan"

type direction =
  | Same
  | Left
  | Right

type membership_side =
  | First
  | Second
  | Neither

let[@vox.def] direction new_key key =
  if int_equal new_key key
  then Same
  else if int_less new_key key
  then Left
  else Right

let[@vox.def] membership_side first_member second_member =
  if first_member
  then First
  else if second_member then Second else Neither

let empty = (Empty : t{ _ = Empty })

let[@vox.def] rec member (query : int) (tree : t @ logical) =
  match tree with
  | Empty -> false
  | Node (left, key, right) ->
    if int_equal query key
    then true
    else if int_less query key
    then member query left
    else member query right

let[@vox.def] rec agrees (t1 : t @ logical) (t2 : t @ logical)
    (nodes : t @ logical) =
  match nodes with
  | Empty -> true
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

let[@vox.def] rec insert (new_key : int) (tree : t @ logical)
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

let empty_law ~(key : int)
    : unit{ member key empty = false } =
  prove_member_empty key

let insert_law ~(inserted : int)
    ~(tree : t @ logical) ~(query : int)
    : unit{
      member query (insert inserted tree)
      = ((inserted = query) || member query tree)
    }
  =
  let _membership = member_insert inserted tree query in
  if int_equal query inserted then () else ()

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

let finish_equal_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(query : int)
    ~proof:(_proof : unit{ member query t1 = member query t2 })
    : unit{ member query t1 = member query t2 } =
  ()

let rec agrees_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(nodes : t @ logical) ~(query : int)
    ~(agreement : unit{ agrees t1 t2 nodes = true })
    ~(present : unit{ member query nodes = true })
    : unit{ member query t1 = member query t2 } =
  match nodes with
  | Empty ->
    let _member = member_def query Empty in
    ()
  | Node (left, key, right) ->
    let facts =
      agrees_node ~t1 ~t2 ~left ~key ~right ~proof:agreement
    in
    let _member = member_def query (Node (left, key, right)) in
    let choice = direction query key in
    let _choice = direction_def query key in
    match choice with
    | Same -> finish_equal_member ~t1 ~t2 ~query ~proof:facts
    | Left ->
      finish_equal_member ~t1 ~t2 ~query
        ~proof:(agrees_member ~t1 ~t2 ~nodes:left ~query
                  ~agreement:facts ~present)
    | Right ->
      finish_equal_member ~t1 ~t2 ~query
        ~proof:(agrees_member ~t1 ~t2 ~nodes:right ~query
                  ~agreement:facts ~present)

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
    finish_equal_member ~t1 ~t2 ~query
      ~proof:(agrees_member ~t1 ~t2 ~nodes:t1 ~query
                ~agreement:() ~present:())
  | Second ->
    finish_equal_member ~t1 ~t2 ~query
      ~proof:(agrees_member ~t1 ~t2 ~nodes:t2 ~query
                ~agreement:() ~present:())
  | Neither -> finish_equal_member ~t1 ~t2 ~query ~proof:()

let finish_equal_implication ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(query : int)
    ~proof:(_proof : unit{
      equal t1 t2 = false || member query t1 = member query t2
    })
    : unit{
      equal t1 t2 = false || member query t1 = member query t2
    } =
  ()

let equal_implies_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(query : int)
    : unit{
      equal t1 t2 = false || member query t1 = member query t2
    } =
  let equality = equal t1 t2 in
  let side = membership_side equality false in
  let _side = membership_side_def equality false in
  match side with
  | First ->
    let _member = prove_equal_member ~t1 ~t2 ~query in
    finish_equal_implication ~t1 ~t2 ~query ~proof:()
  | Second -> finish_equal_implication ~t1 ~t2 ~query ~proof:()
  | Neither -> finish_equal_implication ~t1 ~t2 ~query ~proof:()

let members_imply_equal ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(witness : q:int ->
                 unit{ member q t1 = member q t2 })
    : unit{ equal t1 t2 = true } =
  let rec prove nodes : unit{ agrees t1 t2 nodes = true } =
    match nodes with
    | Empty ->
      let _definition = agrees_def t1 t2 Empty in
      ()
    | Node (left, key, right) ->
      let _same_membership = witness ~q:key in
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
