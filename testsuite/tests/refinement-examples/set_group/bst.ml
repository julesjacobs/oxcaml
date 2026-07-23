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

let empty = Empty

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
  member_def query (Node (left, key, right));
  ()

let member_insert_empty new_key query
    : unit{
      member query (insert new_key Empty)
      = if query = new_key then true else member query Empty
  }
  =
  insert_def new_key Empty;
  member_def query Empty;
  member_node query Empty new_key Empty;
  ()

let member_insert_same key (new_key : int{ _ = key }) left right query
    : unit{
      member query (insert new_key (Node (left, key, right)))
      = if query = new_key
        then true
        else member query (Node (left, key, right))
  }
  =
  insert_def new_key (Node (left, key, right));
  member_node query left key right;
  ()

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
  insert_def new_key (Node (left, key, right));
  member_node query left key right;
  member_node query (insert new_key left) key right;
  ()

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
  insert_def new_key (Node (left, key, right));
  member_node query left key right;
  member_node query left key (insert new_key right);
  ()

let rec member_insert new_key tree query
    : unit{
      member query (insert new_key tree)
      = if query = new_key then true else member query tree
    }
  =
  match tree with
  | Empty -> member_insert_empty new_key query
  | Node (left, key, right) ->
    let choice = direction new_key key in
    direction_def new_key key;
    match choice with
    | Same -> member_insert_same key new_key left right query
    | Left ->
      let induction = member_insert new_key left query in
      member_insert_left key new_key left right query induction
    | Right ->
      let induction = member_insert new_key right query in
      member_insert_right key new_key left right query induction

let empty_law ~(query : int)
    : unit{ member query empty = false } =
  member_def query empty;
  ()

let insert_law ~(inserted : int)
    ~(tree : t @ logical) ~(query : int)
    : unit{
      member query (insert inserted tree)
      = ((query = inserted) || member query tree)
    }
  =
  member_insert inserted tree query;
  ()

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
  | Empty ->
    member_def query Empty;
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
    | Empty ->
      agrees_def t1 t2 Empty;
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
