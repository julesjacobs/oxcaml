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

(* Search membership: one comparison per level, following a single spine.
   Away from ordered trees this is not occurrence in the tree, which is
   why the laws below are stated for trees satisfying [invariant]. *)
let[@vox.def] rec member (query : int) (tree : t @ logical) =
  match tree with
  | Empty -> false
  | Node (left, key, right) ->
    if int_equal query key
    then true
    else if int_less query key
    then member query left
    else member query right

(* Occurrence anywhere in the tree.  Used only to say what the single
   spine is worth: on an ordered tree the two agree. *)
let[@vox.def] rec occurs (query : int) (tree : t @ logical) =
  match tree with
  | Empty -> false
  | Node (left, key, right) ->
    int_equal query key || occurs query left || occurs query right

let[@vox.def] rec below (tree : t @ logical) (bound : int) =
  match tree with
  | Empty -> true
  | Node (left, key, right) ->
    int_less key bound && below left bound && below right bound

let[@vox.def] rec above (tree : t @ logical) (bound : int) =
  match tree with
  | Empty -> true
  | Node (left, key, right) ->
    int_less bound key && above left bound && above right bound

let[@vox.def] rec ordered (tree : t @ logical) =
  match tree with
  | Empty -> true
  | Node (left, key, right) ->
    ordered left && ordered right && below left key && above right key

let[@vox.def] invariant (tree : t @ logical) = ordered tree

let rec below_absent (bound : int) (query : int{ bound < _ })
    (tree : t @ logical)
    (_bounded : unit{ below tree bound = true })
    : unit{ occurs query tree = false } =
  match tree with
  | Empty ->
    occurs_def query Empty;
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
  | Empty ->
    occurs_def query Empty;
    ()
  | Node (left, key, right) ->
    above_def (Node (left, key, right)) bound;
    occurs_def query (Node (left, key, right));
    above_absent bound query left ();
    above_absent bound query right ();
    ()

(* The single spine finds exactly the keys that occur, on ordered trees. *)
let rec member_occurs (query : int) (tree : t @ logical)
    (_ordered : unit{ ordered tree = true })
    : unit{ member query tree = occurs query tree } =
  match tree with
  | Empty ->
    member_def query Empty;
    occurs_def query Empty;
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

let rec insert_below (bound : int) (new_key : int{ _ < bound })
    (tree : t @ logical)
    (_bounded : unit{ below tree bound = true })
    : unit{ below (insert new_key tree) bound = true } =
  match tree with
  | Empty ->
    insert_def new_key Empty;
    below_def (Node (Empty, new_key, Empty)) bound;
    below_def Empty bound;
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
      below_def (Node (insert new_key left, key, right)) bound;
      ()
    | Right ->
      insert_below bound new_key right ();
      below_def (Node (left, key, insert new_key right)) bound;
      ()

let rec insert_above (bound : int) (new_key : int{ bound < _ })
    (tree : t @ logical)
    (_bounded : unit{ above tree bound = true })
    : unit{ above (insert new_key tree) bound = true } =
  match tree with
  | Empty ->
    insert_def new_key Empty;
    above_def (Node (Empty, new_key, Empty)) bound;
    above_def Empty bound;
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
      above_def (Node (insert new_key left, key, right)) bound;
      ()
    | Right ->
      insert_above bound new_key right ();
      above_def (Node (left, key, insert new_key right)) bound;
      ()

let rec insert_ordered (new_key : int) (tree : t @ logical)
    (_ordered : unit{ ordered tree = true })
    : unit{ ordered (insert new_key tree) = true } =
  match tree with
  | Empty ->
    insert_def new_key Empty;
    ordered_def (Node (Empty, new_key, Empty));
    ordered_def Empty;
    below_def Empty new_key;
    above_def Empty new_key;
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
      ()
    | Right ->
      insert_ordered new_key right ();
      insert_above key new_key right ();
      ordered_def (Node (left, key, insert new_key right));
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

let insert_law ~(inserted : int)
    ~(tree : t @ logical) ~(query : int)
    ~(well_formed : unit{ invariant tree = true })
    : unit{
      member query (insert inserted tree)
      = ((query = inserted) || member query tree)
    }
  =
  member_insert inserted tree query;
  ()

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

(* ------------------------------------------------------------------ *)
(* The [COUNTED_SET] laws.                                             *)
(* ------------------------------------------------------------------ *)

let[@vox.def] rec size (tree : t @ logical) : Bigint.t =
  match tree with
  | Empty -> Bigint.zero
  | Node (left, _, right) ->
    Bigint.add Bigint.one (Bigint.add (size left) (size right))

(* [insert] adds exactly one node, and none when the key is already
   found.  This is false for an insert that pushes a second copy of a key
   it already holds, however that insert's ordering predicate is
   written. *)
let rec size_insert_step (new_key : int) (tree : t @ logical)
    : unit{
      size (insert new_key tree)
      = (if member new_key tree
         then size tree
         else Bigint.add (size tree) Bigint.one)
    } =
  match tree with
  | Empty ->
    insert_def new_key Empty;
    member_def new_key Empty;
    size_def Empty;
    size_def (Node (Empty, new_key, Empty));
    ()
  | Node (left, key, right) ->
    insert_def new_key (Node (left, key, right));
    member_def new_key (Node (left, key, right));
    size_def (Node (left, key, right));
    let choice = direction new_key key in
    direction_def new_key key;
    (match choice with
     | Same -> ()
     | Left ->
       size_insert_step new_key left;
       size_def (Node (insert new_key left, key, right));
       ()
     | Right ->
       size_insert_step new_key right;
       size_def (Node (left, key, insert new_key right));
       ())

let size_empty : unit{ size empty = Bigint.zero } =
  size_def Empty;
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
