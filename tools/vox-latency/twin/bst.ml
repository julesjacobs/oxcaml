(* Refinement-stripped twin of set_group/bst.ml.

   Produced from the original by exactly four mechanical edits, and nothing
   else.  Line order, function order, parameter order and every expression in
   every body are those of the original.

   1. Refinement types are reduced to their carriers:
        [t{ _ <> Empty }]     becomes [t]
        [int{ _ = key }]      becomes [int]
        [unit{ member ... }]  becomes [unit]
   2. Mode annotations are removed: [@ logical], [@@ total].
   3. The [[@vox.def]] attribute is removed from the six definitional
      bindings.  The attribute does two things: it forces the binding to
      [@ total], and it synthesises a companion lemma
        [let f_def p1 ... pn = (() : unit{ f p1 ... pn = <body of f> })]
      whose parameter list is [f]'s own and whose body is [()].  Dropping the
      attribute drops both, so the twin declares the six companions by hand,
      with [f]'s parameter list and the body [()].  This is what the expander
      would have produced once its refinement is stripped, so the twin keeps
      the six extra closures and the twenty-one calls to them that the
      original has.  Removing them instead would have deleted real code from
      the twin and charged its compilation to the refinement machinery.
   4. The single-value definitions are otherwise untouched.

   See tools/vox-latency/vox_attribute_time.sh for the Lambda-level check that
   these are the only differences. *)

type t =
  | Empty
  | Node of t * int * t

external int_equal : int -> int -> bool = "%equal"
external int_less : int -> int -> bool = "%lessthan"

type direction =
  | Same
  | Left
  | Right

type membership_side =
  | First
  | Second
  | Neither

let direction new_key key =
  if int_equal new_key key
  then Same
  else if int_less new_key key
  then Left
  else Right

let direction_def (new_key : int) (key : int) = ()

let membership_side first_member second_member =
  if first_member
  then First
  else if second_member then Second else Neither

let membership_side_def (first_member : bool) (second_member : bool) = ()

let empty = Empty

let rec member (query : int) (tree : t) =
  match tree with
  | Empty -> false
  | Node (left, key, right) ->
    if int_equal query key
    then true
    else if int_less query key
    then member query left
    else member query right

let member_def (query : int) (tree : t) = ()

let rec agrees (t1 : t) (t2 : t)
    (nodes : t) =
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

let agrees_def (t1 : t) (t2 : t) (nodes : t) = ()

let equal (t1 : t) (t2 : t) =
  if agrees t1 t2 t1 then agrees t1 t2 t2 else false

let equal_def (t1 : t) (t2 : t) = ()

let rec insert (new_key : int) (tree : t)
    : t
  =
  match tree with
  | Empty -> Node (Empty, new_key, Empty)
  | Node (left, key, right) ->
    if int_equal new_key key
    then tree
    else if int_less new_key key
    then Node (insert new_key left, key, right)
    else Node (left, key, insert new_key right)

let insert_def (new_key : int) (tree : t) = ()

let member_node query left key right
    : unit
  =
  member_def query (Node (left, key, right));
  ()

let member_insert_empty new_key query
    : unit
  =
  insert_def new_key Empty;
  member_def query Empty;
  member_node query Empty new_key Empty;
  ()

let member_insert_same key (new_key : int) left right query
    : unit
  =
  insert_def new_key (Node (left, key, right));
  member_node query left key right;
  ()

let member_insert_left key (new_key : int) left right query
    (_induction : unit)
    : unit
  =
  insert_def new_key (Node (left, key, right));
  member_node query left key right;
  member_node query (insert new_key left) key right;
  ()

let member_insert_right key
    (new_key : int) left right query
    (_induction : unit)
    : unit
  =
  insert_def new_key (Node (left, key, right));
  member_node query left key right;
  member_node query left key (insert new_key right);
  ()

let rec member_insert new_key tree query
    : unit
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
    : unit =
  member_def query empty;
  ()

let insert_law ~(inserted : int)
    ~(tree : t) ~(query : int)
    : unit
  =
  member_insert inserted tree query;
  ()

let agrees_node ~(t1 : t) ~(t2 : t)
    ~(left : t) ~(key : int) ~(right : t)
    ~proof:(_proof : unit)
    : unit =
  agrees_def t1 t2 (Node (left, key, right));
  ()

let rec agrees_member ~(t1 : t) ~(t2 : t)
    ~(nodes : t) ~(query : int)
    ~(agreement : unit)
    ~(present : unit)
    : unit =
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

let prove_equal_member ~(t1 : t)
    ~(t2 : t)
    ~(query : int)
    : unit =
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

let equal_forward_law ~(t1 : t) ~(t2 : t)
    ~(equal_trees : unit) ~(query : int)
    : unit =
  prove_equal_member ~t1 ~t2 ~query

let equal_backward_law ~(t1 : t) ~(t2 : t)
    ~(pointwise : query:int -> unit)
    : unit =
  let rec prove nodes : unit =
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
