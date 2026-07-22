module M : Set_intf.SET = struct
type t =
  | Nil
  | Cons of int * t

external int_equal : int -> int -> bool @@ total = "%equal"

let empty = (Nil : t{ _ = Nil })

let[@vox.def] rec member (query : int) (set : t @ local logical)
    : bool{ _ = true || _ = false } =
  match set with
  | Nil -> false
  | Cons (key, rest) ->
    if int_equal query key then true else member query rest

let[@vox.def] rec unique (set : t @ logical)
    : bool{ _ = true || _ = false } =
  match set with
  | Nil -> true
  | Cons (key, rest) ->
    if member key rest then false else unique rest

let[@vox.def] insert (inserted : int) (set : t @ logical) =
  if member inserted set then set else Cons (inserted, set)

let finish_unique_insert (inserted : int) (set : t @ logical)
    (_proof : unit{ unique (insert inserted set) = true })
    : unit{ unique (insert inserted set) = true } =
  ()

let insert_preserves_unique (inserted : int) (set : t @ logical)
    (_unique : unit{ unique set = true })
    : unit{ unique (insert inserted set) = true } =
  let present = member inserted set in
  match present with
  | true ->
    let _insert = insert_def inserted set in
    finish_unique_insert inserted set _unique
  | false ->
    let _insert = insert_def inserted set in
    let _definition = unique_def (Cons (inserted, set)) in
    finish_unique_insert inserted set ()

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
  | Nil -> true
  | Cons (key, rest) ->
    let first_member = member key t1 in
    let second_member = member key t2 in
    if first_member
    then
      if second_member then agrees t1 t2 rest else false
    else if second_member
    then false
    else agrees t1 t2 rest

let[@vox.def] equal (_t1 : t @ logical) (_t2 : t @ logical) =
  false

let empty_law ~(query : int)
    : unit{ member query empty = false } =
  let _definition = member_def query Nil in
  ()

let finish_member_insert (inserted : int) (tree : t @ logical)
    (query : int)
    (_proof : unit{
       member query (insert inserted tree)
       = ((query = inserted) || member query tree)
     })
    : unit{
      member query (insert inserted tree)
      = ((query = inserted) || member query tree)
    } =
  ()

let insert_law ~(inserted : int) ~(tree : t @ logical) ~(query : int)
    : unit{
      member query (insert inserted tree)
      = ((query = inserted) || member query tree)
    } =
  let present = member inserted tree in
  match present with
  | true ->
    let _insert = insert_def inserted tree in
    if int_equal query inserted
    then finish_member_insert inserted tree query ()
    else finish_member_insert inserted tree query ()
  | false ->
    let _insert = insert_def inserted tree in
    let _new_member = member_def query (Cons (inserted, tree)) in
    if int_equal query inserted
    then finish_member_insert inserted tree query ()
    else finish_member_insert inserted tree query ()

let agrees_cons ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(key : int) ~(rest : t @ logical)
    ~proof:(_proof : unit{ agrees t1 t2 (Cons (key, rest)) = true })
    : unit{
      member key t1 = member key t2
      && agrees t1 t2 rest = true
    } =
  let _definition = agrees_def t1 t2 (Cons (key, rest)) in
  let first_member = member key t1 in
  let second_member = member key t2 in
  if first_member
  then if second_member then () else ()
  else if second_member then () else ()

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
  | Nil ->
    let _member = member_def query Nil in
    ()
  | Cons (key, rest) ->
    let facts = agrees_cons ~t1 ~t2 ~key ~rest ~proof:agreement in
    let _member = member_def query (Cons (key, rest)) in
    if int_equal query key
    then finish_equal_member ~t1 ~t2 ~query ~proof:facts
    else
      finish_equal_member ~t1 ~t2 ~query
        ~proof:(agrees_member ~t1 ~t2 ~nodes:rest ~query
                  ~agreement:facts ~present:())

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

let equal_forward_law ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(equal_trees : unit{ equal t1 t2 = true }) ~(query : int)
    : unit{ member query t1 = member query t2 } =
  let _equality = equal_trees in
  prove_equal_member ~t1 ~t2 ~query

let equal_backward_law ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(pointwise : query:int ->
                   unit{ member query t1 = member query t2 })
    : unit{ equal t1 t2 = false } =
  let rec prove nodes : unit{ agrees t1 t2 nodes = true } =
    match nodes with
    | Nil ->
      let _definition = agrees_def t1 t2 Nil in
      ()
    | Cons (key, rest) ->
      pointwise ~query:key;
      let _rest = prove rest in
      let _definition = agrees_def t1 t2 (Cons (key, rest)) in
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
end
