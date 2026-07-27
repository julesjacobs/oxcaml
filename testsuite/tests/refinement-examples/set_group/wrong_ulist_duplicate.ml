module M : Bal_intf.COUNTED_SET = struct
type t =
  | Nil
  | Cons of int * t

external int_equal : int -> int -> bool @@ total = "%equal"

let empty = Nil

let[@vox.def] rec member (query : int) (set : t @ logical) =
  match set with
  | Nil -> false
  | Cons (key, rest) ->
    if int_equal query key then true else member query rest

let[@vox.def] insert (inserted : int) (set : t @ logical) =
  Cons (inserted, set)

(* No key is repeated: the list really is a set, and every key occupies
   exactly one cell. *)
let[@vox.def] rec unique (set : t @ logical) =
  match set with
  | Nil -> true
  | Cons (key, rest) -> if member key rest then false else unique rest

let[@vox.def] invariant (set : t @ logical) = unique set

let empty_invariant : unit{ invariant empty = true } =
  let _invariant = invariant_def empty in
  let _definition = unique_def Nil in
  ()

let insert_invariant ~(inserted : int) ~(tree : t @ logical)
    ~(well_formed : unit{ invariant tree = true })
    : unit{ invariant (insert inserted tree) = true } =
  let _tree = invariant_def tree in
  let _result = invariant_def (insert inserted tree) in
  let _insert = insert_def inserted tree in
  let present = member inserted tree in
  match present with
  | true -> ()
  | false ->
    let _definition = unique_def (Cons (inserted, tree)) in
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

let[@vox.def] equal (t1 : t @ logical) (t2 : t @ logical) =
  if agrees t1 t2 t1 then agrees t1 t2 t2 else false

let empty_law ~(query : int)
    : unit{ member query empty = false } =
  let _definition = member_def query Nil in
  ()

let insert_law ~(inserted : int) ~(tree : t @ logical) ~(query : int)
    ~(well_formed : unit{ invariant tree = true })
    : unit{
      member query (insert inserted tree)
      = ((query = inserted) || member query tree)
    } =
  let _insert = insert_def inserted tree in
  let _new_member = member_def query (Cons (inserted, tree)) in
  if int_equal query inserted then () else ()

let agrees_cons ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(key : int) ~(rest : t @ logical)
    ~proof:(_proof : unit{ agrees t1 t2 (Cons (key, rest)) = true })
    : unit{
      member key t1 = member key t2
      && agrees t1 t2 rest = true
  } =
  let _definition = agrees_def t1 t2 (Cons (key, rest)) in
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
    then facts
    else
      agrees_member ~t1 ~t2 ~nodes:rest ~query
        ~agreement:facts ~present:()

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
    | Nil ->
      let _definition = agrees_def t1 t2 Nil in
      ()
    | Cons (key, rest) ->
      let _same_membership = pointwise ~query:key in
      let _rest = prove rest in
      let _definition = agrees_def t1 t2 (Cons (key, rest)) in
      ()
  in
  let _first = prove t1 in
  let _second = prove t2 in
  let _definition = equal_def t1 t2 in
  ()

(* ------------------------------------------------------------------ *)
(* The [COUNTED_SET] laws.                                             *)
(* ------------------------------------------------------------------ *)

let[@vox.def] rec size (set : t @ logical) : Bigint.t =
  match set with
  | Nil -> Bigint.zero
  | Cons (_, rest) -> Bigint.add Bigint.one (size rest)

let size_empty : unit{ size empty = Bigint.zero } =
  size_def Nil;
  ()

let size_insert ~(inserted : int) ~(tree : t @ logical)
    ~(well_formed : unit{ invariant tree = true })
    : unit{
      size (insert inserted tree)
      = (if member inserted tree
         then size tree
         else Bigint.add (size tree) Bigint.one)
    } =
  insert_def inserted tree;
  let present = member inserted tree in
  match present with
  | true -> ()
  | false -> size_def (Cons (inserted, tree)); ()
end
