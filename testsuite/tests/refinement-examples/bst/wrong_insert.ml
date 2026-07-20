type t =
  | Empty
  | One of int

external int_equal : int -> int -> bool @@ total = "%equal"

let empty = Empty

let[@vox.def] member (query : int @ logical) (tree : t @ logical) =
  match tree with
  | Empty -> false
  | One key -> int_equal query key

(* This replacement insert drops every element already present. *)
let[@vox.def] insert (key : int @ logical) (_tree : t @ logical) = One key
let[@vox.def] equal (_left : t @ logical) (_right : t @ logical) = false

let empty_law ~(key : int)
    : unit{ member key empty = false } =
  let _ = member_def key empty in
  ()

let insert_law ~(inserted : int)
    ~(tree : t @ logical) ~(query : int)
    : unit{
      member query (insert inserted tree) = (inserted = query)
    }
  =
  let _ = insert_def inserted tree in
  let _ = member_def query (One inserted) in
  if int_equal query inserted then () else ()

let equal_implies_member ~(t1 : t @ logical) ~(t2 : t @ logical)
    ~(query : int)
    : unit{
      equal t1 t2 = false || member query t1 = member query t2
    } =
  let _ = equal_def t1 t2 in
  ()
