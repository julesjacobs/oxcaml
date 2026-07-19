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

let empty_law ~(key : int @ logical)
    : unit{ member key empty = false } =
  let _ = member_def key empty in
  ()

let insert_law ~(key : int @ logical) ~(tree : t @ logical)
    : unit{ member key (insert key tree) = true } =
  let _ = insert_def key tree in
  let _ = member_def key (One key) in
  ()

let member_insert_law ~(inserted : int @ logical)
    ~(tree : t @ logical) ~(query : int @ logical)
    : unit{
      member query (insert inserted tree) = (inserted = query)
    }
  =
  let _ = insert_def inserted tree in
  let _ = member_def query (One inserted) in
  if int_equal query inserted then () else ()
