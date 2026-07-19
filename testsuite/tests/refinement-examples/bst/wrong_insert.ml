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

let empty_has_no_zero (_ : unit @ logical)
    : unit{ member 0 empty = false } =
  let _ = member_def 0 empty in ()

let empty_has_no_one (_ : unit @ logical)
    : unit{ member 1 empty = false } =
  let _ = member_def 1 empty in ()

let insert_zero_has_zero (_ : unit @ logical)
    : unit{ member 0 (insert 0 empty) = true } =
  let _ = insert_def 0 empty in
  let _ = member_def 0 (One 0) in
  ()

let insert_zero_has_no_one (_ : unit @ logical)
    : unit{ member 1 (insert 0 empty) = false } =
  let _ = insert_def 0 empty in
  let _ = member_def 1 (One 0) in
  ()

let insert_one_preserves_zero (_ : unit @ logical)
    : unit{ member 0 (insert 1 (insert 0 empty)) = false } =
  let _ = insert_def 0 empty in
  let _ = insert_def 1 (One 0) in
  let _ = member_def 0 (One 1) in
  ()
