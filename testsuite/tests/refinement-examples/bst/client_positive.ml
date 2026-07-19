let empty_is_absent =
  let () = Bst.empty_has_no_zero () in
  (Bst.member 0 Bst.empty : bool{ _ = false })

let inserted_key_is_present =
  let () = Bst.insert_zero_has_zero () in
  (Bst.member 0 (Bst.insert 0 Bst.empty) : bool{ _ = true })

let distinct_key_is_absent =
  let () = Bst.insert_zero_has_no_one () in
  (Bst.member 1 (Bst.insert 0 Bst.empty) : bool{ _ = false })

let inserted_key_survives_another_insert =
  let () = Bst.insert_one_preserves_zero () in
  (Bst.member 0 (Bst.insert 1 (Bst.insert 0 Bst.empty))
    : bool{ _ = true })
