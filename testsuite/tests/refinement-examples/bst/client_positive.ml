let empty_is_absent_for_any_key =
  let () = Bst.empty_law ~key:37 in
  (Bst.member 37 Bst.empty : bool{ _ = false })

let inserted_key_is_present_in_any_tree =
  let () =
    Bst.insert_law ~key:37
      ~tree:(Bst.insert 11 (Bst.insert 5 Bst.empty))
  in
  (Bst.member 37
     (Bst.insert 37 (Bst.insert 11 (Bst.insert 5 Bst.empty)))
    : bool{ _ = true })

let characterization_at_a_new_key_and_tree =
  let () =
    Bst.member_insert_law ~inserted:37
      ~tree:(Bst.insert 11 (Bst.insert 5 Bst.empty)) ~query:37
  in
  (Bst.member 37
     (Bst.insert 37 (Bst.insert 11 (Bst.insert 5 Bst.empty)))
    : bool{ _ = true })
