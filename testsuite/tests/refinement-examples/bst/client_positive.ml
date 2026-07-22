let empty_is_absent_for_any_key =
  Bst.empty_law ~key:37;
  (Bst.member 37 Bst.empty : bool{ _ = false })

let inserted_key_is_present_in_any_tree =
  Bst.insert_law ~inserted:37
    ~tree:(Bst.insert 11 (Bst.insert 5 Bst.empty)) ~query:37;
  (Bst.member 37
     (Bst.insert 37 (Bst.insert 11 (Bst.insert 5 Bst.empty)))
    : bool{ _ = true })

let characterization_at_a_new_key_and_tree =
  Bst.insert_law ~inserted:37
    ~tree:(Bst.insert 11 (Bst.insert 5 Bst.empty)) ~query:11;
  Bst.insert_law ~inserted:11 ~tree:(Bst.insert 5 Bst.empty)
    ~query:11;
  (Bst.member 11
     (Bst.insert 37 (Bst.insert 11 (Bst.insert 5 Bst.empty)))
    : bool{ _ = true })

let equality_is_extensional =
  let first = Bst.insert 5 (Bst.insert 11 Bst.empty) in
  let second = Bst.insert 11 (Bst.insert 5 Bst.empty) in
  let equality = Bst.equal first second in
  if equality
  then begin
    Bst.equal_implies_member ~t1:first ~t2:second ~query:37;
    Bst.member 37 first
  end
  else false
