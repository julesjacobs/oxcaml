module Int_set = Polyset.Make (Ordered_int)

let empty_is_absent_at_a_fresh_key =
  Int_set.empty_law ~query:41;
  (Int_set.member 41 Int_set.empty : bool{ _ = false })

let insert_is_characterized_at_fresh_keys =
  let set = Int_set.insert 17 Int_set.empty in
  Int_set.insert_law ~inserted:41 ~set ~query:41;
  (Int_set.member 41 (Int_set.insert 41 set) : bool{ _ = true })

let equal_carries_membership_left_to_right
    ~(left : Int_set.t @ logical)
    ~(right : Int_set.t{ Int_set.equal left _ = true } @ logical)
    ~(present : unit{ Int_set.member 43 left = true }) =
  Int_set.equal_left_to_right ~left ~right ~query:43;
  (Int_set.member 43 right : bool{ _ = true })

let equal_carries_membership_right_to_left
    ~(left : Int_set.t @ logical)
    ~(right : Int_set.t{ Int_set.equal left _ = true } @ logical)
    ~(present : unit{ Int_set.member 47 right = true }) =
  Int_set.equal_right_to_left ~left ~right ~query:47;
  (Int_set.member 47 left : bool{ _ = true })

let pointwise_for_reordered_insertions ~(query : int)
    : unit{
      Int_set.member query
        (Int_set.insert 17 (Int_set.insert 41 Int_set.empty))
      = Int_set.member query
          (Int_set.insert 41 (Int_set.insert 17 Int_set.empty))
    } =
  Int_set.insert_law ~inserted:17
    ~set:(Int_set.insert 41 Int_set.empty) ~query;
  Int_set.insert_law ~inserted:41 ~set:Int_set.empty ~query;
  Int_set.insert_law ~inserted:41
    ~set:(Int_set.insert 17 Int_set.empty) ~query;
  Int_set.insert_law ~inserted:17 ~set:Int_set.empty ~query;
  ()

let reordered_insertions_equal ()
    : unit{
      Int_set.equal
        (Int_set.insert 17 (Int_set.insert 41 Int_set.empty))
        (Int_set.insert 41 (Int_set.insert 17 Int_set.empty))
      = true
    } =
    Int_set.equal_backward_law
      ~left:(Int_set.insert 17 (Int_set.insert 41 Int_set.empty))
      ~right:(Int_set.insert 41 (Int_set.insert 17 Int_set.empty))
      ~pointwise:pointwise_for_reordered_insertions

let reordered_insertions_compare : bool{ _ = true } =
  reordered_insertions_equal ();
  Int_set.equal
    (Int_set.insert 17 (Int_set.insert 41 Int_set.empty))
    (Int_set.insert 41 (Int_set.insert 17 Int_set.empty))
