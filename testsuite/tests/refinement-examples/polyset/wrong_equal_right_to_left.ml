module Int_set = Polyset.Make (Ordered_int)

let false_right_to_left_conclusion
    ~(left : Int_set.t @ logical)
    ~(right : Int_set.t{ Int_set.equal left _ = true } @ logical)
    ~(present : unit{ Int_set.member 67 right = true }) =
  let () = Int_set.equal_right_to_left ~left ~right ~query:67 in
  (Int_set.member 67 left : bool{ _ = false })
