module Int_set = Polyset.Make (Ordered_int)

let false_left_to_right_conclusion
    ~(left : Int_set.t @ logical)
    ~(right : Int_set.t{ Int_set.equal left _ = true } @ logical)
    ~(present : unit{ Int_set.member 61 left = true }) =
  let () = Int_set.equal_left_to_right ~left ~right ~query:61 in
  (Int_set.member 61 right : bool{ _ = false })
