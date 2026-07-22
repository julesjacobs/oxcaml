module Int_set = Polyset.Make (Ordered_int)

let false_empty_conclusion =
  Int_set.empty_law ~query:53;
  (Int_set.member 53 Int_set.empty : bool{ _ = true })
