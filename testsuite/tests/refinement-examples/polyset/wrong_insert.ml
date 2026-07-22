module Int_set = Polyset.Make (Ordered_int)

let false_insert_conclusion =
  Int_set.empty_law ~query:59;
  Int_set.insert_law ~inserted:53 ~set:Int_set.empty ~query:59;
  (Int_set.member 59 (Int_set.insert 53 Int_set.empty)
    : bool{ _ = true })
