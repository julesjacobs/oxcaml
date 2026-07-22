external int_equal : int -> int -> bool @@ total = "%equal"

let (is_zero @ total) ~x : bool{ _ = int_equal x 0 } = int_equal x 0
