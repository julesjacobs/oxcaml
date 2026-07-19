type 'a nested =
  | Empty
  | More of 'a * ('a list) nested

let witness =
  (More (1, Empty) : int nested{ _ = More (1, Empty) })

