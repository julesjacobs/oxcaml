let equal ~(x : int) ~(y : int{ _ = x }) = ()
let partial = equal ~y:1

let returning ~(x : int) ~(y : int{ _ = x }) : int{ _ = x } =
  let _ = y in
  x

let returning_partial = returning ~y:1
