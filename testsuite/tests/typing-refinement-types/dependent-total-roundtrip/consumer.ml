let apply (x : Producer.t @ total) =
  let _result = Producer.keep x in
  ()

let apply_immutable (x : Producer.t @ total immutable) =
  let _result = Producer.keep_immutable x in
  ()
