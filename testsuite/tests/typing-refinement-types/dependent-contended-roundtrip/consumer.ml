let apply (x : Producer.t @ contended) =
  let _result = Producer.keep x in
  ()
