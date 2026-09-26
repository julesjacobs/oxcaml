let explicit (xs : Producer.Concrete.t list) :
    { x : int | Producer.Concrete.holds x } list =
  xs

let accept (x : Producer.Concrete.t) = Producer.Concrete.accept x
