module A = struct
  let witness = 1
end

module B = struct
  let witness = 2
end

module One = Refined_functor.Make (A)
module Two = Refined_functor.Make (B)

module Check_one : sig
  val result : int{ _ = A.witness }
end = One

module Check_two : sig
  val result : int{ _ = B.witness }
end = Two
