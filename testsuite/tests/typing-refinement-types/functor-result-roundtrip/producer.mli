module type Predicate = sig
  val holds : int -> bool @@ total
end

module type Result = sig
  val holds : int -> bool @@ total
  type t = { x : int | holds x }
  val accept : { x : int | holds x } -> unit @@ total
end

module Positive : Predicate
module Make (P : Predicate) : Result
module Concrete : sig
  val holds : int -> bool @@ total
  type t = { x : int | holds x }
  val accept : { x : int | holds x } -> unit @@ total
end
