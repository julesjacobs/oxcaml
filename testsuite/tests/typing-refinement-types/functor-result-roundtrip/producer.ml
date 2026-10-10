module type Predicate = sig
  val holds : int -> bool @@ total
end

module type Result = sig
  val holds : int -> bool @@ total
  type t = { x : int | holds x }
  val accept : { x : int | holds x } -> unit @@ total
end

external greater : int -> int -> bool @@ total = "%greaterthan"

module Positive : Predicate = struct
  let (holds @ total) x = greater x 0
end

module Make (P : Predicate) : Result = struct
  let (holds @ total) = P.holds
  type t = { x : int | holds x }
  external accept : { x : int | holds x } -> unit @@ total = "%ignore"
end

module Concrete = Make (Positive)
