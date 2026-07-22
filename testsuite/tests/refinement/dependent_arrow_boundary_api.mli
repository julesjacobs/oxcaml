val keep : (x : int) -> int -> int{ _ = x }

module type Empty = sig end

module type Result = sig
  val keep : (x : int) -> int -> int{ _ = x }
end

module Applicative (X : Empty) : Result
module Anonymous (_ : Empty) : Result
module Generative () : Result
