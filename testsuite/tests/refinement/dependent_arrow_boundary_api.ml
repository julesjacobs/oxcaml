let keep (x : int) (_ignored : int) : int{ _ = x } = x

module type Empty = sig end

module type Result = sig
  val keep : (x : int) -> int -> int{ _ = x }
end

module Make () = struct
  let keep (x : int) (_ignored : int) : int{ _ = x } = x
end

module Applicative (X : Empty) = Make ()
module Anonymous (_ : Empty) = Make ()
module Generative () = Make ()
