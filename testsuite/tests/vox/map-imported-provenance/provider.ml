module Order = struct
  type t = int
  external compare : int -> int -> int @@ total = "%compare"
end

module M = Map.MakeTotal (Order)
module Alias = M
