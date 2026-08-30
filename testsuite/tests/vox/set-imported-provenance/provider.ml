module Order = struct
  type t = int
  external compare : int -> int -> int @@ total = "%compare"
end

module S = Set.MakeTotal (Order)
module Alias = S
