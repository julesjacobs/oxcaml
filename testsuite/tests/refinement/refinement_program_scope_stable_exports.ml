external int_equal : int -> int -> bool @@ total = "%equal"

let external_witness x : bool{ _ = int_equal x 0 } = int_equal x 0

module Included = struct
  let anchor = 11
end

include Included

let included_witness : int{ _ = anchor } = anchor
