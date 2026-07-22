let anchor = 1
let witness : int{ _ = anchor } = anchor
let consume (value : int{ _ = anchor }) = value

module Inner = struct
  let outer_witness : int{ _ = anchor } = anchor
  let consume_outer (value : int{ _ = anchor }) = value
end

module Left = struct
  let anchor = 10
  let witness : int{ _ = anchor } = anchor
  let consume (value : int{ _ = anchor }) = value
end

module Right = struct
  let anchor = 20
  let witness : int{ _ = anchor } = anchor
  let consume (value : int{ _ = anchor }) = value
end

module Make () = struct
  let anchor = 30
  let witness : int{ _ = anchor } = anchor
  let consume (value : int{ _ = anchor }) = value

  module Nested = struct
    let outer_witness : int{ _ = anchor } = anchor
    let consume_outer (value : int{ _ = anchor }) = value
  end
end
