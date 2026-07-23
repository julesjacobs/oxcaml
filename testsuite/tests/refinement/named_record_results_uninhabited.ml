module Uninhabited (Key : sig type t : immutable_data end) = struct
  type box =
    { payload : Key.t;
      tag : int
    }

  external fabricate : int -> Key.t @@ total = "%identity"

  let make (tag : int) : box{ _.tag = tag } =
    { payload = fabricate tag;
      tag
    }
end
