module Global = struct
  let x = 7
end

let value : int{ _ = Global.x } = Global.x

let outer = 11

module Nested : sig
  val inherited : int{ _ = outer }
  val base : int
  val sibling : int{ _ = base }
end = struct
  let inherited : int{ _ = outer } = outer
  let base = 12
  let sibling : int{ _ = base } = base
end
