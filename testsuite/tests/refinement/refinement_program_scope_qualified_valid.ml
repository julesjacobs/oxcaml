module Global = struct
  let x = 1
end

let global : int{ _ = Global.x } = Global.x

let same_scope () =
  let module M : sig val x : int end = struct
    let x = 2
  end in
  let value : int{ _ = M.x } = M.x in
  ignore value

let outer = 3

module Collision : sig
  val outer : int
  val value : int{ _ = outer }
end = struct
  let outer = 4
  let value : int{ _ = outer } = outer
end

let collision : int{ _ = Collision.outer } = Collision.value
