module Weak_magic : sig
  val x : int{ _ = 2 }
end = struct
  let x = Obj.magic 3
end
