let project x = x

module Let_only = struct
  let ( let* ) x f = f x
end

module And_only = struct
  let ( let* ) x f = f x
  let ( and* ) x y = x, y
end

module Ordinary = struct
  let ( let* ) x f = f x
  let ( and* ) x y = x, y
end
