external greater : int -> int -> bool @@ total = "%greaterthan"
external less_equal : int -> int -> bool @@ total = "%lessequal"

let (positive @ total) x = greater x 0
let (nonpositive @ total) x = less_equal x 0
let choose_positive = ref false

module Make (_ : sig end) = struct
  let (holds @ total) =
    if !choose_positive then positive else nonpositive

  type t = { x : int | holds x }
end

module Argument = struct end
module A = Make (Argument)
let () = choose_positive := true
module B = Make (Argument)
module Alias = A
