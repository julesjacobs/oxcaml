module type WRONG_EXPECTATION = sig
  type t

  val empty : t @@ total
  val view : t @ local logical -> int list @@ total
  val view_law :
    seed:int -> unit{ (view empty = []) && seed = 0 } @@ total
end

module Candidate : WRONG_EXPECTATION = struct
  type t = int iarray

  external make_empty : unit -> t @@ total = "vox_sorted_iarray_empty"
  external view : t @ local logical -> int list @@ total
    = "vox_sorted_iarray_view"

  let empty = make_empty ()

  external view_law :
    seed:int -> unit{ view empty = [] } @@ total
    = "%ignore"
end
