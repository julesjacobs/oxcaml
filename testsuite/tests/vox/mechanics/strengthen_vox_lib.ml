module M = struct
  type t = int
  let mk : (n : int) -> t = fun n -> n
  let get : (x : t) -> int{ _ = 0 -> true } = fun x -> x
end
