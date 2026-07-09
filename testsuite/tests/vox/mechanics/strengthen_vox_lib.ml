module M = struct
  type t = int
  let mk : (n : int) -> t = fun n -> n
  let get : (x : t) -> int{ _ = x } = fun x -> x
end
