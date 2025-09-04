module type Asig = sig
  type t : value
end

module A : Asig = struct
  type t = int
end