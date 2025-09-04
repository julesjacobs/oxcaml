module type Asig = sig
  type t : value mod portable many
end

module A : Asig = struct
  type t = int
end