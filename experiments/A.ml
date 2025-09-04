module type Asig = sig
  type t : immediate with int ref
end

module A : Asig = struct
  type t = int ref
end