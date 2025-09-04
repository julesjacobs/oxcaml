module type Asig = sig
  type t : value with int ref
end

module A : Asig = struct
  type t : any = int ref
end