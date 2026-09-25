(* TEST
 flags += "-extension mode_polymorphism_alpha";
 native;
*)

module M = struct
  let id x = x
  let compose f g x = f (g x)
end

module N : module type of M = struct
  let id x = x
  let compose f g x = f (g x)
end

module Reordered = struct
  let compose f g x = f (g x)
  let id x = x
end

module P : module type of M = Reordered

let () = assert (N.compose N.id N.id 42 = P.compose P.id P.id 42)
