(* TEST
 flags = "-vox-backend z3";
 expect;
*)

(* Recursive re-entry at a different type instantiation is rejected before
   monomorphization can keep growing. *)
module Nonregular = struct
  type ('a, 'b) t = C of ('a list, 'a option) t

  let reflexive (x : (int, bool) t @ logical) : unit{ x = x } = ()
end

[%%expect {|
Line 4, characters 64-66:
4 |   let reflexive (x : (int, bool) t @ logical) : unit{ x = x } = ()
                                                                    ^^
Error: Refinement verification failed (solver-error)
|}]

(* A datatype declaration must have a finite constructor value. *)
module Nonproductive = struct
  type t = Loop of t

  let reflexive (x : t @ logical) : unit{ x = x } = ()
end

[%%expect {|
Line 4, characters 52-54:
4 |   let reflexive (x : t @ logical) : unit{ x = x } = ()
                                                        ^^
Error: Refinement verification failed (solver-error)
|}]
