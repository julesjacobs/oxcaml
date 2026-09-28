(* Refined closures in an interface.  The refinements are trivial ("true");
   what matters is that a value entering a refined type must be total,
   stateless and portable (DESIGN.md 2.1, MC). *)

(* The returned closure is pure: MC constrains its mode to total. *)
val pure : unit -> {f : int -> int | true}

(* The returned closure updates a counter: rejected by MC. *)
val counter : unit -> {f : int -> int | true}
