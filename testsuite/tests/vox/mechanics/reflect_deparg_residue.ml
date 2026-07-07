(* TEST
 flags = "-vox-dry-run";
 expect;
*)

(* Task #53 RESIDUE: an argument that is neither reflectable nor a call
   with an exact result contract still cannot name a dependent binder;
   the reworded diagnostic says what was actually missing and points at
   the [let] workaround. *)

let opaque (n : int) : int = n + 1
let f : (n : int) -> int{ _ = n } = fun n -> n
let g (x : int) : int = f (opaque x)
[%%expect{|
val opaque : int -> int = <fun>
val f : (n : int) -> int{ _ = n } = <fun>
Line 3, characters 26-36:
3 | let g (x : int) : int = f (opaque x)
                              ^^^^^^^^^^
Error: vox: this argument for a dependent parameter cannot be named in the logic: it is neither a reflectable expression (a variable, literal, arithmetic, constructor, field read, or reflected call) nor a call with an exact result contract; bind it with a let first
|}]
