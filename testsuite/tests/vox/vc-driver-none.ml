(* TEST
 flags = "-vox-backend none";
 expect;
*)

(* The default driver policy: -vox-backend none short-circuits before the
   walk (design-docs/vc-generation.md, "Where the pass sits").  The control
   is the unrepresentable shape: under any running backend it is a located
   tier-2 error raised by the WALK itself (vc-z3.ml's `unrepresentable`),
   so its silent compilation here pins that the pass does not run at all —
   not merely that nothing is discharged.  Under this default, refined
   types are recorded, unverified claims. *)

let unrepresentable_control : (int -> int){ true } = fun x -> x;;
[%%expect{|
val unrepresentable_control : (int -> int){ true } = <fun>
|}]
